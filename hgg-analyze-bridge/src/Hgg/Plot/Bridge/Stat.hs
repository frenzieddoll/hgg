-- |
-- Module      : Hgg.Plot.Bridge.Stat
-- Description : ggplot 風 stat-in (statLm/statSmooth) の回帰計算を hanalyze に委譲して解決 (Phase 16)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 系統 B (ggplot 風スタット・イン) の解決ロジック (Phase 16)。
--
--   ggplot の @geom_smooth(method="lm")@ / @stat_smooth@ に相当。 ユーザは plot-core の
--   __stat layer__ (@statLm@ / @statSmooth@) を通常 geom と同じく @<>@ で重ねる:
--
--   @
--   df |>> ( layer (scatter "x" "y")
--          <> layer (statLm "x" "y" <> colorStatic "red" <> stroke 2)   -- 装飾も通常 geom と同じ
--          <> title "fit" )
--   @
--
--   stat layer (@MStatLM@/@MStatSmooth@) は plot-core が持つ純タグで、 描画前に本モジュールの
--   'resolveStats' が **回帰計算を hanalyze に委譲**して具体 layer (band + line) に展開する。
--
--   ★依存方向 (最重要): @plot-core@ は analyze 非依存のまま (タグを持つだけ)。 回帰 fit は
--   @plot → analyze@ の逆エッジゆえ本 opt-in 隔離 package (`hgg-analyze-bridge`) のみで行う。
--
--   ★使い方: バインド後に 'saveSVGBoundStats' / 'renderBoundStats' を使えば、 bpResolver で
--   自動的に 'resolveStats' してから描画する (df は 1 回参照)。
--
--   ★委譲の内訳: @statLm@ = §3.6 @parseModel "y ~ x"@ + @fitLMF@ で fit + LM @confidenceBand@ で
--   信頼帯 (設計行列は @designMatrix@、 fitLMF と同一 [1,x] 設計)。 @statSmooth@ = @y ~ bs(x,n)@ の
--   fitLMF (B-spline、 曲線のみ・帯なし)。 stat layer の装飾 (color/stroke/alpha) は展開後の
--   band/line に引き継がれる。
{-# LANGUAGE OverloadedStrings #-}

module Hgg.Plot.Bridge.Stat
  ( resolveStats
  , saveSVGBoundStats
  , renderBoundStats
  ) where

import           Data.List               (sortOn, zip4)
import           Data.Monoid             (First (..), Last (..))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified DataFrame               as DX

import           Hgg.Plot.Backend.SVG (renderBound, saveSVGBound)
import           Hgg.Plot.Frame       (BoundPlot (..))
import           Hgg.Plot.Palette     (ggplotHue)
import           Hgg.Plot.Spec        (ColRef (..), ColorEnc (..), Layer (..),
                                           MarkKind (..), Resolver, ThemeName (..),
                                           VisualSpec (..), alpha, band,
                                           orderedCats,
                                           line, scatter, stroke, resolveNum, resolveTxt,
                                           themeSeriesPalette)
import           Hgg.Plot.Unit        (mm, (*~))
import           Data.Maybe              (fromMaybe)
import           Hanalyze.Model.Core      (FitResult, fittedList)
import           Hanalyze.Model.Formula.Design   (designMatrixF, fitLMF)
import           Hanalyze.Model.Formula.Frame    (modelFrame)
import           Hanalyze.Model.Formula.RFormula (parseModel)
import qualified Hanalyze.Model.LM        as LM
import qualified Numeric.LinearAlgebra    as LA

-- ===========================================================================
-- resolveStats: VisualSpec 中の未解決 stat layer を band+line に展開
-- ===========================================================================

-- | 'VisualSpec' の各 layer を走査し、 未解決の stat layer (@MStatLM@/@MStatSmooth@) を
--   @Resolver@ でデータ解決 → hanalyze で fit → 具体 layer (band+line / line) に置換する。
--   それ以外の layer は不変。 解決/fit に失敗した stat layer は黙って除去 (描画を妨げない)。
resolveStats :: Resolver -> VisualSpec -> VisualSpec
resolveStats r vs = vs { vsLayers = concatMap (expandLayer pal r) (vsLayers vs) }
  where pal = statPalette vs

-- | B2 群色の元になる categorical palette。 ★Layout の catPalRaw と同じ規律で求める
--   (spec.palette 明示 > theme 既定 series)。 これで group 別 stat 線の色が ColorByCol
--   scatter (renderer は lpCategoricalPalette を index 参照) と一致する。
--   ggplot hue sentinel (["__ggplot_hue__"]) は群数依存ゆえ 'resolveGrouped' で展開する。
statPalette :: VisualSpec -> [Text]
statPalette vs =
  let themeDefaultPal = themeSeriesPalette (fromMaybe ThemeDefault (getLast (vsTheme vs)))
  in fromMaybe themeDefaultPal (getLast (vsPalette vs))

expandLayer :: [Text] -> Resolver -> Layer -> [Layer]
expandLayer pal r ly = case getFirst (lyKind ly) of
  Just MStatLM     -> grouped resolveLM
  Just MStatSmooth -> grouped resolveSmooth
  Just MStatPoly   -> grouped resolvePoly
  Just MStatResid  -> grouped resolveResid
  _                -> [ly]
  where grouped f = either (const []) id (resolveGrouped pal r ly f)

-- | 単群 stat resolver の型。 装飾/オプションは 'Layer'、 データは xs/ys で受ける
--   (Resolver からの解決は 'resolveGrouped' が一度だけ行う)。
type StatFn = Layer -> V.Vector Double -> V.Vector Double -> Either String [Layer]

-- ===========================================================================
-- B2: group 別 fit (= ggplot geom_smooth(aes(color=g)))
-- ===========================================================================

-- | stat layer の color encoding が群列 ('ColorByCol') を指すなら、 群ごとに分割 fit し
--   群色 (ggplotHue) で複数 line/band を重畳する。 群指定が無ければ単群で 1 回 fit (B1/B3 同等)。
resolveGrouped :: [Text] -> Resolver -> Layer -> StatFn -> Either String [Layer]
resolveGrouped palRaw r ly f = do
  xs0 <- colOf r (lyEncX ly)
  ys0 <- colOf r (lyEncY ly)
  -- ★ NaN (= Maybe 列の Nothing / 欠損) を持つ行を fit から落とす (mark の na.rm と同じ挙動)。
  --   x/y どちらかが NaN の点を除外し、 群列も同じ有効行に整列させる。
  let n0    = min (V.length xs0) (V.length ys0)
      valid = V.fromList [ i | i <- [0 .. n0 - 1]
                             , not (isNaN (xs0 V.! i)), not (isNaN (ys0 V.! i)) ]
      xs    = V.map (xs0 V.!) valid
      ys    = V.map (ys0 V.!) valid
  case groupColumn r ly of
    Nothing -> f ly xs ys                                  -- 単群 (B1/B3)
    Just gs0 -> do
      let gs      = V.map (gs0 V.!) valid                  -- 群列を有効行に整列
          groups  = orderedCats (V.toList gs)              -- Phase 28: ggplot factor 既定 = アルファベット順
          -- ★ColorByCol scatter と同色: hue sentinel は群数で展開、 他は spec/theme palette を index 参照
          palette | palRaw == ["__ggplot_hue__"] = ggplotHue (length groups)
                  | otherwise                    = palRaw
          gv      = V.toList gs
          perGroup (i, g) =
            let idxs = [ j | (j, gg) <- zip [0 ..] gv, gg == g ]
                gx   = V.fromList [ xs V.! j | j <- idxs ]
                gy   = V.fromList [ ys V.! j | j <- idxs ]
                col  = palette !! (i `mod` max 1 (length palette))
                -- 群色で上書き (内部 palette は Text 経路ゆえ ColorStatic 直構築で温存。
                --  Last 後勝ち → band/line/scatter に伝播)。
                ly'  = ly <> mempty { lyColor = Last (Just (ColorStatic col)) }
            in either (const []) id (f ly' gx gy)
      Right (concatMap perGroup (zip [0 ..] groups))

-- | lyColor が 'ColorByCol' なら群列を Resolver で文字ベクタに解決 (= 群分割キー)。
groupColumn :: Resolver -> Layer -> Maybe (V.Vector Text)
groupColumn r ly = case getLast (lyColor ly) of
  Just (ColorByCol cr) -> resolveTxt r cr
  _                    -> Nothing

-- | 出現順を保つ distinct (= Render.nubKeep 同等。 群の色対応を安定させる)。
nubKeepOrd :: Eq a => [a] -> [a]
nubKeepOrd = go []
  where go seen []     = reverse seen
        go seen (x:xs)
          | x `elem` seen = go seen xs
          | otherwise     = go (x : seen) xs

-- | geom_smooth / stat_lm の回帰線の既定線幅 (Phase 34 A1: ggplot geom_smooth は
--   @linewidth = 2 × 既定線@ = 0.753mm)。@decoOf ly@ (= user 指定 stroke) が後置で
--   上書きするので、user が 'stroke' を明示した場合はそちらが優先される。
smoothLineDefault :: Layer
smoothLineDefault = stroke (0.753 *~ mm)

-- | 装飾だけを抜き出した Layer (lyKind/encoding は持たない)。 band/line に @<>@ で合成すると
--   color/stroke/alpha/linetype が引き継がれ、 band/line 自身の lyKind は保たれる (First Monoid)。
decoOf :: Layer -> Layer
decoOf ly = mempty
  { lyColor    = lyColor    ly
  , lyStroke   = lyStroke   ly
  , lyAlpha    = lyAlpha    ly
  , lyLinetype = lyLinetype ly
  }

-- | stat layer の encoding 列を Resolver で数値ベクタに解決。
colOf :: Resolver -> Last ColRef -> Either String (V.Vector Double)
colOf r lc = case getLast lc >>= resolveNum r of
  Just v  -> Right v
  Nothing -> Left "stat layer の x/y 列を数値として解決できません"

-- | lm: 回帰線 + 95% 信頼帯。 帯は半透明 (alpha 既定 0.2)、 線は装飾引き継ぎ。
resolveLM :: StatFn
resolveLM ly xs ys = do
  (fr, _) <- fitFormula "y ~ x" xs ys
  let lvl  = fromMaybe 0.95 (getLast (lyStatLevel ly))   -- B1: statLmLevel で可変、 既定 0.95
      dm   = LM.designMatrix xs                 -- [1, x] = fitLMF "y~x" と同一設計
      cib  = LM.confidenceBand dm fr lvl
      rows = sortOn (\(x, _, _, _) -> x)
               (zip4 (V.toList xs) (fittedList fr)
                     (LM.lowerBound cib) (LM.upperBound cib))
      sx  = V.fromList [ x | (x, _, _, _) <- rows ]
      syh = V.fromList [ y | (_, y, _, _) <- rows ]
      slo = V.fromList [ l | (_, _, l, _) <- rows ]
      shi = V.fromList [ h | (_, _, _, h) <- rows ]
      bandLy = band (ColNum sx) (ColNum slo) (ColNum shi)
                 <> mempty { lyColor = lyColor ly }
                 <> maybe (alpha 0.2) alpha (getLast (lyAlpha ly))
      lineLy = line (ColNum sx) (ColNum syh) <> smoothLineDefault <> decoOf ly
  Right [ bandLy   -- 帯を先に (背面)
        , lineLy ] -- 線を後に (前面)

-- | smooth: B-spline 平滑。 knot 数は lyBinCount (既定 6)。
--   B1: 'lyStatLevel' が Just (= 'statSmoothCI') なら bs 設計行列の 'LM.confidenceBand' で
--   信頼帯 (band) + 曲線 (line)、 Nothing (= 'statSmooth') なら曲線のみ。
resolveSmooth :: StatFn
resolveSmooth ly xs ys = do
  let n       = maybe 6 id (getLast (lyBinCount ly))
      formula = "y ~ bs(x," <> T.pack (show n) <> ")"
  (fr, _) <- fitFormula formula xs ys
  case getLast (lyStatLevel ly) of
    Nothing  -> do
      let rows = sortOn fst (zip (V.toList xs) (fittedList fr))
          sx   = V.fromList (map fst rows)
          syh  = V.fromList (map snd rows)
      Right [ line (ColNum sx) (ColNum syh) <> smoothLineDefault <> decoOf ly ]
    Just lvl -> do
      dm <- designFor formula xs ys           -- bs 基底設計行列 (= fitLMF と同一)
      let cib  = LM.confidenceBand dm fr lvl
          rows = sortOn (\(x, _, _, _) -> x)
                   (zip4 (V.toList xs) (fittedList fr)
                         (LM.lowerBound cib) (LM.upperBound cib))
          sx  = V.fromList [ x | (x, _, _, _) <- rows ]
          syh = V.fromList [ y | (_, y, _, _) <- rows ]
          slo = V.fromList [ l | (_, _, l, _) <- rows ]
          shi = V.fromList [ h | (_, _, _, h) <- rows ]
          bandLy = band (ColNum sx) (ColNum slo) (ColNum shi)
                     <> mempty { lyColor = lyColor ly }
                     <> maybe (alpha 0.2) alpha (getLast (lyAlpha ly))
          lineLy = line (ColNum sx) (ColNum syh) <> smoothLineDefault <> decoOf ly
      Right [ bandLy   -- 帯を先に (背面)
            , lineLy ] -- 線を後に (前面)

-- | poly: 多項式回帰 (= ggplot stat_smooth(method="lm", formula=y~poly(x,deg)))。
--   次数 deg は lyBinCount (既定 2)。 poly 設計行列の confidenceBand で band+line に展開。
resolvePoly :: StatFn
resolvePoly ly xs ys = do
  let deg     = maybe 2 id (getLast (lyBinCount ly))
      lvl     = fromMaybe 0.95 (getLast (lyStatLevel ly))
      formula = "y ~ poly(x," <> T.pack (show deg) <> ")"
  (fr, _) <- fitFormula formula xs ys
  dm      <- designFor formula xs ys
  let cib  = LM.confidenceBand dm fr lvl
      rows = sortOn (\(x, _, _, _) -> x)
               (zip4 (V.toList xs) (fittedList fr)
                     (LM.lowerBound cib) (LM.upperBound cib))
      sx  = V.fromList [ x | (x, _, _, _) <- rows ]
      syh = V.fromList [ y | (_, y, _, _) <- rows ]
      slo = V.fromList [ l | (_, _, l, _) <- rows ]
      shi = V.fromList [ h | (_, _, _, h) <- rows ]
      bandLy = band (ColNum sx) (ColNum slo) (ColNum shi)
                 <> mempty { lyColor = lyColor ly }
                 <> maybe (alpha 0.2) alpha (getLast (lyAlpha ly))
      lineLy = line (ColNum sx) (ColNum syh) <> smoothLineDefault <> decoOf ly
  Right [ bandLy   -- 帯を先に (背面)
        , lineLy ] -- 線を後に (前面)

-- | resid: 残差 vs fitted 診断散布 (= base R plot(lm) #1)。 y~x で fit し、
--   各点を (fitted, residual=y-fitted) に写した scatter に展開する (装飾は scatter に引き継ぐ)。
resolveResid :: StatFn
resolveResid ly xs ys = do
  (fr, _) <- fitFormula "y ~ x" xs ys
  let fitted = fittedList fr
      resid  = zipWith (-) (V.toList ys) fitted
      fx     = V.fromList fitted
      ry     = V.fromList resid
  Right [ scatter (ColNum fx) (ColNum ry) <> decoOf ly ]

-- | x/y の 2 列 DataFrame を組み、 formula を parse → fitLMF (回帰計算を analyze に委譲)。
--   列名は内部固定 ("x"/"y") で、 formula も "x"/"y" を参照する。
fitFormula :: Text -> V.Vector Double -> V.Vector Double
           -> Either String (FitResult, [Text])
fitFormula formula xs ys = do
  f <- parseModel formula
  fitLMF f (xyFrame xs ys)

-- | formula の設計行列 (= confidenceBand 用)。 fitLMF と同じ modelFrame→designMatrixF 経路を
--   通すので、 bs(x,n) 等の基底展開も fit と完全一致する。
designFor :: Text -> V.Vector Double -> V.Vector Double
          -> Either String (LA.Matrix Double)
designFor formula xs ys = do
  f       <- parseModel formula
  mf      <- modelFrame f (xyFrame xs ys)
  (dm, _) <- designMatrixF f mf
  Right dm

-- | x/y 2 列の内部 DataFrame。
xyFrame :: V.Vector Double -> V.Vector Double -> DX.DataFrame
xyFrame xs ys = DX.fromNamedColumns
  [ ("x", DX.fromList (V.toList xs))
  , ("y", DX.fromList (V.toList ys)) ]

-- ===========================================================================
-- BoundPlot 向けの描画ラッパ (df 1 回参照・自動解決)
-- ===========================================================================

-- | バインド済プロットの spec を、 自分が持つ resolver で 'resolveStats' する。
resolveBound :: BoundPlot -> BoundPlot
resolveBound bp = bp { bpSpec = resolveStats (bpResolver bp) (bpSpec bp) }

-- | stat を解決してから SVG 保存 (= ggplot2 の geom_smooth 込み図を 1 回の df 参照で)。
saveSVGBoundStats :: FilePath -> BoundPlot -> IO ()
saveSVGBoundStats path = saveSVGBound path . resolveBound

-- | stat を解決してから SVG 文字列を返す。
renderBoundStats :: BoundPlot -> Text
renderBoundStats = renderBound . resolveBound
