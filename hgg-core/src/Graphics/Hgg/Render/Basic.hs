-- |
-- Module      : Graphics.Hgg.Render.Basic
-- Description : 基本 mark (scatter/line/bar/histogram/band/step/stem)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 7 A4: Render モノリス分割 (出力中立・純粋移動)。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphics.Hgg.Render.Basic where

import           Graphics.Hgg.Layout (numToText,
                                      Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, resolutionOf,
                                      AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical)
import           Graphics.Hgg.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Graphics.Hgg.Spec   (Annotation (..), AxisFormat (..),
                                      ColData (..), ColRef,
                                      ColorEnc (..), ConnectSpec (..),
                                      DAGEdge (..), DAGLayoutAlgorithm (..),
                                      DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..), DAGSpec (..), Layer (..),
                                      LegendPosition (..), LegendSpec (..),
                                      Inset (..), MarginalSpec (..), MarkKind (..),
                                      MarkShape (..), ShapeMapEntry (..),
                                      LineType (..), lineTypeDash, lineTypeForIndex,
                                      ReferenceLine (..), Resolver,
                                      Position (..), Coord (..),
                                      FacetScales (..), freeScaleX, freeScaleY,
                                      FacetSpace (..), freeSpaceX, freeSpaceY,
                                      ThemeOverride (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axShowGrid,
                                      FontSpec (..), histBinning, orderedCats,
                                      colRefName, resolveCol, resolveNum)
import           Data.Maybe          (mapMaybe, isJust)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Graphics.Hgg.Spec
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)

import           Graphics.Hgg.Primitive
import           Graphics.Hgg.Render.Common


-- | TODO-11 (2026-05-27): area band (= 信頼区間 / 予測帯)。
-- |   encX  = 共通 x、 encY = 下境界、 encY2 = 上境界
-- | PPath fill 1 枚 (= forward x-yLow + backward x-yHigh + close)。
-- | alpha は layer modifier (default 0.2)。
renderBand :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBand r layout pal ly =
  let xs   = V.toList (vecOr (lyEncX ly) r)
      yLo  = V.toList (vecOr (lyEncY ly) r)
      yHi  = case getLast (lyEncY2 ly) of
        Just c  -> V.toList (vecOr (Last (Just c)) r)
        Nothing -> []
      n  = minimum [length xs, length yLo, length yHi]
      c  = staticColorOr ly (tpDefault pal)
      a  = doubleOr (lyAlpha ly) 0.2
      coord = lpCoord layout
      pp = projectPoint coord layout
      takeN k = take k
  in if n < 2 then []
     else
       let xsN  = takeN n xs
           loN  = takeN n yLo
           hiN  = takeN n yHi
           forwardPts = zipWith pp xsN loN
           upperPts   = zipWith pp xsN hiN
           backwardPts = reverse upperPts
           segs = case forwardPts of
             []     -> []
             (h:tl) -> [MoveTo h]
                       <> map LineTo tl
                       <> map LineTo backwardPts
                       <> [ClosePath]
       in if null segs then []
          else [ PPath segs (FillStyle c a) Nothing ]

-- | Phase 52.D2: streamgraph (= 中心化積層 area、 ThemeRiver 風)。 color aes で系列分割し
-- (= 'renderBarGrouped' と同型の群キー取得)、 各 x 値で系列 y を積層、 baseline を
-- -(Σy)/2 から開始 (silhouette 中心化) して各系列を塗り polygon ('renderBand' と同型の
-- forward 下境界 + backward 上境界 + close) で描く。 wiggle 最小化 (ThemeRiver) は行わない。
renderStream :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderStream r layout pal ly =
  let xsAll = V.toList (vecOr (lyEncX ly) r)
      ysAll = V.toList (vecOr (lyEncY ly) r)
      rawKeys = case getLast (lyColor ly) of
        Just (ColorByCol cr) -> case resolveCol r cr of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> map (T.pack . show . (round :: Double -> Int)) (V.toList v)
          _                -> []
        _ -> []
      n = if null rawKeys
            then min (length xsAll) (length ysAll)
            else minimum [length xsAll, length ysAll, length rawKeys]
      keys   = if null rawKeys then replicate n "" else take n rawKeys
      rows   = zip3 (take n xsAll) keys (take n ysAll)
      groups = let cats = lyColorCats ly in if null cats then orderedCats keys else cats
      xUniq  = sort (nub [ x | (x, _, _) <- rows ])
      a      = doubleOr (lyAlpha ly) 0.8
      coord  = lpCoord layout
      pp     = projectPoint coord layout
      palArr = lpCategoricalPalette layout
      colorG gi = if null palArr then tpDefault pal
                  else palArr !! (gi `mod` length palArr)
      -- (x, group) セルの値 = 該当 row 群の和 (= 同一 (x,group) が複数 row のとき)
      cellY x g = sum [ y | (xx, gk, y) <- rows, xx == x, gk == g ]
      totalAt x = sum [ cellY x g | g <- groups ]
      -- 系列 gi の x 点での下境界 = 中心化 baseline + 先行群の累積高さ
      lowerAt x gi = negate (totalAt x / 2)
                     + sum [ cellY x (groups !! j) | j <- [0 .. gi - 1] ]
      mkSeries gi =
        let g           = groups !! gi
            forwardPts  = [ pp x (lowerAt x gi)            | x <- xUniq ]
            backwardPts = reverse [ pp x (lowerAt x gi + cellY x g) | x <- xUniq ]
            segs = case forwardPts of
              []     -> []
              (h:tl) -> [MoveTo h]
                        <> map LineTo tl
                        <> map LineTo backwardPts
                        <> [ClosePath]
        in if null segs then [] else [ PPath segs (FillStyle (colorG gi) a) Nothing ]
  in if n < 2 || length xUniq < 2 || null groups then []
     else concat [ mkSeries gi | gi <- [0 .. length groups - 1] ]

-- | Scatter: 各 (x, y) を PCircle に。
renderScatter :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderScatter r layout pal ly =
  -- NA 行を整列したまま落とすため vecOrFull (= 長さ保持) を使い、 点生成時に
  -- NaN を skip する (色/サイズ vector との index 整列を保つ = ggplot 行単位 na.rm)。
  let xs = vecOrFull (lyEncX ly) r
      ys = vecOrFull (lyEncY ly) r
      n  = min (V.length xs) (V.length ys)
      cs = colorVector r layout pal ly n
      a  = doubleOr (lyAlpha ly) 0.85
      aVec = alphaVector r ly a n        -- ★ Phase 30 A8: per-point alpha (lyAlphaBy)
      szVec = sizeVector r layout ly n   -- ★ TODO-3e: per-point size (lySizeBy)
      coord = lpCoord layout
      -- ★ TODO-3c (2026-05-29): P14 jitter (= PS Render port)。
      -- lyJitterX/Y は plotArea に対する比率 (0..1)。 hashRand で deterministic。
      jx = doubleOr (lyJitterX ly) 0.0
      jy = doubleOr (lyJitterY ly) 0.0
      area = lpPlotArea layout
      jitterOffsetX i = if jx == 0.0 then 0.0
                        else (hashRand (i * 2) - 0.5) * jx * rW area
      jitterOffsetY i = if jy == 0.0 then 0.0
                        else (hashRand (i * 2 + 1) - 0.5) * jy * rH area
      -- error bar (= Phase 26 §C-2 #6)
      errXVec = vecOr (lyErrorX ly) r
      errYVec = vecOr (lyErrorY ly) r
      -- ★ Phase 41: cap 横/縦長を ggplot 同様データ単位化 (width = markWidth × resolution)。
      --   cap 端点をデータ空間で作り projectXY に通すので flip も自動追従 (旧 px 固定を置換)。
      capWFactor = doubleOr (lyMarkWidth ly) 0.9   -- ggplot geom_errorbar 既定 width = 0.9
      finite v = not (isNaN v) && not (isInfinite v)
      resX = resolutionOf (filter finite (V.toList xs))
      resY = resolutionOf (filter finite (V.toList ys))
      capHalfX = 0.5 * capWFactor * resX  -- errorY の横 cap 半幅 (x データ単位)
      capHalfY = 0.5 * capWFactor * resY  -- errorX の縦 cap 半幅 (y データ単位)
      mkErrX i =
        let x  = xs V.! i; y = ys V.! i
            ex = errXVec V.!? i
        in case ex of
             Just dx ->
               -- errorX (x 方向誤差) の cap は y 方向 (高さ) にデータ単位で伸びる。
               let pL  = uncurry Point (projectXY coord layout (x - dx) y)
                   pR  = uncurry Point (projectXY coord layout (x + dx) y)
                   cLlo = uncurry Point (projectXY coord layout (x - dx) (y - capHalfY))
                   cLhi = uncurry Point (projectXY coord layout (x - dx) (y + capHalfY))
                   cRlo = uncurry Point (projectXY coord layout (x + dx) (y - capHalfY))
                   cRhi = uncurry Point (projectXY coord layout (x + dx) (y + capHalfY))
               in [ PLine pL pR  (solid (tpAxis pal) 1.0)
                  , PLine cLlo cLhi (solid (tpAxis pal) 1.0)
                  , PLine cRlo cRhi (solid (tpAxis pal) 1.0)
                  ]
             Nothing -> []
      mkErrY i =
        let x  = xs V.! i; y = ys V.! i
            ey = errYVec V.!? i
        in case ey of
             Just dy ->
               -- errorY (y 方向誤差) の cap は x 方向 (幅) にデータ単位で伸びる。
               let pLo = uncurry Point (projectXY coord layout x (y - dy))
                   pHi = uncurry Point (projectXY coord layout x (y + dy))
                   cLoL = uncurry Point (projectXY coord layout (x - capHalfX) (y - dy))
                   cLoR = uncurry Point (projectXY coord layout (x + capHalfX) (y - dy))
                   cHiL = uncurry Point (projectXY coord layout (x - capHalfX) (y + dy))
                   cHiR = uncurry Point (projectXY coord layout (x + capHalfX) (y + dy))
               in [ PLine pLo pHi (solid (tpAxis pal) 1.0)
                  , PLine cLoL cLoR (solid (tpAxis pal) 1.0)
                  , PLine cHiL cHiR (solid (tpAxis pal) 1.0)
                  ]
             Nothing -> []
      errorPrims = concatMap (\i -> mkErrX i <> mkErrY i) [0 .. n - 1]
      -- connect 線 (= Phase 26 §C-2 #5)
      connectPrims = case getLast (lyConnect ly) of
        Nothing -> []
        Just cs_ -> renderConnect r layout pal ly cs_ xs ys n
      pointZBefore = case getLast (lyConnect ly) of
        Just cs_ | csBefore cs_ -> connectPrims
        _                       -> []
      pointZAfter = case getLast (lyConnect ly) of
        Just cs_ | not (csBefore cs_) -> connectPrims
        _                              -> []
      hoverParts =
        [ (colRefName cr, vecOr (Last (Just cr)) r)
        | cr <- lyHover ly ]
      -- ★ Phase 28/34: マーカー塗り・縁は 'markerFillFor'/'markerStrokeFor' に一本化
      --   (既定縁なし=塗り点 shape 19、 hollow=輪郭のみ、 edge 指定時のみ縁)。
      --   凡例キー (Render.Layer) も同関数を使い、 plot と凡例の見た目を一致させる。
      -- ★ TODO-3c/3e/3f: jitter + sizeBy + shapeBy 反映
      circles =
        [ shapeToPrim sh (Point (px + jitterOffsetX i) (py + jitterOffsetY i)) sz
                      (markerFillFor ly c ai)
                      (markerStrokeFor ly c)
                      (Just (mkLabel x y i))
        | i <- [0 .. n - 1]
        , let x  = xs V.! i
              y  = ys V.! i
        , not (isNaN x), not (isNaN y)          -- NA 点を落とす (行整列維持)
        , let (px, py) = projectXY coord layout x y
              c  = cs V.! i
              ai = aVec V.! i                    -- ★ Phase 30 A8: per-point alpha
              sz = szVec V.! i
              sh = pointShapeAt ly r i
              mkLabel xv yv idx =
                let base = T.concat [ "(", numToText xv, ", ", numToText yv, ")" ]
                    extra = T.intercalate ", "
                      [ T.concat [ name, ": ", numToText v ]
                      | (name, vec) <- hoverParts
                      , Just v <- [vec V.!? idx] ]
                in if T.null extra then base else T.concat [base, " | ", extra] ]
  in pointZBefore <> errorPrims <> circles <> pointZAfter

-- ===========================================================================
-- Phase 26 A2: vector field (quiver)
-- ===========================================================================

-- | 各 (x,y) に成分 (u,v) の矢印を描く (= matplotlib @quiver@)。 矢印長は
-- autoscale (= 最長矢印がデータ対角の 8%) に 'lyArrowScale' 倍を掛けた長さ。
-- 'lyArrowMagnitude' で magnitude (√(u²+v²)) の連続色マップ (viridis)。 矢印は
-- 始点 (x,y) を根元に置く (pivot=tail・matplotlib 既定)。 magnitude 0 の矢印は
-- 退化して描かれない。
renderQuiver :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderQuiver r layout pal ly =
  let xs = vecOr (lyEncX ly) r
      ys = vecOr (lyEncY ly) r
      us = vecOr (lyEncU ly) r
      vs = vecOr (lyEncV ly) r
      n  = minimum [V.length xs, V.length ys, V.length us, V.length vs]
      coord = lpCoord layout
      w     = doubleOr (lyStroke ly) defaultLineWidth
      userScale = doubleOr (lyArrowScale ly) 1.0
      magOn = case getLast (lyArrowMagnitude ly) of Just True -> True; _ -> False
      idxs  = [0 .. n - 1]
      magAt i = let u = us V.! i; v = vs V.! i in sqrt (u * u + v * v)
      mags    = map magAt idxs
      maxMag  = if null mags then 0 else maximum mags
      -- autoscale: 最長矢印 = データ対角の 8% (data 空間で tip = (x+s*u, y+s*v))
      spanOf vec = let ws = [vec V.! i | i <- idxs]
                   in if null ws then 1
                      else let mx = maximum ws; mn = minimum ws
                           in if mx > mn then mx - mn else 1
      diag  = sqrt (spanOf xs ** 2 + spanOf ys ** 2)
      sAuto = if maxMag <= 0 then 0 else 0.08 * diag / maxMag
      s     = sAuto * userScale
      -- 単色 (theme/lyColor) を colorVector から、 magnitude 時は viridis で上書き
      cs    = colorVector r layout pal ly n
      colorAt i = if magOn && maxMag > 0 then viridis (magAt i / maxMag)
                  else cs V.! i
      arrow i
        | magAt i <= 0 = []   -- 零ベクトルは描かない
        | otherwise =
            let x = xs V.! i; y = ys V.! i; u = us V.! i; v = vs V.! i
                (px0, py0) = projectXY coord layout x y
                (px1, py1) = projectXY coord layout (x + s * u) (y + s * v)
            in drawArrow2D (Point px0 py0) (Point px1 py1) (solid (colorAt i) w)
  -- ★ Phase 36 A: 矢印は格子点から伸びて plotArea を超えうるので、 元レンジのまま
  --   plotArea でクリップする (端の矢印は途切れる)。 = ドメイン拡張より自然。
  in PClipPush (lpPlotArea layout) : concatMap arrow idxs ++ [PClipPop]

-- | Phase 26 A2: 始点 from → 終点 to の矢印 (本線 + 2 本の矢じり)。 矢じり形状は
-- 'AnnArrow' (Render/Layer.hs) と同じ (長さ 2.5mm・開き比 0.5)。
drawArrow2D :: Point -> Point -> LineStyle -> [Primitive]
drawArrow2D (Point px1 py1) (Point px2 py2) ls =
  let dx = px2 - px1; dy = py2 - py1
      len = sqrt (dx * dx + dy * dy)
      (ux, uy) = if len == 0 then (0, 0) else (dx / len, dy / len)
      ah = mmPt 2.5; aw = 0.5
      bx = px2 - ux * ah; by = py2 - uy * ah
      lx = bx - uy * ah * aw; ly = by + ux * ah * aw
      rx = bx + uy * ah * aw; ry = by - ux * ah * aw
  in [ PLine (Point px1 py1) (Point px2 py2) ls
     , PLine (Point px2 py2) (Point lx ly) ls
     , PLine (Point px2 py2) (Point rx ry) ls ]

renderLine :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderLine r layout pal ly =
  -- NA 行を整列したまま落とすため vecOrFull (長さ保持) で取り、 seg で NaN 点を
  -- 除いてから線分化する (= ggplot が NA で線を切らず詰める na.rm 既定相当)。
  let xs = V.toList $ vecOrFull (lyEncX ly) r
      ys = V.toList $ vecOrFull (lyEncY ly) r
      w  = doubleOr (lyStroke ly) defaultLineWidth
      coord = lpCoord layout
      pp = projectPoint coord layout
      -- Phase 11 A4-b: 固定 linetype (= ggplot linetype=)。 既定 Solid → [] = 実線。
      fixedDash = maybe [] lineTypeDash (getLast (lyLinetype ly))
      -- 連続点列を dash 付き線分群に (色は引数で指定)。 NaN (= NA) 点は除く。
      seg col dash pts =
        let pts' = [ p | p@(x, y) <- pts, not (isNaN x), not (isNaN y) ]
        in [ PLine (pp xa ya) (pp xb yb) (LineStyle col w dash)
           | ((xa, ya), (xb, yb)) <- zip pts' (drop 1 pts') ]
  in case getLast (lyColor ly) of
       -- Phase 52.A10: ColorByCol は群ごとに色付き線 (= ggplot color=group)。 単一カテゴリ
       -- (statLabel 1 本) なら 1 本を該当カテゴリ色で描く。 旧実装は ColorByCol を staticColorOr
       -- が拾えず default 単色に潰れ、 異モデル重畳の色分けが効かなかった。 色は 'colorVector'
       -- (scale_color_manual 辞書→palette index) を流用し各群代表点 (=同カテゴリゆえ同色) を採る。
       Just (ColorByCol cr) | Just keys <- groupKeysOf r cr ->
         let cs     = V.toList (colorVector r layout pal ly (length xs))
             groups = orderedGroups keys (zip3 xs ys cs)
             lineFor (_, gpts) =
               let col = case gpts of ((_, _, gc) : _) -> gc; [] -> tpDefault pal
                   pts = [ (gx, gy) | (gx, gy, _) <- gpts ]
               in seg col fixedDash pts
         in concatMap lineFor groups
       _ ->
         let c = staticColorOr ly (tpDefault pal)
         in case getLast (lyLinetypeBy ly) >>= groupKeysOf r of
              -- linetypeBy (= ggplot linetype=factor(g)): 群ごとに別 line。 dash は
              -- 既定では群ごとに巡回するが、 固定 linetype (lyLinetype) があれば全群その dash。
              -- (= ggplot aes(group=g) 相当の「色も線種も変えない純粋な群分割」。
              --  例: linetypeBy "track" <> linetype LtSolid で全曲を実線で群分割。)
              Just keys ->
                let dashFor i = maybe (lineTypeDash (lineTypeForIndex i)) lineTypeDash
                                      (getLast (lyLinetype ly))
                in concat [ seg c (dashFor i) gpts
                          | (i, (_, gpts)) <- zip [0 ..] (orderedGroups keys (zip xs ys)) ]
              Nothing   -> seg c fixedDash (zip xs ys)

-- | Phase 9 B: position adjustment 対応 dispatcher。
--   既定 (position identity) または群分け (color aesthetic) 無しは従来の単色 bar
--   ('renderBarSimple')。 dodge/stack/fill かつ categorical x かつ ColorByCol 群分けあり
--   のとき 'renderBarGrouped' で系列を並べる。
renderBar :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBar r layout pal ly =
  let pos = maybe PosIdentity id (getLast (lyPosition ly))
      grpKeys = case getLast (lyColor ly) of
        Just (ColorByCol cr) -> case resolveCol r cr of
          Just (TxtData v) -> Just (V.toList v)
          Just (NumData v) -> Just (map (T.pack . show . (round :: Double -> Int)) (V.toList v))
          _                -> Nothing
        _ -> Nothing
      isCat = not (null (lpXCategoryLabels layout))
  in case (pos, grpKeys) of
       (PosIdentity, _)       -> renderBarSimple r layout pal ly
       (_, Just keys) | isCat -> renderBarGrouped pos keys r layout pal ly
       _                      -> renderBarSimple r layout pal ly

-- | position identity / 群分けなしの bar。 ★Phase 19 A2: 色は 'colorVector' に
-- 委譲 (ColorByCol で per-bar 色分け = ggplot の identity + fill aesthetic 同型。
-- ColorStatic / 色指定なしは colorVector が単色を返すので従来挙動不変)。
renderBarSimple :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBarSimple r layout pal ly =
  -- categorical x: 各 row の label を xCats (= x 軸のカテゴリ列) の index へ。
  -- ★Phase 19 A2: 旧実装は row index (0..n-1) に置いており、 カテゴリ重複行が
  -- x domain を超えて plot 域をはみ出していた (実測: 3 行 2 cat で 3 本目が
  -- 右枠外)。 cat index 配置なら重複行は同 slot に重ね描き = ggplot identity
  -- 同型。 重複なしデータは row index = cat index でビット不変。
  let xCats = lpXCategoryLabels layout
      isCat = not (null xCats)
      xsNum = vecOr (lyEncX ly) r
      ys    = vecOr (lyEncY ly) r
      xs    = if isCat
                then case getLast (lyEncX ly) >>= resolveCol r of
                       Just (TxtData v) ->
                         V.map (\t -> maybe 0 fromIntegral (elemIndex t xCats)) v
                       _ -> V.fromList [ fromIntegral i | i <- [0 .. V.length ys - 1] ]
                else xsNum
      -- ★ Phase 34: fill geom (bar) の無指定既定は tpDefaultFill (ggplot grey35)。
      --   colorVector の fallback は tpDefault なので fill 色に差し替えた pal を渡す。
      cs    = colorVector r layout (pal { tpDefault = tpDefaultFill pal }) ly (V.length ys)
      a     = doubleOr (lyAlpha ly) 1.0   -- ★ Phase 34: ggplot bar は不透明
      coord = lpCoord layout
      sx    = scaleApply (lpXScale layout)
      -- Phase 8 B7: bar 境界線。 default False (= ggplot 流フラット)。
      border = case getLast (lyHistBorder ly) of
        Just b  -> if b then Just (StrokeStyle "#ffffff" 1.0) else Nothing
        Nothing -> Nothing
      -- bar 幅 (Phase 8 A2 Step4b/4c, design §A-6): categorical は ggplot 既定 = resolution*0.9。
      -- categorical の resolution=1 なので「1 データ単位の pixel 幅 (= sx 1 - sx 0)」 の 0.9。
      -- Step4c: 旧 rW/nBars (個数ベース) を unit ベース (xUnit) に変更。 ±0.5 expansion 下では
      -- xUnit == rW/nBars だが、 ±0.6 で domain span が n→n+0.2 に変わっても棒が比例して縮み
      -- 隣と接触しない (= 軸スケールに追従)。 numeric は resolution 未算出のため従来 60% 維持。
      area  = lpPlotArea layout
      nBars = max 1 (V.length xs)
      -- Phase 10 A4-fix: 厚みは coord に応じた cross 軸単位 (flip では縦スロット幅)。
      xUnit = catUnitPx coord layout
      bw    = if isCat
                then xUnit * 0.9
                else rW area / fromIntegral nBars * 0.6
      -- Phase 10 A3: bar は projectBarRect で flip 追従 (厚み bw は px のまま、 base=0..value)。
      -- Cartesian は Rect (sx x - bw/2)(min (sy y)(sy 0)) bw (abs (sy y - sy 0)) と bit 一致。
      -- Phase 11 A7-c: 極座標は扇形 (wedge) で描く。 PolarX = rose (角度帯×半径=値)、
      --   PolarY = 中心からの扇形 (角度=値×半径帯)。 厚みは frac 単位の角度/半径幅。
      spanX = lsDomainHi (lpXScale layout) - lsDomainLo (lpXScale layout)
      hwFrac = if spanX == 0 then 0.5 else 0.45 / spanX
      dfx = domFrac (lpXScale layout)
      dfy = domFrac (lpYScale layout)
      mkWedge x y = case coord of
        CoordPolarY -> wedgeSegments layout (dfy 0) (dfy y)
                                     (max 0 (dfx x - hwFrac)) (dfx x + hwFrac)
        _           -> wedgeSegments layout (dfx x - hwFrac) (dfx x + hwFrac)
                                     (dfy 0) (dfy y)
  in if isPolar coord
       then [ PPath (mkWedge x y) (FillStyle c a) border
            | (x, y, c) <- zip3 (V.toList xs) (V.toList ys) (V.toList cs) ]
       else [ PRect (projectBarRect coord layout x 0 y bw)
                    (FillStyle c a) border
            | (x, y, c) <- zip3 (V.toList xs) (V.toList ys) (V.toList cs) ]

-- | Phase 9 B: 群分け bar の position adjustment (dodge / stack / fill)。
--   long-form データ (= 各 row が (x-cat, group, value)) を前提に、 x カテゴリ slot 内で
--   系列 (= color/group aesthetic) を横並び (dodge) / 縦積み (stack) / 100% 正規化 (fill) する。
--   色は群 index → categorical palette (= 'colorVector' の ColorByCol と同一割当)。
renderBarGrouped :: Position -> [Text] -> Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBarGrouped pos keys r layout pal ly =
  let xCats = lpXCategoryLabels layout
      xLabels = case getLast (lyEncX ly) of
        Just cr -> case resolveCol r cr of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> map (T.pack . show . (round :: Double -> Int)) (V.toList v)
          _                -> []
        _ -> []
      ys     = V.toList (vecOr (lyEncY ly) r)
      groups = let cats = lyColorCats ly in if null cats then orderedCats keys else cats
      nG     = max 1 (length groups)
      nX     = max 1 (length xCats)
      a      = doubleOr (lyAlpha ly) 1.0   -- ★ Phase 34: ggplot bar は不透明
      border = case getLast (lyHistBorder ly) of
        Just b  -> if b then Just (StrokeStyle "#ffffff" 1.0) else Nothing
        Nothing -> Nothing
      coord  = lpCoord layout
      sx     = scaleApply (lpXScale layout)
      xUnit  = catUnitPx coord layout   -- Phase 10 A4-fix: flip では縦スロット幅
      slotW  = xUnit * 0.9
      palArr = lpCategoricalPalette layout
      colorG gi = if null palArr then tpDefault pal
                  else palArr !! (gi `mod` length palArr)
      rows   = zip3 xLabels keys ys
      -- (xi, gi) セルの値 = 該当 row 群の和 (= 同一 (cat,group) が複数 row の場合)
      cellY xi gi = sum [ y | (xl, gk, y) <- rows
                            , elemIndex xl xCats  == Just xi
                            , elemIndex gk groups == Just gi ]
      -- stack / fill 共通の縦積み (fill は各 cat 合計 1 に正規化してから積む)
      stackCol xi =
        let total = sum [ cellY xi gi | gi <- [0 .. nG - 1] ]
            scaleV v = case pos of
              PosFill -> if total == 0 then 0 else v / total
              _       -> v
            go _   []          = []
            go cum (gi : rest) =
              let yv  = scaleV (cellY xi gi)
                  top = cum + yv
                  -- Phase 10 A4: slot 中心 = data x=xi、 base..top を data 値で、 厚み slotW px。
                  rect = PRect (projectBarRect coord layout (fromIntegral xi) cum top slotW)
                               (FillStyle (colorG gi) a) border
              in (if yv /= 0 then [rect] else []) ++ go top rest
        -- Phase 28: ggplot position_stack は凡例の逆順で積む (= 第 1 水準が一番上)。
        -- 群を逆順に積むと gi=0 (例: Adelie) が最上段に来て R4DS と一致する。
        in go 0 (reverse [0 .. nG - 1])
  in case pos of
       PosDodge ->
         let subW = slotW / fromIntegral nG
         in [ PRect (projectBarRect coord layout centerD 0 yv subW)
                    (FillStyle (colorG gi) a) border
            | xi <- [0 .. nX - 1], gi <- [0 .. nG - 1]
            , let yv   = cellY xi gi, yv /= 0
            -- Phase 10 A4: sub-bar 中心を data 空間で (= xi-0.45+(gi+0.5)*0.9/nG)。
            -- affine なので Cartesian px は旧 px-offset 版と一致。
            , let centerD = fromIntegral xi - 0.45
                            + (fromIntegral gi + 0.5) * 0.9 / fromIntegral nG ]
       _ -> concat [ stackCol xi | xi <- [0 .. nX - 1] ]

renderHistogram :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderHistogram r layout pal ly =
  let xs = V.toList $ vecOr (lyEncX ly) r
      -- ★ Phase 34: histogram の無指定既定は tpDefaultFill (ggplot grey35)。
      c  = staticColorOr ly (tpDefaultFill pal)
      a  = doubleOr (lyAlpha ly) 1.0   -- ★ Phase 34: ggplot histogram は不透明
      -- TODO-3a (2026-05-29): histDensity = True なら count を density に正規化
      -- (= count / (totalN * binW))。 SVG export でも density モードが動くように。
      isDensity = case getLast (lyHistDensity ly) of
        Just b -> b
        Nothing -> False
      -- Phase 8 B7: bin 境界線 (= 白枠) を出すか。 default False (= ggplot 流フラット)。
      border = case getLast (lyHistBorder ly) of
        Just b  -> if b then Just (StrokeStyle "#ffffff" 1.0) else Nothing
        Nothing -> Nothing
  in if null xs then [] else
    -- Phase 8 B7: bin 境界は lpHistDomain (= 全 histogram layer 共通の生 min/max) を
    -- 単一情報源とする。 y-range 計算 (sharedHistYRange) と同じ domain なので bin 幅が
    -- 一致し、 バーが y range を突き抜けない。 padded な x scale domain は使わない。
    let dom = case lpHistDomain layout of
          Just d  -> d
          Nothing -> (minimum xs, maximum xs)
        -- Phase 28: bin 化は Spec.histBinning に一元化 (binWidth 優先・ggplot 流 origin)。
        (origin, binW, nBin) = histBinning ly dom
        binIx v = min (nBin - 1) (max 0 (floor ((v - origin) / binW)))
        counts = foldl (\acc v -> let i = binIx v
                                  in take i acc <> [acc !! i + 1] <> drop (i+1) acc)
                       (replicate nBin (0 :: Int)) xs
        totalN = length xs
        toY cnt = if isDensity && totalN > 0 && binW > 0
                    then fromIntegral cnt / (fromIntegral totalN * binW)
                    else fromIntegral cnt
        coord = lpCoord layout
        -- Phase 10 A3: histogram bin は bin 幅が data 単位なので projectRectData (data bbox 転置)。
        -- Cartesian は従来の x [bin..bin+w] × y [0..toY] Rect と bit 一致。
    in [ PRect (projectRectData coord layout
                  (origin + fromIntegral i * binW) (origin + fromIntegral (i+1) * binW)
                  0 (toY cnt))
               (FillStyle c a) border
       | (i, cnt) <- zip [0..nBin-1] counts
       , cnt > 0  -- 高さ 0 bin はスキップ (= 軸線 artifact 防止)
       ]

-- | Step plot (Phase 6+ C-3): lyEncX = x、 lyEncY = y、 階段折れ線。
-- 各 segment は (x_i, y_i) → (x_{i+1}, y_i) → (x_{i+1}, y_{i+1})。
renderStep :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderStep r layout pal ly =
  let xs = V.toList (vecOr (lyEncX ly) r)
      ys = V.toList (vecOr (lyEncY ly) r)
      c  = staticColorOr ly (tpDefault pal)
      w  = doubleOr (lyStroke ly) defaultLineWidth
      coord = lpCoord layout
      pp = projectPoint coord layout
      pts = zip xs ys
      dash = maybe [] lineTypeDash (getLast (lyLinetype ly))  -- Phase 11 A4-b
      ls = LineStyle c w dash
      mkSegs [] = []
      mkSegs [_] = []
      mkSegs ((x1, y1) : (x2, y2) : rest) =
        [ PLine (pp x1 y1) (pp x2 y1) ls
        , PLine (pp x2 y1) (pp x2 y2) ls
        ] ++ mkSegs ((x2, y2) : rest)
  in mkSegs pts

-- | Stem / lollipop plot (Phase 6+ C-3): 縦棒 + 上端 circle marker。
renderStem :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderStem r layout pal ly =
  let xs = V.toList (vecOr (lyEncX ly) r)
      ys = V.toList (vecOr (lyEncY ly) r)
      c  = staticColorOr ly (tpDefault pal)
      w  = doubleOr (lyStroke ly) defaultLineWidth
      sz = doubleOr (lySize ly) defaultMarkerDiameter  -- ★ Phase 34 A3: 直径 (PCircle で /2)・scatter と統一
      a  = doubleOr (lyAlpha ly) 0.9
      coord = lpCoord layout
      sy = scaleApply (lpYScale layout)
      -- Phase 8 B23-fix: y=0 が plotArea 内ならそこを baseline、 そうでなければ下端に
      -- clamp (= PS renderStem と同方式)。 全 y が正のとき sy 0 が枠下に出て棒が下軸を
      -- 貫通するのを防ぐ。
      area = lpPlotArea layout
      areaBottom = rY area + rH area
      base0 = sy 0
      base = if base0 >= rY area && base0 <= areaBottom then base0 else areaBottom
      -- Phase 10 A2: marker は projectPoint で flip 追従。 stem 線の baseline (base) は
      -- clamp 済みの px 値軸基準のため Cartesian のまま (flip 時の baseline 入替は後段で対応)。
      mkOne x y =
        let (px, py) = projectXY coord layout x y
        in [ PLine (Point px base) (Point px py) (solid c w)
           , PCircle (Point px py) (sz / 2) (FillStyle c a)
                    (Just (StrokeStyle c 1.0)) Nothing
           ]
  in concatMap (uncurry mkOne) (zip xs ys)
