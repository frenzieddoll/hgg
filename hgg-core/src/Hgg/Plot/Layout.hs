-- |
-- Module      : Hgg.Plot.Layout
-- Description : Layer 2 ─ Layout 計算 (Phase 26 §A-4 ColRef 対応版)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 'VisualSpec' から viewport / scale / axis tick を計算する純粋関数群。
-- col 名参照は 'Resolver' で Vector に解決した上で extent を求める。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Layout
  ( Layout(..)
  , ViewportSize(..)
  , Rect(..)
  , Scale(..)
  , computeLayout
  , scaleApply
    -- ★ Phase 33 B3: 相対単位込み座標 'Pos' の pt 解決 (Layout の産物 = rect/scale
    --   が相対単位の意味を決める ⇒ resolver は Layout 側に置く・Unit は型のみ)。
  , UCtx(..)
  , resolvePosX
  , resolvePosY
  , niceTicks
  , niceTicksLog
  , extendedBreaks
  , formatTicksGG
    -- ★ Phase 8 A2 Step1: 描画側 (Render) と共有する margin 定数 / scale。
  , ggMarginScale
  , ggHalfLine
  , ggTickLen
  , ggAxTextMar
  , ggAxTitleMar
    -- ★ Phase 35/38: 凡例メトリクス定数 + content-based 幅 (Render と共有)。
  , legendBaseSize
  , legendKeyW
  , legendKeyPitch
  , isWideChar
  , textWidthEm
  , dagLabelFs
  , dagNodeBaseHalfWidth
  , legendGuideWidth
    -- ★ Phase 38: 凡例ラベル収集 (Render/Layer から集約・予約と描画の単一情報源)。
  , numToText
  , nubKeep
  , findColorEnc
  , effectiveLegendTitle
  , allColorCategories
  , LegendGuide(..)
  , collectGuides
    -- ★ Phase 8 C (gtable §E): 汎用 1 次元トラック割付 (ggplot gtable 忠実レイアウタの基盤)。
  , Track(..)
  , solveTracks
    -- ★ Phase 9 A-5: legend 配置 (PS と同一)。 予約 (computeLayout) と描画 (Render) で共有。
  , needsLegend
  , effectiveLegendPos
  , hasColorEncoding
    -- ★ Phase 9 C: coord_flip 用の座標投影 helper (Render が共有)。
  , projectXY
  , projectRectData
  , projectBarRect
  , catUnitPx
  , resolutionOf
  , AxisPlacement(..)
  , coordXAxisPlacement
  , coordYAxisPlacement
  , coordXGridIsVertical
  , coordOf
  , isPolar
  , polarCenter
  , polarPoint
  , domFrac
  ) where

import           Hgg.Plot.Layout.RangeOf (collectXY, extentsOrDefault,
                                              histRawDomain)
import           Hgg.Plot.Palette (ggplotHue)
import           Hgg.Plot.Unit (lengthToPt, Pos (..))
import           Hgg.Plot.Spec (AxisKind (..), AxisSpec (..), ColData (..),
                                    DAGNode (..), DAGNodeKind (..),
                                    ColRef, ColorEnc (..), FontSpec (..), Layer (..),
                                    LegendPosition (..), LegendSpec (..),
                                    MarkKind (..), Resolver,
                                    ThemeName (..), Coord (..),
                                    VisualSpec (..), YAxisSide (..),
                                    applyDiscreteLimits, axisKindOf, ridgeAutoFlip,
                                    axTickValsOf, axTickLabelsOf, axisRotateOf, distGroupRef,
                                    compositeLanes, colRefName,
                                    lgPosition, lyColor, lyColorCats, lyShapeBy,
                                    lyEncX, lyEncY, lyKind, lyBinCount,
                                    lyYAxisSide, orderedCats, resolveCol,
                                    resolveNum, themeSeriesPalette,
                                    HexCell (..), hexbinLayerCells)
import           Hgg.Plot.Primitive (Rect (..))  -- Phase 51: leaf へ移設・re-export
import           Numeric           (showFFloat)
import           Data.Aeson        (FromJSON, ToJSON)
import           Data.List         (foldl', nub, group, sort)
import           Data.Monoid       (First (..), Last (..), getFirst)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Vector       (Vector)
import qualified Data.Vector       as V
import           GHC.Generics      (Generic)

data ViewportSize = ViewportSize { vsW :: !Int, vsH :: !Int }
  deriving (Show, Eq, Generic)

instance ToJSON   ViewportSize
instance FromJSON ViewportSize

-- Phase 51: 'Rect' は 'Hgg.Plot.Primitive' (leaf) へ移設。 本 module は
-- import + export list で re-export し、 既存の @import Layout (Rect(..))@ を不変に保つ。

-- | Phase 26 §A-4: Linear のみ。 Log / Sqrt / Time / Ordinal / Band は後続。
-- | Phase 26 §C-2 #1: PlotConfig.xLog / yLog 等価。 LinearScale に加えて
-- LogScale を追加 (= 自然対数 ln で線形化、 描画は底 10 で tick 表示)。
data Scale
  = LinearScale { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  | LogScale    { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  -- | Sqrt scale (P15、 Phase 6 A6): forward = sqrt v (= 数値が非負の domain 限定、
  --   負値は range 下端 clip)。 inverse は描画側で不要 (= tick は値域、 表示は元値)。
  | SqrtScale   { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  -- | Time scale (P7、 Phase 6 A7): unix epoch (Double seconds) を Linear で扱う。
  --   tick は niceTimeTicks (= 1m / 1h / 1d / 1w / 1M / 1y candidates)。
  --   表示 format は AxisFormat の AxisTimeFmt 経由 (= Render 側)。
  | TimeScale   { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  deriving (Show, Eq)

data Layout = Layout
  { lpViewport :: !ViewportSize
  , lpPlotArea :: !Rect
  , lpXScale   :: !Scale
  , lpYScale   :: !Scale
    -- ★ Phase 9 C: coord_flip 用。 データ x を縦 px・データ y を横 px に写す scale。
    --   domain は lpXScale/lpYScale と同一 (= categorical ±0.6 / baseline / funnel を継承)、
    --   range のみ縦横入替。 常時算出するが Cartesian では未使用。 projectXY が参照。
  , lpXScaleFlipped :: !Scale   -- データ x の domain、 range = 縦 px (Y と同じ反転 [rY+rH, rY])
  , lpYScaleFlipped :: !Scale   -- データ y の domain、 range = 横 px [rX, rX+rW]
    -- ★ Phase 10 A2: spec の座標系 (= coordOf spec)。 spec を持たない各 mark renderer が
    --   projectXY/projectPoint で参照するため Layout に保持 (Cartesian は従来と bit 一致)。
  , lpCoord :: !Coord
    -- ★ Phase 8 B22: dual Y 軸 (右側)。 vsYAxisRight が指定された / 右軸 layer が
    --   ある場合のみ Just。 右軸 layer の y 値だけから独立に scale を作る (= 左軸とは
    --   別 domain)。 Nothing なら従来通り単一 Y 軸。
  , lpYScaleRight :: !(Maybe Scale)
  , lpXTicks   :: ![Double]
  , lpYTicks   :: ![Double]
  , lpYTicksRight :: ![Double]   -- ★ Phase 8 B22 右軸 tick (右軸無効なら [])
  , lpCategoricalPalette :: ![T.Text]   -- ★ P17 (= default hggMain F-3)
  , lpContinuousPalette  :: ![T.Text]   -- ★ P17 (= default viridis5)
    -- ★ Phase 11 A4-e: 色/サイズ scale 拡充 (spec 駆動、 colorVector/sizeVector が参照)。
  , lpColorManual :: ![(T.Text, T.Text)]              -- scale_color_manual (空 = 無指定)
  , lpColorGradient2 :: !(Maybe (T.Text, T.Text, T.Text, Double))  -- scale_color_gradient2
  , lpSizeRange :: !(Double, Double)                  -- scale_size range (default (3,10))
    -- ★ Phase 6+ case C-1: categorical x 軸の label (= ColTxt 由来)。
    --   非空なら tick label として整数位置 0..n-1 の代わりにこれを使う。
    --   空なら通常の numeric tick label。
  , lpXCategoryLabels :: ![T.Text]
  , lpYCategoryLabels :: ![T.Text]
    -- ★ Phase 11 A4-d: 明示 tick ラベル (= ggplot labels=)。 非空なら lpXTicks/lpYTicks と
    --   1:1 対応で formatTick を上書き。 空なら従来通り (numeric は値 format、 categorical は
    --   lpXCategoryLabels)。 axTickLabels 指定時のみ非空。
  , lpXTickLabels :: ![T.Text]
  , lpYTickLabels :: ![T.Text]
    -- ★ Phase 8 B7: 全 histogram layer 共通の生 (pad なし) x domain (lo, hi)。
    --   render と y-range 計算が同じ bin 境界を使うため (= はみ出し防止)。
  , lpHistDomain :: !(Maybe (Double, Double))
    -- ★ Phase 8 A2 Step1: margin 縮小係数 (= ggMarginScale)。 描画オフセット
    --   (tick/label/title) を computeLayout と同じ sc で算出するため Layout に保持。
    --   subplots は viewport を 0 に上書きするため viewport から再計算できない。
  , lpMarginScale :: !Double
    -- ★ Phase 8 A2 Step1: 計算済み 4 辺マージン (px)。 描画 (title/軸タイトル) は
    --   plotArea からこれだけ外側に配置する。 subplots panel は plotArea が cell 位置に
    --   平行移動されるが本値 (panel 自身の margin) を保持するので panel 端基準で配置できる。
  , lpMarginTop    :: !Double
  , lpMarginLeft   :: !Double
  , lpMarginBottom :: !Double
  } deriving (Show, Eq)

-- | 'VisualSpec' の全 layer から 'Resolver' で encX/encY を解決、 全 layer
-- 横断で extent を計算。 viewport は spec の width/height、 余白は固定 margin。
computeLayout :: Resolver -> VisualSpec -> Layout
computeLayout r spec0 =
  -- ★ Phase 18 A2: 離散 limits (scale{X,Y}DiscreteLimits) を先に解決 (冪等・
  --   未指定なら完全 no-op)。 renderToPrimitives 側も同じ解決を通るので整合する。
  let spec = ridgeAutoFlip (applyDiscreteLimits r spec0)  -- ★ B1c: ridge は coord_flip 自動付与
      -- ★ Phase 33 B4: layout は純 pt 空間 ([[Option 1]])。figure size を pt に解決
      --   (px 入力のみ dpi で pt 化)。既定 468×288pt = 6.5×4in (aspect 1.625・横長)。
      --   横長はデータ図の相関構造が読みやすく R4DS 本文の chunk 比にも近い (B8 で確定)。
      --   raster backend が k=dpi/72 を掛けて device px にするのは B5 (backend 1 箇所)。
      --   dpi は px 入力解決にのみ使う。
      dpiVal = maybe 96 id (getLast (vsDpi spec))
      w = maybe 468 (lengthToPt dpiVal) (getLast (vsWidth  spec))
      h = maybe 288 (lengthToPt dpiVal) (getLast (vsHeight spec))
      vp = ViewportSize (round w) (round h)
      -- Phase 8 A2 Step1 (design §D): ggplot half_line マージンモデル。 固定 px (旧
      -- 60/40/40/50) を全廃し、 plot.margin(halfLine) + grob 実寸 (title/y目盛幅/tick長/
      -- 軸タイトル) を積み上げて算出。 sc は小 viewport (inset/pairs) 用の縮小係数 (下限
      -- 0.4)。 描画オフセット (Render tickMarks/labels) も同じ定数から導出する。
      sc = ggMarginScale w h
      -- Phase 8 B22: 右 Y 軸がある場合は plotArea 右端を 40px 追加で空ける (= PS と同値)。
      hasRightY = case getLast (vsYAxisRight spec) of
        Just _  -> True
        Nothing -> any (\l -> getLast (lyYAxisSide l) == Just YAxisRight) (vsLayers spec)
      rightAxisW = if hasRightY then 40 else 0
      -- フォント実寸 (spec 指定 > default)。 maxYTickW は y 目盛りラベルの最大文字幅
      -- (numeric は fmtNum で近似、 軸 format は width 推定では無視 = Step1 許容)。
      -- ★ Phase 34: 既定フォント実寸を ggplot theme_grey 較正値に合わせる
      --   (Render.mkFontTS と同値。 旧 16/12/11 は margin 過大予約 → 軸タイトルが遠かった)。
      titleSize     = fontSizeOf (vsTitleFont     spec) 13.2  -- plot.title  base×1.2
      axisLabelSize = fontSizeOf (vsAxisLabelFont spec) 11    -- axis.title  base
      tickSize      = fontSizeOf (vsTickFont      spec) 8.8   -- axis.text   base×0.8
      -- 左 margin 用の y 目盛りラベル: 離散 limits (yCatLabels) > 明示ラベル
      -- (axisBreaksLabeled = explicitYLabs) > numeric tick の順で採用する。
      -- (明示ラベルを測らないと長い category ラベルが軸外へ溢れる)。
      yTickLabelStrs
        | not (null yCatLabels)    = yCatLabels
        | not (null explicitYLabs) = explicitYLabs
        | otherwise                = formatTicksGG yTicks
      maxYTickW = if null yTickLabelStrs then 0
                  else 0.6 * tickSize
                         * fromIntegral (maximum (map T.length yTickLabelStrs))
      hasTitle  = case getLast (vsTitle  spec) of Just _ -> True; _ -> False
      hasXLabel = case getLast (vsXLabel spec) of Just _ -> True; _ -> False
      hasYLabel = case getLast (vsYLabel spec) of Just _ -> True; _ -> False
      -- ★ Phase 37 A1: subplots container は自分の軸を描かない。 軸目盛り/軸タイトル分の
      --   マージン (tickLen/axTextMar/maxYTickW/軸タイトル) を予約せず、 plot.margin と
      --   タイトル帯・凡例・caption のみにする (= 描画範囲を各 panel に明け渡す)。
      --   従来は container が phantom 軸マージンを取り、 内側 panel が二重取りしていた。
      isContainer = not (null (vsSubplots spec))
      -- Phase 11 A5-a: labs (subtitle/caption/tag) の margin 予約。 未指定なら 0 で
      -- 従来同一。 subtitle は top に積み増し、 caption は bottom、 tag は title/subtitle
      -- が無い時のみ top (= 在る時は左寄せタグが title 帯に同居できる)。
      hasSubtitle = case getLast (vsSubtitle spec) of Just _ -> True; _ -> False
      hasCaption  = case getLast (vsCaption  spec) of Just _ -> True; _ -> False
      hasTag      = case getLast (vsTag      spec) of Just _ -> True; _ -> False
      labsSubExtra = if hasSubtitle then 11 + sc * ggHalfLine else 0
      labsTagExtra = if hasTag && not (hasTitle || hasSubtitle) then 13 + sc * ggHalfLine else 0
      labsCapExtra = if hasCaption then 9 + sc * ggHalfLine else 0
      -- Phase 8 C (small-viewport text fix): 間隔定数 (halfLine/tickLen/axTextMar/
      -- axTitleMar) は sc 倍するが、 文字サイズ由来の項 (titleSize/tickSize/maxYTickW/
      -- axisLabelSize) は **等倍** (フォントは実寸描画で縮まないため)。 旧実装は全体を
      -- sc 倍し、 小 viewport (subplots/pairs/inset) で数値が軸に被っていた。 sc=1 では
      -- 新旧同値なので通常プロットは不変。
      tM = sc * ggHalfLine + (if hasTitle then titleSize + sc * ggHalfLine else 0)
                 + labsSubExtra + labsTagExtra
      -- ★ x 目盛りラベルの回転 (axisRotate) 予約: 非回転は tickSize (従来) だが、
      --   回転時はラベル**幅**が下方向に伸びる。 左 margin の maxYTickW と対称に、
      --   x 目盛りラベルの最大文字幅を回転角で投影して予約する (rotX=0 で従来同値)。
      xRot = axisRotateOf (vsXAxis spec)
      xTickLabelStrs
        | not (null xCatLabels)    = xCatLabels
        | not (null explicitXLabs) = explicitXLabs
        | otherwise                = []          -- numeric は短いので従来 tickSize 予約で足る
      maxXTickW = if null xTickLabelStrs then 0
                  else 0.6 * tickSize
                         * fromIntegral (maximum (map T.length xTickLabelStrs))
      -- Phase 50 A2: 回転 x ラベル (符号によらず rotX≠0) はラベル**幅**が下へ張り出すので、
      --   左 margin の maxYTickW と対称に、 最大文字幅を回転角で投影して予約する
      --   (rotX=0 で従来 tickSize と一致)。 ggplot の回転ラベル margin と同方針。
      xTickReserve
        | xRot == 0 = tickSize
        | otherwise = let rad = xRot * pi / 180
                      in tickSize * abs (cos rad) + maxXTickW * abs (sin rad)
      bM | isContainer = sc * ggHalfLine + legendH + labsCapExtra
         | otherwise   = sc * (ggHalfLine + ggTickLen + ggAxTextMar) + xTickReserve
                 + (if hasXLabel then sc * ggAxTitleMar + axisLabelSize else 0)
                 + legendH + labsCapExtra
      lM | isContainer = sc * ggHalfLine
         | otherwise   = sc * (ggHalfLine + ggTickLen + ggAxTextMar) + maxYTickW
                 + (if hasYLabel then sc * ggAxTitleMar + axisLabelSize else 0)
      -- Phase 9 A-5 (PS Layout と同一): 凡例ぶん plotArea を縮めて図内に収める (ggplot は
      -- legend を gtable の一部として扱い panel を縮める)。 Inside/None は予約しない。
      -- ★ Phase 34: facet 時も右凡例を予約する (旧実装は facet で legendW=0 にして凡例を
      --   完全に落としていた = ggplot は facet でも凡例を出す)。
      legendPos = needsLegend spec (effectiveLegendPos (vsLegend spec))
      -- Phase 11 A5-c: nrow グリッドぶん予約を拡げる (default 1 で従来同一 = ゼロ diff)。
      --   ★ Phase 38: 右凡例は縦スタック (renderGuideBlock 単列) なので legNcol 予約は廃止。
      legNrow = max 1 (maybe 1 id (getLast (vsLegendNrow spec)))
      -- ★ Phase 38: 右凡例幅を「最長ラベル」で算出 (固定 80/+70列 を撤去)。 renderGuideBlock の
      --   描画式に一致する 'legendGuideWidth' を全 guide に適用し、 縦スタックゆえ最大幅を予約。
      --   gap (panel→凡例 = 2*ggHalfLine) は renderLegendRight の x0 オフセットと一致。
      --   フォントは既定 (item=base×0.8 / title=base)。 override 無し時 render と一致 (旧固定80は
      --   フォント完全無視だったので後退なし)。
      legItemF  = legendBaseSize * 0.8
      legTitleF = legendBaseSize
      shapeCats scr = case resolveCol r scr of
        Just (TxtData v) -> orderedCats (V.toList v)
        Just (NumData v) -> orderedCats (map numToText (V.toList v))
        _                -> []
      -- ★ 連続 colorbar の予約ラベルは renderGuideBlock (ColorByContinuous) の描画と
      --   同一 = Wilkinson extended breaks の範囲内 nice 値。 旧実装は生 min/mid/max を
      --   使っており、 LCG 等の長大桁データで予約幅 >> 実描画幅 になり凡例が無駄に広かった
      --   (予約と描画は同一ラベル源にする不変条件・本 module 冒頭コメント参照)。
      contColorLabels cr = case resolveNum r cr of
        Just nums | not (V.null nums) ->
          let vMin = V.minimum nums; vMax = V.maximum nums
          in case filter (\b -> b >= vMin && b <= vMax) (extendedBreaks 5 vMin vMax) of
               [] -> [numToText vMin, numToText vMax]
               bs -> map numToText bs
        _ -> []
      guideWidth g = case g of
        ColorGuide (ColorByCol _)         ->
          legendGuideWidth legItemF legTitleF (effectiveLegendTitle spec) (allColorCategories r (vsLayers spec))
        ColorGuide (ColorByContinuous cr) ->
          legendGuideWidth legItemF legTitleF (effectiveLegendTitle spec) (contColorLabels cr)
        ColorGuide (ColorStatic _)        -> 0
        CountBarGuide lo hi               ->
          legendGuideWidth legItemF legTitleF "count"
            (map numToText (filter (\b -> b >= lo && b <= hi) (extendedBreaks 5 lo hi)))
        ShapeGuide scr                    ->
          -- 見出しは render と同じく sentinel を空に潰してから幅を見積る。
          let nm = colRefName scr
              t  = if nm == "<inline-num>" || nm == "<inline-txt>" then "" else nm
          in legendGuideWidth legItemF legTitleF t (shapeCats scr)
      legendGuidesW = maximum (0 : map guideWidth (collectGuides r spec))
      -- Phase 32 (re-apply): LegendRightCenter も右域に同じ幅を予約 (縦位置のみ違う)。
      legendW = if legendPos == LegendRight || legendPos == LegendRightCenter
                  then 2 * ggHalfLine + legendGuidesW else 0
      legendH = if legendPos == LegendBottom then 50 + fromIntegral (legNrow - 1) * 16 else 0
      rM = sc * ggHalfLine + rightAxisW + legendW
      -- Phase 8 A2 Step2 (design §A-4): パネル本体は可用域 (margin を除いた残り) を取る。
      -- aspect 未指定 (Nothing) = ggplot Coord$aspect=NULL と同じく可用域を埋める。
      -- aspect 指定 (Just a, a>0) = 高/幅比 a を保つ最大 panel を可用域内に取り中央寄せ
      -- (coord_fixed)。 panelW = min availW (availH/a)、 panelH = panelW*a。
      -- Phase 8 C: sc 撤廃で固定 pt margin になったため、 極小 viewport で panel が負/潰れ
      -- ないよう下限を設ける (ggplot も極小時は軸が支配的になるが panel は非負)。
      -- Phase 8 C (gtable §E-2): パネル本体を solveTracks で算出。 横 = [Fixed lM, Null 1,
      -- Fixed rM]、 縦 = [Fixed tM, Null 1, Fixed bM] の中央 Null トラックがパネル。 結果は
      -- 従来の (lM,tM,w-lM-rM,h-tM-bM) と同値 (= 単一プロットは Null 1 個なので)。
      midTrack solve = case solve of (_ : m : _) -> m; _ -> (0, 0)
      (panelX0, availW) = let (s, l) = midTrack (solveTracks 0 w [Fixed lM, Null 1, Fixed rM]) in (s, max 10 l)
      (panelY0, availH) = let (s, l) = midTrack (solveTracks 0 h [Fixed tM, Null 1, Fixed bM]) in (s, max 10 l)
      area = case getLast (vsAspect spec) of
        Just a | a > 0 ->
          let pw = min availW (availH / a)
              ph = pw * a
          in Rect (panelX0 + (availW - pw) / 2) (panelY0 + (availH - ph) / 2) pw ph
        _ -> Rect panelX0 panelY0 availW availH
      -- Phase 8 B22: 左軸 / 右軸で layer を分割。 x は全 layer 共有、 y は各軸の
      -- layer のみから domain を作る (= 右軸が無ければ leftLayers == 全 layer なので
      -- 従来挙動と完全一致)。
      leftLayers  = filter (\l -> getLast (lyYAxisSide l) /= Just YAxisRight) (vsLayers spec)
      rightLayers = filter (\l -> getLast (lyYAxisSide l) == Just YAxisRight) (vsLayers spec)
      (xs, _)    = collectXY r spec
      (_,  ys)   = collectXY r spec { vsLayers = leftLayers }
      (_,  ysR)  = collectXY r spec { vsLayers = rightLayers }
      (xLo, xHi) = extentsOrDefault xs
      (yLo, yHi) = extentsOrDefault ys
      kindX = axisKindOf (vsXAxis spec)
      kindY = axisKindOf (vsYAxis spec)
      mkScale kind dLo dHi rLo rHi = case kind of
        AxisLinear -> LinearScale dLo dHi rLo rHi
        AxisLog    -> LogScale    dLo dHi rLo rHi
        AxisSqrt   -> SqrtScale   dLo dHi rLo rHi
        AxisTime   -> TimeScale   dLo dHi rLo rHi
      -- Phase 8 C (§5 G3 + sqrt/time fix): tick は **データ範囲** (dLo,dHi) で計算し、
      -- expansion 後の範囲 (pLo,pHi) で censor (= ggplot の breaks→censor)。 linear だけ
      -- でなく log/sqrt/time も同方式に統一 (time の粒度バグ = padded span/5 が 1 日を
      -- 飛び越え 1 週になり tick 1 個に潰れる問題を解消)。
      mkTicks kind dLo dHi pLo pHi =
        let ferr   = abs (pHi - pLo) * 1e-9
            censor = filter (\t -> t >= min pLo pHi - ferr && t <= max pLo pHi + ferr)
        in case kind of
             AxisLinear -> censor (extendedBreaks 5 dLo dHi)
             AxisLog    -> censor (niceTicksLog   5 dLo dHi)
             AxisSqrt   -> censor (niceTicksSqrt  5 dLo dHi)
             AxisTime   -> censor (niceTimeTicks  5 dLo dHi)
      -- categorical x labels (= ColTxt の distinct 値、 layer 横断)
      -- Phase 36 B1b: distribution mark (box/violin/strip/swarm/raincloud) は群列を
      --   encX が無くても colorBy 列から取る ('distGroupRef')。 scatter 等は従来どおり
      --   lyEncX のみ (colorBy をカテゴリ x にしない)。
      distXAcc l = case getFirst (lyKind l) of
        Just k | k `elem` [MBox, MViolin, MStrip, MSwarm, MRaincloud, MRidge]
               -> Last (distGroupRef l)
        _      -> lyEncX l
      xCatLabelsRaw = collectCategoricalLabels distXAcc r spec (getLast (vsXDiscreteLimits spec))
      -- ★ Phase 36 D3: distCols (= 合成 Layer が複数の値列にまたがる) のとき x カテゴリは
      --   各 lane の値列名 (= 列名 slot)。 単一列 (raincloud) は対象外 (従来どおり)。
      distColsLayers = filter (\l -> length (compositeLanes l) > 1) (vsLayers spec)
      isDistCols = not (null distColsLayers)
      distColLabels = nub [ colRefName c | l <- distColsLayers, c <- compositeLanes l ]
      -- Phase 7 A6: waterfall は末尾に合計 (Total) バーを足すため x category を 1 つ拡張。
      hasWaterfallLayer = any (\l -> getFirst (lyKind l) == Just MWaterfall) (vsLayers spec)
      xCatLabels
        | isDistCols = distColLabels
        | hasWaterfallLayer && not (null xCatLabelsRaw) = xCatLabelsRaw ++ [T.pack "Total"]
        | otherwise  = xCatLabelsRaw
      -- Phase 8 B23-fix: forest plot は先頭の研究を上に置くのが慣例 (= PS renderForest
      -- と同方向)。 categorical y は position 0 が下端なので、 forest のときだけラベルを
      -- 反転し position 0(下)= 末尾、 position n-1(上)= 先頭にする。 renderForest も
      -- row i を position (n-1-i) に置く (両者で整合)。
      hasForestLayer = any (\l -> getFirst (lyKind l) == Just MForest) (vsLayers spec)
      -- ★ Phase 36 B1c: ridge は群 baseline から density 山を伸ばすので、 最上段の山が
      --   はみ出さないよう群カテゴリ軸 (ridge は coord_flip 済なので encX = 群) を成長方向へ
      --   1 スロット分 expand する (= ggridges の scale_y_discrete expand 相当)。
      hasRidgeLayer = any (\l -> getFirst (lyKind l) == Just MRidge) (vsLayers spec)
      ridgeHeadroom = if hasRidgeLayer then 1.0 else 0.0
      yCatLabelsRaw = collectCategoricalLabels lyEncY r spec (getLast (vsYDiscreteLimits spec))
      yCatLabels = if hasForestLayer then reverse yCatLabelsRaw else yCatLabelsRaw
      -- categorical の場合は range を [-0.5, n-0.5] に上書き。
      -- numeric padding は MarkKind 別 (Phase 7 A2b):
      --   0-base chart (bar / histogram / density / waterfall) のみ下端を 0 に固定し、
      --   上端のみ 5% pad (= ggplot2 既定 expansion mult=0.05)。 それ以外
      --   (scatter / line / box / violin 等) は値が 0 でも symmetric 8% pad で軸接触を防ぐ。
      -- 旧実装は `lo == 0` を一律 0-base 判定にしていたため、 y に 0 を含む scatter 等が
      -- 下軸に貼り付く副作用があった (= 値ベースの heuristic → MarkKind ベースへ)。
      layerKinds   = [ k | l <- vsLayers spec, Just k <- [getFirst (lyKind l)] ]
      hasYBaseline = any (`elem` [MBar, MHistogram, MDensity, MWaterfall]) layerKinds
      hasXBaseline = any (`elem` [MAutocorr, MEss]) layerKinds
      hasHistogram = MHistogram `elem` layerKinds
      -- Phase 8 B3: funnel plot は y=SE。 SE=0 (最精密) を上端・SE 増加で下端へ置くのが
      -- 慣例 (metafor::funnel)。 通常 y は反転 (lo→下/hi→上) だが、 funnel は y domain を
      -- [0, maxSE+pad] とし range を非反転 (0→上端 rY, max→下端) にして上下を正す。
      hasFunnelLayer = MFunnel `elem` layerKinds
      funnelYHi = let p = (yHi - yLo) * 0.05 in yHi + p
      -- Phase 8 A2 Step4a (design §A-7, G1): 連続軸 expansion = ggplot 既定 mult=0.05
      -- (両側 5%)。 旧 8% から変更。 baseline (bar/hist/density/waterfall, lo==0) は
      -- ggplot bar 既定 mult=c(0,0.05) と同じく下端 0 固定 + 上端のみ 5% (従来通り)。
      paddedRange baseline lo hi
        | hi <= lo            = (lo - 0.5, hi + 0.5)
        | baseline && lo == 0 = (0, hi + (hi - lo) * 0.05)
        | otherwise           = let p = (hi - lo) * 0.05 in (lo - p, hi + p)
      -- histogram の x 軸は ggplot 流に 5% expansion (= bin の外余白を控えめに)。
      paddedRangeX lo hi
        | hi <= lo  = (lo - 0.5, hi + 0.5)
        | otherwise = let p = (hi - lo) * 0.05 in (lo - p, hi + p)
      -- Phase 8 A2 Step4c 段階2 (design §A-7, G2): 離散軸 expansion = ggplot 既定
      -- expansion(add=0.6)。 位置 0..n-1 の両端に ±0.6 → [-0.6, (n-1)+0.6] = [-0.6, n-0.4]。
      -- 旧 ±0.5。 全 categorical geom はスケール経由 (Step4c 段階1) なので自動追従する。
      -- Phase 8 C (sqrt/log fix): sqrt/log 軸は対称 padding が domain 下端を負
      -- (sqrt) / 非正 (log) にすると scaleApply が中央 fallback して全 tick が潰れる。
      -- transformed space 相当に下端をクランプ (sqrt: ≥0、 log: >0 = データ下端の 0.9 倍)。
      clampDomKind kind dataLo (lo, hi) = case kind of
        AxisSqrt -> (max 0 lo, hi)
        AxisLog  -> (if lo <= 0
                       then (if dataLo > 0 then dataLo * 0.9 else abs hi * 1e-6)
                       else lo, hi)
        _        -> (lo, hi)
      -- Phase 11 A7-a: coord_cartesian(xlim,ylim) = データ非破棄 zoom。 numeric 軸
      --   (非 categorical・y は非 funnel) のときだけ scale domain を指定範囲に上書き。
      --   expand=FALSE 相当 (= 余白を足さず厳密に [lo,hi])。 stat は全データから計算済。
      coordXLim = getLast (vsCoordXLim spec)
      coordYLim = getLast (vsCoordYLim spec)
      -- ★ Phase 41: crossbar の箱 (中心 ±halfWidth の幅を x 方向に持つ) が連続 x で軸外へ
      --   はみ出すのを防ぐため、 ドメインを半幅分広げる (categorical は add=0.6 で既に収まる)。
      --   半幅 = 0.5 × markWidth(既定0.9) × resolution(x)。 errorbar の横 cap は小さく ggplot も
      --   clip 任せ (scale を訓練しない) なので対象外 = crossbar のみ。
      xResData = resolutionOf [ v | v <- V.toList xs, not (isNaN v), not (isInfinite v) ]
      widthGeomHalfData =
        let relevant l = getFirst (lyKind l) == Just MCrossbar
            halfOf l = 0.5 * maybe 0.9 id (getLast (lyMarkWidth l)) * xResData
        in maximum (0 : [ halfOf l | l <- vsLayers spec, relevant l ])
      -- 箱の外縁 (xLo-half, xHi+half) を新たな「データ範囲」とみなし、 そこに連続軸既定の
      --   5% expansion を足す。 → 端の箱と軸の間に離散軸 (add=0.6) と同様の余白が出る
      --   (箱がちょうど軸線に接触する窮屈さを解消)。
      widenForWidthGeom (lo, hi)
        | widthGeomHalfData <= 0 = (lo, hi)
        | otherwise =
            let bLo = xLo - widthGeomHalfData
                bHi = xHi + widthGeomHalfData
                p   = (bHi - bLo) * 0.05
            in (min lo (bLo - p), max hi (bHi + p))
      (xLo', xHi') = case coordXLim of
        Just (a, b) | null xCatLabels -> (a, b)
        _ -> if null xCatLabels
               then clampDomKind kindX xLo $ widenForWidthGeom
                      (if hasHistogram
                         then paddedRangeX xLo xHi
                         else paddedRange hasXBaseline xLo xHi)
               else (-0.6, fromIntegral (length xCatLabels) - 0.4 + ridgeHeadroom)
      (yLo', yHi') = case coordYLim of
        Just (a, b) | null yCatLabels && not hasFunnelLayer -> (a, b)
        _ -> if null yCatLabels
               then clampDomKind kindY yLo (paddedRange hasYBaseline yLo yHi)
               else (-0.6, fromIntegral (length yCatLabels) - 0.4)
      sx = mkScale kindX xLo' xHi' (rX area)            (rX area + rW area)
      -- y は通常反転 (lo→下端/hi→上端)。 funnel のみ SE=0 を上端に出すため非反転 (0→上端)。
      sy | hasFunnelLayer = mkScale kindY 0 funnelYHi (rY area) (rY area + rH area)
         | otherwise      = mkScale kindY yLo' yHi' (rY area + rH area) (rY area)
      -- Phase 9 C: coord_flip 用 scale。 domain は sx/sy と同一 (= categorical/baseline 継承)、
      --   range のみ縦横入替。 sxF: データ x → 縦 px (Y と同じ反転で小値が下)。
      --   syF: データ y → 横 px。 funnel は flip 対象外なので非反転特例は写さない。
      sxF = mkScale kindX xLo' xHi' (rY area + rH area) (rY area)
      syF = mkScale kindY yLo' yHi' (rX area)           (rX area + rW area)
      -- Phase 11 A4-a: 軸反転 (scale_x_reverse / scale_y_reverse)。 range を入替えるだけ。
      --   データ軸基準なので Cartesian/flip の両 scale に同じ向きで適用 (coord と独立)。
      revX = getLast (vsReverseX spec) == Just True
      revY = getLast (vsReverseY spec) == Just True
      applyRevX s = if revX then revScale s else s
      applyRevY s = if revY then revScale s else s
      -- Phase 8 B22: 右 Y 軸 scale。 右軸 layer の y 値 (ysR) だけから独立 domain。
      -- PS (findings §2 で正) に合わせ padding は付けない (extentsOrDefault そのまま)。
      kindYR = axisKindOf (vsYAxisRight spec)
      (yLoR, yHiR) = extentsOrDefault ysR
      syR = if hasRightY
              then Just (mkScale kindYR yLoR yHiR (rY area + rH area) (rY area))
              else Nothing
      yTicksR = if hasRightY then mkTicks kindYR yLoR yHiR yLoR yHiR else []
      -- 単一群の distribution mark (= 群列 (encX または colorBy) 無し box/violin/strip/
      -- swarm/raincloud layer のみ) は x tick を抑制。 Phase 36 B1b/B1c: colorBy 単体でも
      -- 群分けされる ('distGroupRef') ので、 その場合は単一群扱いにせず x ラベル (群名) を出す。
      -- ★ Phase 36 D3: distCols は lane 名を x tick に出すので単一群抑制の対象外。
      isSingleGroupBoxOnly = not (null (vsLayers spec)) && not isDistCols &&
        all (\l -> case getFirst (lyKind l) of
                     Just k | k `elem` [MBox, MViolin, MStrip, MSwarm, MRaincloud] ->
                       case distGroupRef l of
                         Nothing -> True
                         Just _  -> False
                     _ -> False) (vsLayers spec)
      -- ★ Phase 11 A4-d: 明示 break/label を (val,label) 対で censor し values/labels に分離。
      --   labels が空 (= breaks のみ指定) なら override label は [] にして render の formatTick に委ねる。
      explicitTicks pLo pHi vals labs =
        let ferr   = abs (pHi - pLo) * 1e-9
            keep t = t >= min pLo pHi - ferr && t <= max pLo pHi + ferr
        in if null labs
             then (filter keep vals, [])
             else let kept = filter (keep . fst)
                               (zip vals (labs ++ repeat (T.pack "")))
                  in (map fst kept, map snd kept)
      explicitXVals = axTickValsOf (vsXAxis spec)
      explicitXLabs = axTickLabelsOf (vsXAxis spec)
      explicitYVals = axTickValsOf (vsYAxis spec)
      explicitYLabs = axTickLabelsOf (vsYAxis spec)
      (xTicksExp, xTickLabsExp) = explicitTicks xLo' xHi' explicitXVals explicitXLabs
      (yTicksExp, yTickLabsExp) = explicitTicks yLo' yHi' explicitYVals explicitYLabs
      useExplicitX = not (null explicitXVals) && null xCatLabels && not isSingleGroupBoxOnly
      useExplicitY = not (null explicitYVals) && null yCatLabels && not hasFunnelLayer
      -- Phase 11 A7-a: zoom 時は break 生成も zoom 範囲で行う (= データ範囲のまま
      --   生成して censor すると視野内 tick が疎になるため)。 未指定は従来 (データ範囲)。
      (xTickLo, xTickHi) = maybe (xLo, xHi) id coordXLim
      (yTickLo, yTickHi) = maybe (yLo, yHi) id coordYLim
      xTicks
        | isSingleGroupBoxOnly = []
        | not (null xCatLabels) = map fromIntegral [0 .. length xCatLabels - 1]
        | useExplicitX          = xTicksExp
        | otherwise             = mkTicks kindX xTickLo xTickHi xLo' xHi'
      yTicks
        | hasFunnelLayer  = mkTicks kindY 0 yHi 0 funnelYHi   -- 0..maxSE (上→下)
        | not (null yCatLabels) = map fromIntegral [0 .. length yCatLabels - 1]
        | useExplicitY    = yTicksExp
        | otherwise       = mkTicks kindY yTickLo yTickHi yLo' yHi'
      xTickLabsOv = if useExplicitX then xTickLabsExp else []
      yTickLabsOv = if useExplicitY then yTickLabsExp else []
      -- P17: spec.palette > theme 既定 series (Phase 9 A-1: ブランドテーマは専用 series、
      -- ggplot 系 preset は従来の hggMain)。 palette 明示指定があればそれが最優先。
      themeDefaultPal = themeSeriesPalette (maybe ThemeDefault id (getLast (vsTheme spec)))
      catPalRaw = maybe themeDefaultPal id (getLast (vsPalette spec))
      -- Phase 7 A6 / Phase 28: ggplot hue sentinel は群数 n で展開 (= hue_pal()(n))。
      -- 群数は (1) categorical color/fill aesthetic の水準数を最優先、 (2) 無ければ x
      -- カテゴリ数 (violin/box/strip 等)、 (3) どちらも無ければ 8。 ★旧実装は連続 x +
      -- 色分け (= R4DS Ch1 の散布図) で x カテゴリが空 → 常に 8 色版になり、 群数 3 でも
      -- 8 色パレットの飛び石を拾って R4DS と色が食い違っていた。
      colorCatN = length (orderedCats (concat
        [ V.toList v
        | l <- vsLayers spec
        , Just (ColorByCol cr) <- [getLast (lyColor l)]
        , Just (TxtData v) <- [resolveCol r cr] ]))
      catPalN | colorCatN > 0         = colorCatN
              | not (null xCatLabels) = length xCatLabels
              | otherwise             = 8
      catPal = if catPalRaw == ["__ggplot_hue__"]
                 then ggplotHue catPalN
                 else catPalRaw
      viridis5Default = ["#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725"]
      contPal = maybe viridis5Default id (getLast (vsContinuousPal spec))
      -- ★ Phase 11 A4-e: spec の色/サイズ scale を Layout へ (renderer が参照)。
      colorManual = maybe [] id (getLast (vsColorManual spec))
      colorGradient2 = getLast (vsColorGradient2 spec)
      -- ★ Phase 34 A3: scale_size 範囲は **直径** pt (size=直径 統一)。既定 (6,20)pt
      -- → 半径 3..10pt (= 旧 radius 範囲 (3,10) と同値・sizeBy 見た目を保存)。
      sizeRange = maybe (6, 20) id (getLast (vsSizeRange spec))
  in Layout
       { lpViewport = vp
       , lpPlotArea = area
       , lpXScale   = applyRevX sx
       , lpYScale   = applyRevY sy
       , lpXScaleFlipped = applyRevX sxF
       , lpYScaleFlipped = applyRevY syF
       , lpCoord    = coordOf spec
       , lpYScaleRight = fmap applyRevY syR
       , lpXTicks   = xTicks
       , lpYTicks   = yTicks
       , lpYTicksRight = yTicksR
       , lpCategoricalPalette = catPal
       , lpContinuousPalette  = contPal
       , lpColorManual = colorManual
       , lpColorGradient2 = colorGradient2
       , lpSizeRange = sizeRange
       , lpXCategoryLabels = xCatLabels
       , lpYCategoryLabels = yCatLabels
       , lpXTickLabels = xTickLabsOv
       , lpYTickLabels = yTickLabsOv
       , lpHistDomain = histRawDomain r (vsLayers spec)
       , lpMarginScale = sc
       , lpMarginTop    = tM
       , lpMarginLeft   = lM
       , lpMarginBottom = bM
       }

-- | Phase 8 C (ggplot 準拠): margin 縮小係数を撤廃 (常に 1)。 ggplot は文字・余白を
-- 固定 pt で扱い viewport サイズで縮めない (パネルが残りを埋めるだけ)。 旧実装は小
-- viewport で sc<1 に縮小していたが、 grid は軸帯確保 (renderSubplots) で対応し、 単一
-- 小 viewport (inset) も固定 pt で ggplot と同挙動にする。 panel が潰れないよう
-- computeLayout 側で availW/availH に下限を設ける。 シグネチャは互換のため温存。
ggMarginScale :: Double -> Double -> Double
ggMarginScale _ _ = 1

-- ===========================================================================
-- Phase 8 C (gtable §E): 汎用 1 次元トラック割付
-- ===========================================================================

-- | gtable のトラック (行 or 列) サイズ種別。 ggplot の grid::unit に対応:
--   Fixed v = 固定 pt (= 軸テキスト/タイトル/strip/plot.margin の grob 実寸)、
--   Null  w = 伸縮トラック (= unit(w,"null")、 残りスペースを重み比で分配 = パネル本体)。
data Track = Fixed !Double | Null !Double
  deriving (Show, Eq)

-- | Phase 8 C (§E-1): 1 次元トラック割付。 利用可能長 avail から Fixed 合計を先取りし、
-- 残りを Null トラックに重み比で配分する (= ggplot gtable の「固定先取り → null 残り均等」)。
-- 残りが負なら Null=0 (= パネルが潰れる、 ggplot と同挙動)。 パネル間 spacing は呼び出し側が
-- Fixed トラックとして明示挿入する。 戻り = 各トラックの (start, length) (start は absolute)。
solveTracks :: Double -> Double -> [Track] -> [(Double, Double)]
solveTracks origin avail tracks =
  let fixedSum  = sum [ v | Fixed v <- tracks ]
      weightSum = sum [ w | Null  w <- tracks ]
      remainder = max 0 (avail - fixedSum)
      per       = if weightSum <= 0 then 0 else remainder / weightSum
      sizeOf (Fixed v) = v
      sizeOf (Null  w) = per * w
      go _   []       = []
      go pos (t : ts) = let sz = sizeOf t in (pos, sz) : go (pos + sz) ts
  in go origin tracks

-- | Phase 8 A2 Step1 (design §D): ggplot half_line マージン定数 (pt, sc 適用前)。
-- ★ Phase 33 B4: layout が純 pt 空間になり、これらは ggplot 由来の pt 値 (half_line=
-- base_size/2=5.5pt) そのものとして正しく pt 意味になる (値は不変・k は backend)。
-- computeLayout の margin 計算と Render の描画オフセットで共有 (単一情報源)。
ggHalfLine, ggTickLen, ggAxTextMar, ggAxTitleMar :: Double
ggHalfLine   = 5.5    -- plot.margin 四辺 + title 下 margin
ggTickLen    = 2.75   -- Phase 8 C: axis.ticks.length = half_line/2 (ggplot 忠実、 旧 5)
ggAxTextMar  = 2.2    -- axis.text margin (0.8*halfLine/2)
ggAxTitleMar = 2.75   -- axis.title margin (halfLine/2)

-- ===========================================================================
-- 凡例メトリクス (Phase 35 で導入・Phase 38 で Layout へ集約)
--   ★Layout (予約) と Render (描画) の単一情報源にするため最下層へ置く。
--   Render/Layer は本モジュールから import する (旧: Render/Layer 内ローカル定義)。
-- ===========================================================================

-- | 凡例のベースフォント (pt)。 ggplot @base_size@ = 2 × half_line = 11pt。
legendBaseSize :: Double
legendBaseSize = 2 * ggHalfLine

-- | 凡例キーの 1 辺 (pt) = ggplot @legend.key.size = unit(1.2,"lines")@。
--   ★R gtable トレース実測 = 17.34pt (base 11pt 時)。 grid の "lines" は行高 (= 1.2 ×
--   base × lineheight) なので 1.2×base(=13.2) ではなくこの値。 = 1.2 × base × 1.3133。
legendKeyW :: Double
legendKeyW = 1.2 * legendBaseSize * 1.3133

-- | 凡例キーの行ピッチ = keyW (= ggplot gtable のキー間 spacing 行 = 0pt = キーセル隣接)。
legendKeyPitch :: Double
legendKeyPitch = legendKeyW

-- ---------------------------------------------------------------------------
-- Phase 38: 凡例幅を「ラベル内容」に応じて算出する純関数。
--   Layout の legendW (右予約) と Render の描画幅を**同一式**で駆動して食い違いを無くす。
--   テキスト幅は字種別 advance 近似 ('charWidthEm'・全角=1.0em / Latin は字種別実測較正)。
--   ★ggplot は実フォント advance で測るが、 backend 非依存・HS=PS byte parity を保つため
--   本ライブラリは決定論的な等幅近似で一貫させる (全角を 1.0em にして日本語ラベルの
--   過小評価=はみ出しを防ぐ・大小/はみ出し挙動を ggplot と整合)。
-- ---------------------------------------------------------------------------

-- | East Asian Width が全角 (F=Fullwidth / W=Wide) の文字か。 CJK 統合漢字・かな・
--   全角記号・ハングル等を 1.0em 扱いにする。 範囲は Unicode EAW (UAX #11) の W/F に対応する
--   代表ブロックを網羅 (厳密 table でなく実用的な近似・凡例幅にのみ使用)。
isWideChar :: Char -> Bool
isWideChar c =
  let o = fromEnum c
  in (o >= 0x1100  && o <= 0x115F)   -- Hangul Jamo
  || (o >= 0x2E80  && o <= 0x303E)   -- CJK Radicals .. Kangxi .. CJK Symbols (一部)
  || (o >= 0x3041  && o <= 0x33FF)   -- Hiragana/Katakana/CJK 記号/互換等
  || (o >= 0x3400  && o <= 0x4DBF)   -- CJK Ext A
  || (o >= 0x4E00  && o <= 0x9FFF)   -- CJK 統合漢字
  || (o >= 0xA000  && o <= 0xA4CF)   -- Yi
  || (o >= 0xAC00  && o <= 0xD7A3)   -- Hangul 音節
  || (o >= 0xF900  && o <= 0xFAFF)   -- CJK 互換漢字
  || (o >= 0xFE30  && o <= 0xFE4F)   -- CJK 互換形
  || (o >= 0xFF00  && o <= 0xFF60)   -- 全角 ASCII 変種
  || (o >= 0xFFE0  && o <= 0xFFE6)   -- 全角記号
  || (o >= 0x1F300 && o <= 0x1FAFF)  -- 絵文字 (W)
  || (o >= 0x20000 && o <= 0x3FFFD)  -- CJK Ext B 以降

-- | 1 文字の advance を em 単位で近似。 ★既定 sans (DejaVu) の実 advance を計測して
--   字種別にバケット化 (2026-06-23・rsvg trim 実測。 例 i/l≈0.25・a/e≈0.56・M/W≈0.9)。
--   旧 flat 0.6 は細字主体ラベル (小文字+ハイフン等) で平均 ~0.49em/字を 0.6 と過大予約し
--   右余白を生んでいた。 値は実測平均をやや上回る安全側に丸め 「切れない方向」 を維持。
--   全角は 'isWideChar' で 1.0em。 ★この表は HS=PS で完全一致させること (PS canvas も同値)。
charWidthEm :: Char -> Double
charWidthEm c
  | isWideChar c                              = 1.0
  | c `elem` ("iIl|.,;:'`!()[]{} " :: String) = 0.30  -- 細字・記号・空白
  | c `elem` ("jftr-/\\" :: String)           = 0.42  -- やや細
  | c `elem` ("mwMW@" :: String)              = 0.92  -- 幅広
  | c >= 'A' && c <= 'Z'                       = 0.70  -- 大文字 (M/W は上で処理済)
  | otherwise                                 = 0.58  -- 小文字・数字・その他

-- | 文字列の幅を em 単位で見積もる (字種別 'charWidthEm' の総和)。 実 pt 幅 = fontSize × この値。
textWidthEm :: Text -> Double
textWidthEm = T.foldl' (\acc ch -> acc + charWidthEm ch) 0

-- | DAG node ラベルのフォントサイズ (pt)。 layout (Sugiyama の size-aware 横幅
--   見積り) と render ('nodeExtent') で共有する単一定義。 旧 Render.EdgeRoute から移管。
dagLabelFs :: Double
dagLabelFs = 11

-- | DAG node の **radius 非依存** な横半幅 (px)。 = 'nodeExtent' の rx から
--   @max baseR@ の floor を除いた本体 (ラベル名 / 分布 sublabel 幅に由来)。
--
--   Phase 39 P8 A4-2: layout の size-aware simplex (Sugiyama 'auxSepOf' /
--   'clusterAuxEdges') と render の 'nodeExtent' が **同一式**を共有することで、
--   simplex が確保する node 間隔と描画箱の幅を整合させる (= 兄弟 plate の box
--   重なりを根治)。 radius は layout 時に未知 (= render-time の lySize) ゆえ
--   floor 部分は render 側 ('nodeExtent') で適用する。
dagNodeBaseHalfWidth :: DAGNode -> Double
dagNodeBaseHalfWidth n =
  let showDist = case dnKind n of
        NodeDeterministic -> False
        _                 -> maybe False (const True) (dnDist n)
      nameEm = textWidthEm (dnLabel n)
      distEm = case dnDist n of Just d | showDist -> textWidthEm d; _ -> 0
      maxEm  = max 0.5 (max nameEm distEm)
  in dagLabelFs * maxEm / 2 + 8

-- | 単一 guide (右凡例・縦1列) の必要幅 (pt)。 renderGuideBlock の描画式に厳密一致:
--   列幅 = (key 1辺) + (key→label gap = half_line/2) + (最長ラベル幅) + (右パディング = half_line)。
--   タイトルがそれより広ければタイトル幅。 引数: item フォント pt / title フォント pt /
--   タイトル文字列 / ラベル群。
--   ★「最長」は文字数でなく 'textWidthEm' 最大 (全角混在で逆転し得るため幅で選ぶ)。
legendGuideWidth :: Double -> Double -> Text -> [Text] -> Double
legendGuideWidth fItem fTitle title labels = max titleW colW
  where
    maxLabelEm = maximum (0 : map textWidthEm labels)
    colW       = legendKeyW + ggHalfLine / 2 + fItem * maxLabelEm + ggHalfLine
    titleW     = fTitle * textWidthEm title

-- ===========================================================================
-- 凡例ラベル収集 (Phase 35 で導入・Phase 38 で Render/Layer から Layout へ集約)。
--   ★Layout の legendW 予約と Render の renderGuideBlock 描画が**同一関数**でラベル文字列を
--   得るための単一情報源。 別実装だとラベル文字列がズレ予約幅≠描画幅になる。
-- ===========================================================================

-- | 数値 → 表示文字列。 浮動小数点アーチファクト (0.1+0.2=0.300…04 等) を 12 桁 round で
--   回避。 整数なら trailing zero / decimal point を除去。 (旧 Render.Common.numToText)
numToText :: Double -> Text
numToText v =
  let rounded = fromIntegral (round (v * 1e12) :: Integer) / 1e12
      s = if rounded == fromIntegral (truncate rounded :: Integer)
            then show (truncate rounded :: Integer)
            else showFFloat Nothing rounded ""
  in case T.pack s of
       t -> case T.stripSuffix ".0" t of
              Just t' -> t'
              Nothing -> case T.stripSuffix "." t of
                Just t' -> t'
                Nothing -> t

-- | 順序保存 nub (初出順)。 glyph 色 ('colorVector' の nub) / PS (Array.nub) と揃える。
nubKeep :: [Text] -> [Text]
nubKeep = nub

-- | 色 aesthetic を持つ最初のレイヤの ColorEnc (categorical / continuous)。
findColorEnc :: [Layer] -> Maybe ColorEnc
findColorEnc ls = case [ ce | l <- ls
                            , Just ce <- [getLast (lyColor l)]
                            , isColorMap ce ] of
  (ce : _) -> Just ce
  []       -> Nothing
  where
    isColorMap (ColorByCol _)        = True
    isColorMap (ColorByContinuous _) = True
    isColorMap _                     = False

-- | 明示凡例タイトル (vsLegendTitle = scale name / labs(color=))。 未指定なら ""。
effectiveLegendTitle :: VisualSpec -> Text
effectiveLegendTitle spec = maybe "" id (getLast (vsLegendTitle spec))

-- | 全 ColorByCol レイヤのカテゴリを順序保存で union (= 凡例 swatch / glyph 色の正本)。
--   明示 'colorCats' があればそれを先頭に、 無ければデータ水準を 'orderedCats' 順で。
allColorCategories :: Resolver -> [Layer] -> [Text]
allColorCategories r ls =
  let dataCats = orderedCats $ concat
        [ case resolveCol r cr of
            Just (TxtData v) -> V.toList v
            Just (NumData v) -> V.toList (V.map numToText v)
            Nothing          -> []
        | l <- ls
        , Just (ColorByCol cr) <- [getLast (lyColor l)] ]
      explicit = nubKeep (concatMap lyColorCats ls)
  in if null explicit
       then dataCats
       else explicit ++ filter (`notElem` explicit) dataCats

-- | 凡例 guide (色 / 形)。 描画 (renderGuideBlock) と予約 (legendW) が共有。
data LegendGuide
  = ColorGuide !ColorEnc      -- 色 guide (categorical / continuous)
  | ShapeGuide !ColRef        -- 形 guide (色とは別列・または色無しのとき)
  | CountBarGuide !Double !Double  -- ★ Phase 40: 件数 colorbar (lo,hi)。 hexbin/bin2d-count 用
                                   -- (列でなく集計値ゆえ ColorByContinuous と別。 ラベル = "count")

-- | spec から guide を ggplot 順 (color → shape) で収集。 形が色と同列なら統合し形 guide なし。
collectGuides :: Resolver -> VisualSpec -> [LegendGuide]
collectGuides r spec =
  let mEnc     = findColorEnc (vsLayers spec)
      colorG   = maybe [] (\e -> [ColorGuide e]) mEnc
      colorCol = case mEnc of
        Just (ColorByCol cr) -> Just (colRefName cr)
        _                    -> Nothing
      shapeG   = case [ sc | l <- vsLayers spec, Just sc <- [getLast (lyShapeBy l)] ] of
        (sc : _) | Just (colRefName sc) /= colorCol -> [ShapeGuide sc]
        _                                           -> []
      -- ★ Phase 40: 色 enc が無い hexbin (件数) は count colorbar を出す。
      countG = case (mEnc, hexbinCountDomain r spec) of
        (Nothing, Just (lo, hi)) -> [CountBarGuide lo hi]
        _                        -> []
  in colorG <> countG <> shapeG

-- | Phase 40: spec 中の hexbin layer の件数域 (min,max)。 colorbar guide + needsLegend が使う。
--   render (renderHexbin) と同じ 'hexbinLayerCells' で計算するので域が一致する。
hexbinCountDomain :: Resolver -> VisualSpec -> Maybe (Double, Double)
hexbinCountDomain r spec =
  case [ l | l <- vsLayers spec, getFirst (lyKind l) == Just MHexbin ] of
    (l : _) -> case map hexCount (hexbinLayerCells r l) of
      [] -> Nothing
      cs -> Just (fromIntegral (minimum cs), fromIntegral (maximum cs))
    _ -> Nothing

-- | spec の font slot から size を取り出す (未指定なら default)。
fontSizeOf :: Last FontSpec -> Double -> Double
fontSizeOf lf def = case getLast lf of
  Just fs -> maybe def id (getLast (fsSize fs))
  Nothing -> def

-- | Phase 9 A-5 (PS Layout と同一): 凡例を実際に描画する位置 (= None なら凡例なし)。
-- color encoding が無ければ位置指定があっても None。 予約 (computeLayout) / 描画 (Render) の
-- 両方がこれを使い、 「予約したのに描かれない / 描いたのに予約してない」 ズレを防ぐ。
needsLegend :: VisualSpec -> LegendPosition -> LegendPosition
needsLegend spec pos
  | pos == LegendNone                = LegendNone
  -- ★ Phase 35: 形のみ (shapeBy・色無し) でも凡例を出す (= ggplot shape guide)。
  -- ★ Phase 40: hexbin (件数 colorbar) も色 enc 無しで凡例を出す。
  | hasColorEncoding (vsLayers spec)
    || hasShapeEncoding (vsLayers spec)
    || hasHexbinCountGuide spec       = pos
  | otherwise                        = LegendNone

-- | Phase 40: 色 enc を持たない hexbin layer (= 件数 colorbar 駆動) があるか (構造のみ)。
hasHexbinCountGuide :: VisualSpec -> Bool
hasHexbinCountGuide spec =
  not (hasColorEncoding (vsLayers spec))
  && any (\l -> getFirst (lyKind l) == Just MHexbin) (vsLayers spec)

-- | layer 群に shape aesthetic (lyShapeBy) があるか。
hasShapeEncoding :: [Layer] -> Bool
hasShapeEncoding = any (\l -> case getLast (lyShapeBy l) of
                                Just _  -> True
                                Nothing -> False)

-- | vsLegend (Last LegendSpec) から有効 position を得る。 未指定 = LegendRight (ggplot auto)。
effectiveLegendPos :: Last LegendSpec -> LegendPosition
effectiveLegendPos ls = case getLast ls of
  Just l  -> lgPosition l
  Nothing -> LegendRightCenter  -- Phase 43: 既定を ggplot legend.position="right" と同じ縦中央に

-- | layer 群に color/fill aesthetic (ColorByCol / ColorByContinuous) があるか。
hasColorEncoding :: [Layer] -> Bool
hasColorEncoding = any (\l -> case getLast (lyColor l) of
  Just (ColorByCol _)        -> True
  Just (ColorByContinuous _) -> True
  _                          -> False)

-- | Phase 34: 軸 tick ラベルを ggplot / base-R @format()@ 準拠で **ベクトル整形**する。
-- ggplot の連続スケール既定 (@labels = waiver()@) は break ベクトル全体に base R
-- @format()@ を掛ける。 その挙動を再現:
--
--   1. 全 break で**小数桁を統一**する (末尾ゼロを残す)。 例 0,.25,.5 → "0.00","0.25","0.50"
--      (旧 numToText は単値ごとにゼロ削りして "0.5" になっていた)。
--   2. **固定小数 vs 指数**を「最大幅が短い方」で選ぶ (base R @scipen = 0@: 固定表記が
--      指数表記より広いときだけ指数にする)。 例 density の 0..5e-4 は固定 "0.0005"(6字) >
--      指数 "5e-04"(5字) ゆえ "0e+00".."5e-04"、 0..1 は固定 "0.50"(4字) ≤ 指数 "5e-01"(5字)
--      ゆえ "0.00".."1.00"。
--
-- R @ggplot_build@ 実測値と一致することを確認済 (density y / 0..1 比率 y / 3000..6000 x)。
formatTicksGG :: [Double] -> [Text]
formatTicksGG [] = []
formatTicksGG xs =
  let dFixed = maximum (0 : map decimalsNeeded xs)
      fixed  = map (\v -> T.pack (showFFloat (Just dFixed) v "")) xs
      dSci   = maximum (0 : map (decimalsNeeded . fst . sciParts) xs)
      sci    = map (sciStr dSci) xs
      wFixed = maximum (map T.length fixed)
      wSci   = maximum (map T.length sci)
  in if wFixed > wSci then sci else fixed

-- | v を誤差なく表すのに要する小数桁 (0..10)。 nice tick 前提で 10 桁上限。
decimalsNeeded :: Double -> Int
decimalsNeeded v = go 0
  where
    go k | k >= 10                              = 10
         | abs (v - rounded k) <= 1e-9 * max 1 (abs v) = k
         | otherwise                            = go (k + 1)
    rounded k = let tk = 10 ^^ k :: Double
                in fromIntegral (round (v * tk) :: Integer) / tk

-- | v を仮数 m∈[1,10) と指数 e に正規化 (v = m * 10^e)。 0 は (0,0)。
sciParts :: Double -> (Double, Int)
sciParts 0 = (0, 0)
sciParts v =
  let e0 = floor (logBase 10 (abs v)) :: Int
      m0 = v / (10 ^^ e0)
  in norm m0 e0
  where
    norm m e
      | abs m >= 10 = norm (m / 10) (e + 1)
      | abs m <  1  = norm (m * 10) (e - 1)
      | otherwise   = (m, e)

-- | 指数表記 1 個 (仮数 d 桁 + "e±NN")。
sciStr :: Int -> Double -> Text
sciStr d v =
  let (m, e) = sciParts v
      mant   = showFFloat (Just d) m ""
      sign   = if e < 0 then "-" else "+"
      ae     = abs e
      expt   = (if ae < 10 then "0" else "") ++ show ae
  in T.pack (mant ++ "e" ++ sign ++ expt)

-- | Categorical axis labels (= ColTxt の distinct 値、 layer 横断)。
-- どの encoding (encX / encY) を見るかは accessor 引数で指定。
--
-- Phase 28 (2026-06-14): 既定順を ggplot2 の factor 既定と同じ **アルファベット順**
-- ('orderedCats') にした (= R4DS と凡例・色・軸並びを一致させる)。 明示順が要るときは
-- @scale_x_discrete(limits=)@ 相当の discrete-limits override (第 4 引数) を渡す
-- (= fct_infreq / fct_reorder 相当)。 override 指定時はデータ内に在る水準だけを
-- その順で返す (applyDiscreteLimits がデータ側を既に filter/並べ替え済)。
collectCategoricalLabels
  :: (Layer -> Last ColRef)
  -> Resolver -> VisualSpec -> Maybe [Text] -> [Text]
collectCategoricalLabels acc r spec mOverride =
  let labels = concat
        [ V.toList v
        | l <- vsLayers spec
        , Just cr <- [getLast (acc l)]
        , Just (TxtData v) <- [resolveCol r cr]
        ]
  in case mOverride of
       Just ws -> [ w | w <- ws, w `elem` labels ]   -- 明示順 (= fct_infreq 等)
       Nothing -> orderedCats labels                  -- 既定 = アルファベット順

-- | Phase 11 A4-a: scale の range (rLo/rHi) を入替えて軸反転。 domain は不変なので
-- tick (= domain 値) は scaleApply 経由で自動的に逆向き座標へ写る。 全 Scale variant が
-- lsRangeLo/lsRangeHi を共有するため record update 1 つで賄える。
revScale :: Scale -> Scale
revScale s = s { lsRangeLo = lsRangeHi s, lsRangeHi = lsRangeLo s }

scaleApply :: Scale -> Double -> Double
scaleApply (LinearScale dLo dHi rLo rHi) v
  | dHi == dLo = (rLo + rHi) / 2
  | otherwise  = rLo + (v - dLo) / (dHi - dLo) * (rHi - rLo)
scaleApply (LogScale dLo dHi rLo rHi) v
  | dHi <= 0 || dLo <= 0 = (rLo + rHi) / 2   -- 不正 domain は中央
  | v <= 0               = rLo                -- log 不能値は range 下端 clip
  | dHi == dLo           = (rLo + rHi) / 2
  | otherwise            =
      let lLo = log dLo; lHi = log dHi; lv = log v
      in rLo + (lv - lLo) / (lHi - lLo) * (rHi - rLo)
scaleApply (SqrtScale dLo dHi rLo rHi) v
  | dHi <  0 || dLo <  0 = (rLo + rHi) / 2   -- 負値 domain (= sqrt 不能) は中央
  | v < 0                = rLo                -- 負値 input は range 下端 clip
  | dHi == dLo           = (rLo + rHi) / 2
  | otherwise            =
      let sLo = sqrt dLo; sHi = sqrt dHi; sv = sqrt v
      in rLo + (sv - sLo) / (sHi - sLo) * (rHi - rLo)
scaleApply (TimeScale dLo dHi rLo rHi) v
  -- Time scale は internal は Linear (= 値 = unix epoch seconds)。
  -- tick / 表示 format のみ別 (= 描画側で適用)。
  | dHi == dLo = (rLo + rHi) / 2
  | otherwise  = rLo + (v - dLo) / (dHi - dLo) * (rHi - rLo)

-- ===========================================================================
-- Phase 33 B3: 相対単位込み座標 'Pos' の pt 解決
-- ===========================================================================
--
-- native/npc の意味は panel rect / scale (= Layout の産物) が決める。よって
-- 解決は backend ではなく engine 内 (この層) で行う ([[Option 1]])。本 phase の
-- layout 出力は純 pt なので、UCtx も pt 空間で解く (dpi は PAbs の Px 入力解決だけ)。

-- | 'Pos' を pt 座標へ解決する context。panel rect と x/y scale を与える。
data UCtx = UCtx
  { uDpi    :: !Double   -- ^ PAbs の Px を pt 化する dpi。
  , uRect   :: !Rect     -- ^ panel rect (pt)。PNpc 解決に使う。
  , uXScale :: !Scale    -- ^ PNative (x) 解決。
  , uYScale :: !Scale    -- ^ PNative (y) 解決。
  } deriving (Show, Eq)

-- | x 座標の 'Pos' を pt へ。PNpc 0=左端 (rX), 1=右端 (rX+rW)。
resolvePosX :: UCtx -> Pos -> Double
resolvePosX c p = case p of
  PAbs len  -> rX (uRect c) + lengthToPt (uDpi c) len
  PNpc t    -> rX (uRect c) + t * rW (uRect c)
  PNative v -> scaleApply (uXScale c) v

-- | y 座標の 'Pos' を pt へ。device 座標は y 下向き (rY=上端) ゆえ
-- PNpc 1=上端 (rY), 0=下端 (rY+rH)。PNative は反転済 scale が処理。
resolvePosY :: UCtx -> Pos -> Double
resolvePosY c p = case p of
  PAbs len  -> rY (uRect c) + lengthToPt (uDpi c) len
  PNpc t    -> rY (uRect c) + (1 - t) * rH (uRect c)
  PNative v -> scaleApply (uYScale c) v

-- ===========================================================================
-- Phase 9 C: coord_flip 用の座標投影 (= ggplot Coord の中間レイヤ)
-- ===========================================================================
--
-- 各 renderer は `Point (sx x)(sy y)` の代わりに projectXY/projectRectData/
-- projectBarRect を通す。 Cartesian は従来と bit 一致、 Flip は x/y を入替える。
-- **Coord は位置だけ変換** (= テキスト anchor/font・点半径・bar 厚みは px のまま)。

-- | spec の座標系 (Nothing = Cartesian)。
coordOf :: VisualSpec -> Coord
coordOf spec = maybe CoordCartesian id (getLast (vsCoord spec))

-- | データ空間 (dx, dy) → px (横, 縦)。 Cartesian は (sx dx, sy dy)、 Flip は
--   データ x を縦 px・データ y を横 px に (= 軸入替)。
projectXY :: Coord -> Layout -> Double -> Double -> (Double, Double)
projectXY CoordCartesian l dx dy =
  (scaleApply (lpXScale l) dx, scaleApply (lpYScale l) dy)
projectXY CoordFlip l dx dy =
  (scaleApply (lpYScaleFlipped l) dy, scaleApply (lpXScaleFlipped l) dx)
-- Phase 11 A7-c: 極座標。 theta 軸 (PolarX=x / PolarY=y) を角度 (0..2π、 上始点・
--   時計回り)、 他軸を半径 (中心=domain 下端、 外周=domain 上端) に写す。
projectXY CoordPolarX l dx dy = polarPoint l (domFrac (lpXScale l) dx) (domFrac (lpYScale l) dy)
projectXY CoordPolarY l dx dy = polarPoint l (domFrac (lpYScale l) dy) (domFrac (lpXScale l) dx)

-- | scale の domain における正規化位置 [0,1] (= (v - dLo)/(dHi - dLo))。 極座標で
--   角度/半径の比率を出すのに使う。 domain が退化していれば 0。
domFrac :: Scale -> Double -> Double
domFrac s v = let lo = lsDomainLo s; hi = lsDomainHi s
              in if hi == lo then 0 else (v - lo) / (hi - lo)

-- | 極座標の中心と最大半径 (= panel に内接する円)。
polarCenter :: Layout -> (Double, Double, Double)
polarCenter l = let a = lpPlotArea l
                    cx = rX a + rW a / 2
                    cy = rY a + rH a / 2
                    maxR = min (rW a) (rH a) / 2
                in (cx, cy, maxR)

-- | (角度 frac, 半径 frac) → px。 角度 0 を上 (12 時) とし時計回り、 半径 frac=1 が外周。
polarPoint :: Layout -> Double -> Double -> (Double, Double)
polarPoint l thetaFrac rFrac =
  let (cx, cy, maxR) = polarCenter l
      theta = thetaFrac * 2 * pi
      r     = rFrac * maxR
  in (cx + r * sin theta, cy - r * cos theta)

-- | データ空間の矩形 (x/y の min/max) → px Rect。 Flip では bbox が縦横転置される。
--   2 隅を projectXY して min/abs で正規化するだけ (= 向きに依らず正しい Rect)。
projectRectData :: Coord -> Layout -> Double -> Double -> Double -> Double -> Rect
projectRectData c l xminD xmaxD yminD ymaxD =
  let (x0, y0) = projectXY c l xminD yminD
      (x1, y1) = projectXY c l xmaxD ymaxD
  in Rect (min x0 x1) (min y0 y1) (abs (x1 - x0)) (abs (y1 - y0))

-- | bar/box 用: 中心線の data 座標 (centerD = x 群位置) と base..value の data 区間、
--   厚み thicknessPx (= px 単位の bar 幅) から px Rect を作る。 Cartesian では
--   横位置 = centerD ± 厚み/2、 縦 = base..value。 Flip では縦位置 = centerD ± 厚み/2、
--   横 = base..value (= 厚みは常に px のまま = 軸スケールに依らない)。
projectBarRect :: Coord -> Layout -> Double -> Double -> Double -> Double -> Rect
projectBarRect CoordCartesian l centerD baseD valueD thicknessPx =
  let cx = scaleApply (lpXScale l) centerD
      y0 = scaleApply (lpYScale l) baseD
      y1 = scaleApply (lpYScale l) valueD
  in Rect (cx - thicknessPx / 2) (min y0 y1) thicknessPx (abs (y1 - y0))
projectBarRect CoordFlip l centerD baseD valueD thicknessPx =
  let cy = scaleApply (lpXScaleFlipped l) centerD
      x0 = scaleApply (lpYScaleFlipped l) baseD
      x1 = scaleApply (lpYScaleFlipped l) valueD
  in Rect (min x0 x1) (cy - thicknessPx / 2) (abs (x1 - x0)) thicknessPx
-- Phase 11 A7-c: 極座標の bar は wedge (扇形) で描くため Rect では表せない。
--   renderBar が極座標を検出して PPath で arc を描く (= projectBarRect は使わない)。
--   ここは totality 維持のための placeholder (Cartesian 同式・極座標 bar 経路では未使用)。
projectBarRect CoordPolarX l centerD baseD valueD thicknessPx =
  projectBarRect CoordCartesian l centerD baseD valueD thicknessPx
projectBarRect CoordPolarY l centerD baseD valueD thicknessPx =
  projectBarRect CoordCartesian l centerD baseD valueD thicknessPx

-- | Phase 10 A4-fix: categorical 1 スロットの cross 軸 px 幅 (bar/box 等の厚みに使う)。
--   Cartesian は x 軸 (sx) の 1 単位、 Flip は category が縦に来るので flipped scale の
--   縦 1 単位。 これを使わず常に (sx 1 - sx 0) を厚みにすると flip 時に縦スロットを超えて
--   bar が重なる。 Cartesian では (sx 1 - sx 0) と完全一致 (= ゼロ diff)。
-- | Phase 41: ggplot @resolution(x)@ = ソート済み一意値の最小正間隔。 errorbar/crossbar の
-- cap 幅をデータ単位化する基準 (width = markWidth × resolution)。 一意値が 1 個以下なら 1
-- (categorical = 整数位置 0,1,2… で間隔 1・単一点も 1)。
resolutionOf :: [Double] -> Double
resolutionOf vs =
  let us  = map head . group . sort $ vs
      gaps = filter (> 1e-12) (zipWith (-) (drop 1 us) us)
  in case gaps of
       [] -> 1
       gs -> minimum gs

catUnitPx :: Coord -> Layout -> Double
catUnitPx CoordCartesian l = scaleApply (lpXScale l) 1 - scaleApply (lpXScale l) 0
catUnitPx CoordFlip      l =
  abs (scaleApply (lpXScaleFlipped l) 1 - scaleApply (lpXScaleFlipped l) 0)
catUnitPx CoordPolarX l = scaleApply (lpXScale l) 1 - scaleApply (lpXScale l) 0
catUnitPx CoordPolarY l = scaleApply (lpXScale l) 1 - scaleApply (lpXScale l) 0

-- | 軸が物理的にどの辺に来るか。 Cartesian: データ x=下・y=左。 Flip: データ x=左・y=下。
--   極座標は直交的な辺軸を持たない (Render の polar 分岐が独自に grid/軸を描く)。
data AxisPlacement = AxisBottom | AxisLeft | AxisTop | AxisRight
  deriving (Show, Eq)

coordXAxisPlacement :: Coord -> AxisPlacement
coordXAxisPlacement CoordFlip      = AxisLeft
coordXAxisPlacement _              = AxisBottom

coordYAxisPlacement :: Coord -> AxisPlacement
coordYAxisPlacement CoordFlip      = AxisBottom
coordYAxisPlacement _              = AxisLeft

-- | データ x の grid line が縦線か (Cartesian) 横線か (Flip)。
coordXGridIsVertical :: Coord -> Bool
coordXGridIsVertical CoordFlip      = False
coordXGridIsVertical _              = True

-- | 極座標か (= CoordPolarX / CoordPolarY)。
isPolar :: Coord -> Bool
isPolar CoordPolarX = True
isPolar CoordPolarY = True
isPolar _           = False

-- | D3 風 nice tick (= 1/2/5 × 10^k の刻み)。
niceTicks :: Int -> Double -> Double -> [Double]
niceTicks n lo hi
  | hi <= lo  = [lo]
  | n <= 0    = []
  | otherwise =
      let span_   = hi - lo
          rawStep = span_ / fromIntegral n
          mag     = 10 ** fromIntegral (floor (logBase 10 rawStep) :: Int)
          norm    = rawStep / mag
          step
            | norm < 1.5 = 1   * mag
            | norm < 3.5 = 2   * mag
            | norm < 7.5 = 5   * mag
            | otherwise  = 10  * mag
          start = fromIntegral (ceiling (lo / step) :: Int) * step
          go x | x > hi    = []
               | otherwise = x : go (x + step)
      in go start

-- | Phase 8 C (§5 G3): R labeling::extended (Talbot, Lin & Hanrahan 2010
-- "An Extension of Wilkinson's Algorithm…") の移植。 ggplot2 の既定 breaks
-- (`scales::extended_breaks(n)`) と同一: 候補刻み Q=[1,5,2,2.5,4,3]、 重み
-- w=[simplicity 0.25, coverage 0.2, density 0.5, legibility 0.05]、 only.loose=False、
-- legibility は常に 1 (R 実装も placeholder)。 simplicity/coverage/density の重み付き
-- スコアを最大化する (lmin, lmax, lstep) を選び、 等間隔 break 列を返す。
-- 入力 (dmin,dmax) は **expansion 前のデータ範囲**、 m は目標ラベル数。 旧 niceTicks
-- (1/2/5×10^k) を linear 軸で置換 (端点・本数が ggplot と一致する)。
--
-- j→q→k→z→start のネストループは R 実装をそのまま再現。 各段の上界 (simplicityMax /
-- densityMax / coverageMax) による枝刈りで停止するが、 浮動小数の保険として j/k/z に
-- 上限ガードを置く (実用域では枝刈りが先に効く)。
data Best = Best
  { bLmin  :: !Double
  , bLmax  :: !Double
  , bLstep :: !Double
  , bScore :: !Double
  }

extendedBreaks :: Int -> Double -> Double -> [Double]
extendedBreaks m dmin0 dmax0
  | not (dmax - dmin >= eps) = [dmin]
  | bScore best <= -2        = [dmin, dmax]   -- 念のためのフォールバック
  | otherwise                = genSeq (bLmin best) (bLmax best) (bLstep best)
  where
    (dmin, dmax) = if dmin0 > dmax0 then (dmax0, dmin0) else (dmin0, dmax0)
    eps = 2.220446049250313e-14 * 100        -- .Machine$double.eps * 100
    qs  = [1, 5, 2, 2.5, 4, 3] :: [Double]
    nD  = 6 :: Double
    mD  = fromIntegral m :: Double
    w1 = 0.25; w2 = 0.2; w3 = 0.5; w4 = 0.05
    qIdx q = go (1 :: Int) qs
      where go i (x : xs) = if x == q then i else go (i + 1) xs
            go i []       = i
    fmod' a b = a - b * fromIntegral (floor (a / b) :: Integer)
    simplicityMax q j =
      (nD - fromIntegral (qIdx q)) / (nD - 1) + 1 - fromIntegral j
    simplicity q j lmin lmax lstep =
      let mlt = fmod' lmin lstep
          v   = if (mlt < eps || lstep - mlt < eps) && lmin <= 0 && lmax >= 0
                  then 1 else 0
      in (nD - fromIntegral (qIdx q)) / (nD - 1) + v - fromIntegral j
    coverage lmin lmax =
      let rng = dmax - dmin
      in 1 - 0.5 * ((dmax - lmax) ** 2 + (dmin - lmin) ** 2) / ((0.1 * rng) ** 2)
    coverageMax spn =
      let rng = dmax - dmin
      in if spn > rng
           then let half = (spn - rng) / 2
                in 1 - 0.5 * (half ** 2 + half ** 2) / ((0.1 * rng) ** 2)
           else 1
    densityF k lmin lmax =
      let r  = (fromIntegral k - 1) / (lmax - lmin)
          rt = (mD - 1) / (max lmax dmax - min dmin lmin)
      in 2 - max (r / rt) (rt / r)
    densityMax k =
      if k >= m then 2 - (fromIntegral k - 1) / (mD - 1) else 1
    genSeq lo hi st
      | st <= 0   = [lo]
      | otherwise = let cnt = round ((hi - lo) / st) :: Int
                    in [ lo + fromIntegral i * st | i <- [0 .. cnt] ]
    best = goJ 1 (Best 0 0 1 (-2))
    -- j ループ (skip amount)。 q ループが「全停止」 を返したら打ち切る。
    goJ j b
      | j > 30    = b
      | otherwise = case goQ qs j b of
          (b', True)  -> b'
          (b', False) -> goJ (j + 1) b'
    goQ [] _ b = (b, False)
    goQ (q : qrest) j b =
      let sm = simplicityMax q j
      in if w1 * sm + w2 + w3 + w4 < bScore b
           then (b, True)               -- これ以降 score 改善不可 → 全停止
           else goQ qrest j (goK q sm j 2 b)
    -- k ループ (tick 本数)。
    goK q sm j k b
      | k > 2 * m + 6 = b
      | otherwise =
          let dm = densityMax k
          in if w1 * sm + w2 + w3 * dm + w4 < bScore b
               then b                   -- k ループ break
               else
                 let delta = (dmax - dmin) / fromIntegral (k + 1)
                               / fromIntegral j / q
                     z0 = ceiling (logBase 10 delta) :: Int
                 in goK q sm j (k + 1) (goZ q sm j k dm (60 :: Int) z0 b)
    -- z ループ (刻みの桁)。
    goZ q sm j k dm fuel z b
      | fuel <= 0 = b
      | otherwise =
          let step = fromIntegral j * q * (10 ** fromIntegral z)
              cm   = coverageMax (step * fromIntegral (k - 1))
          in if w1 * sm + w2 * cm + w3 * dm + w4 < bScore b
               then b                   -- z ループ break
               else
                 let minStart = floor   (dmax / step) * fromIntegral j
                                  - fromIntegral ((k - 1) * j)
                     maxStart = ceiling (dmin / step) * fromIntegral j
                     b' = if minStart > maxStart
                            then b
                            else goStart q j k step minStart maxStart b
                 in goZ q sm j k dm (fuel - 1) (z + 1) b'
    -- start ループ (label 列の起点)。
    goStart q j k step minStart maxStart b =
      foldl' upd b [minStart .. maxStart]
      where
        unit = step / fromIntegral j
        upd acc start =
          let lmin  = fromIntegral start * unit
              lmax  = lmin + step * fromIntegral (k - 1)
              lstep = step
              s     = simplicity q j lmin lmax lstep
              c     = coverage lmin lmax
              d     = densityF k lmin lmax
              score = w1 * s + w2 * c + w3 * d + w4 * 1   -- legibility = 1
          in if score > bScore acc
               then Best lmin lmax lstep score
               else acc

-- | Log scale 用 tick (= 10^k グリッド)。 domain 内の整数 exponent を出す。
niceTicksLog :: Int -> Double -> Double -> [Double]
niceTicksLog _n lo hi
  | lo <= 0 || hi <= 0 || hi <= lo = [lo]
  | otherwise =
      let kLo = floor   (logBase 10 lo) :: Int
          kHi = ceiling (logBase 10 hi) :: Int
      in [ 10 ** fromIntegral k | k <- [kLo .. kHi], let v = 10 ** fromIntegral k :: Double
                                                 , v >= lo, v <= hi ]

-- | Sqrt scale 用 tick (Phase 6 A6): sqrt 後を niceTicks に通し、 二乗して戻す。
-- domain が非負前提。 負値 lo は 0 にクランプ。
niceTicksSqrt :: Int -> Double -> Double -> [Double]
niceTicksSqrt n lo hi
  | hi <= lo  = [max 0 lo]
  | hi < 0    = [lo]
  | otherwise =
      let lo'  = max 0 lo
          sLo  = sqrt lo'
          sHi  = sqrt hi
          sTks = niceTicks n sLo sHi
      in map (\t -> t * t) sTks

-- | Time scale 用 tick (Phase 6 A7): unix epoch (= seconds since 1970) を入力に、
-- 「綺麗な」 間隔 (= 1m / 1h / 1d / 1w / 1M / 1y) で tick を生成。
-- 簡略実装: linear nice ticks を秒単位で取り、 1m / 1h / 1d / 1w 単位に丸め。
-- 月 / 年単位の境界調整は将来。
niceTimeTicks :: Int -> Double -> Double -> [Double]
niceTimeTicks n lo hi
  | hi <= lo  = [lo]
  | otherwise =
      let span_  = hi - lo
          rawStep = span_ / fromIntegral n
          -- 候補単位 (秒): 1s, 5s, 15s, 30s, 1m, 5m, 15m, 30m, 1h, 3h, 6h, 12h, 1d, 1w
          candidates =
            [ 1, 5, 15, 30
            , 60, 5*60, 15*60, 30*60       -- 分
            , 3600, 3*3600, 6*3600, 12*3600 -- 時
            , 86400, 7*86400                -- 日, 週
            , 30*86400, 91*86400, 365*86400 -- 月相当, 四半期, 年
            ] :: [Double]
          step = head $ dropWhile (< rawStep) candidates ++ [last candidates]
          start = fromIntegral (ceiling (lo / step) :: Int) * step
          go x | x > hi    = []
               | otherwise = x : go (x + step)
      in go start
