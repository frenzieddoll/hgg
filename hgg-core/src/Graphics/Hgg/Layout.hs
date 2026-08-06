-- |
-- Module      : Graphics.Hgg.Layout
-- Description : Layer 2 — layout computation (viewport / scale / axis tick)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'VisualSpec' から viewport / scale / axis tick を計算する純粋関数群。
--   col 名参照は 'Resolver' で Vector に解決した上で extent を求める。
-- [English]: A set of pure functions that compute viewport / scale / axis
--   ticks from a 'VisualSpec'. Column-name references are resolved to
--   vectors via 'Resolver' before their extents are computed.
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Layout
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
  , legendOrder
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
    -- ★ Phase 63 A4: tick 長・向きの実効値 (予約 computeLayout と描画 tickMarks で共有)。
  , effectiveTickLength
  , effectiveTickDir
  , tickOutwardLen
    -- ★ Phase 63 A5: plot margin の実効値 (予約 computeLayout と描画 labels で共有)。
  , effectivePlotMargin
    -- ★ Phase 63 A12: base font size の実効値 (予約 computeLayout と描画 mkFontTS で共有)。
  , effectiveBaseFontSize
  , effectiveFontSize
    -- ★ Phase 63 A13: half_line 派生 spacing の実効値 (既定 11 で従来定数と bit 同値)。
  , effectiveHalfLine
  , effectiveAxTextMar
  , effectiveAxTitleMar
  , effectiveLegendBaseSize
  , effectiveLegendKeyW
  , effectiveLegendKeyPitch
  , effectiveSubtitleSize
  , effectiveCaptionSize
  , effectiveTagSize
    -- ★ Phase 63 A19: axis.text / axis.title 表示の実効値 (予約 computeLayout と
    --   描画 tickMarks/labels で共有。 ThemeVoid のみ既定 False)。
  , effectiveShowAxisText
  , effectiveShowAxisTitle
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
    -- ★ Phase 64 A8: 外周円 (θ ラベル位置 = clip 境界) の比と clip path
  , polarOuterFrac
  , polarClipPath
  , domFrac
    -- ★ Phase 64 A2: 座標系依存の形状を投影層に集約する口 (§1 の受け皿)。
  , projectSegment
  , BarShape(..)
  , projectBar
  , wedgeSegments
    -- ★ Phase 64 A3: categorical-cross geom (box/violin/strip/swarm) 用の投影口。
  , CrossLoc(..)
  , projectCrossPoint
  , projectCrossSpan
  , projectCrossBar
  , valueAxisPx
  ) where

import           Graphics.Hgg.Layout.RangeOf (collectXY, extentsOrDefault,
                                              histRawDomain)
import           Graphics.Hgg.Palette (ggplotHue)
import           Graphics.Hgg.Unit (lengthToPt, Pos (..))
import           Graphics.Hgg.Spec (AxisKind (..), AxisSpec (..), ColData (..),
                                    DAGNode (..), DAGNodeKind (..),
                                    ColRef, ColorEnc (..), FontSpec (..), Layer (..),
                                    LegendPosition (..), LegendSpec (..),
                                    MarkKind (..), Resolver,
                                    ThemeName (..), ThemeOverride (..), TickDir (..),
                                    Margin (..), Coord (..), PolarOpts (..),
                                    VisualSpec (..), YAxisSide (..),
                                    applyDiscreteLimits, axisKindOf, ridgeAutoFlip,
                                    axTickValsOf, axTickLabelsOf, distGroupRef,
                                    resolveAxisAngle, axisTextAngleXOf,
                                    compositeLanes, colRefName,
                                    lgPosition, lyColor, lyColorCats, lyShapeBy,
                                    lyEncX, lyEncY, lyKind, lyBinCount,
                                    lyYAxisSide, orderedCats, resolveCol,
                                    resolveNum, themeSeriesPalette,
                                    HexCell (..), hexbinLayerCells)
import           Graphics.Hgg.Primitive (PathSegment (..), Point (..),
                                         Rect (..))  -- Phase 51: leaf へ移設・re-export
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

-- Phase 51: 'Rect' は 'Graphics.Hgg.Primitive' (leaf) へ移設。 本 module は
-- import + export list で re-export し、 既存の @import Layout (Rect(..))@ を不変に保つ。

-- | [日本語]: 当初は Linear のみだったが、 PlotConfig.xLog / yLog 等価の
--   LogScale を追加 (= 自然対数 ln で線形化、 描画は底 10 で tick 表示)。
--   [English]: Originally Linear-only; LogScale was added to match
--   PlotConfig.xLog / yLog (linearized via the natural log ln, rendered
--   with base-10 ticks).
data Scale
  = LinearScale { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  | LogScale    { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  -- | [日本語]: Sqrt scale: forward = sqrt v (= 数値が非負の domain 限定、
  --   負値は range 下端 clip)。 inverse は描画側で不要 (= tick は値域、 表示は元値)。
  --   [English]: The Sqrt scale: forward = sqrt v (restricted to a
  --   non-negative domain; negative values clip to the range's lower
  --   bound). No inverse is needed on the render side (ticks are in value
  --   space, and displayed values are the originals).
  | SqrtScale   { lsDomainLo, lsDomainHi, lsRangeLo, lsRangeHi :: !Double }
  -- | [日本語]: Time scale: unix epoch (Double seconds) を Linear で扱う。
  --   tick は niceTimeTicks (= 1m / 1h / 1d / 1w / 1M / 1y candidates)。
  --   表示 format は AxisFormat の AxisTimeFmt 経由 (= Render 側)。
  --   [English]: The Time scale: treats a unix epoch (Double seconds)
  --   linearly. Ticks come from niceTimeTicks (1m / 1h / 1d / 1w / 1M / 1y
  --   candidates). Display formatting goes through AxisFormat's AxisTimeFmt
  --   (on the Render side).
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
    -- ★ Phase 63 A15: 軸タイトルの配置 offset (panel 端 → axis.title margin 外縁まで =
    --   tick 突出 + axis.text margin + tick ラベル帯 + axis.title margin)。 描画
    --   (Render.labels) は panel 端 + この offset を基準に baseline を置く (= ggplot の
    --   「軸 text 直下 + margin」 方式)。 bM/lM の予約 stack と同じ構成要素 (単一情報源)。
    --   旧 boxBottom/boxLeft 最外端 pin は LegendBottom/caption 時にタイトルが凡例の
    --   外側 (最下端) へ出ていた (J2/J5)。
  , lpXTitleOff :: !Double
  , lpYTitleOff :: !Double
    -- ★ Phase 63 A17: bottom 凡例の配置 (予約 bM と描画 renderLegendBottom の単一情報源)。
    --   lpLegendYOff = panel 下端 → 凡例ブロック上端 (= bM の軸 stack と同一構成 +
    --   legend gap 2×half_line = ggplot legend.box.spacing)。 lpLegendNCol = 実効列数
    --   (明示 legendNrow 優先、 未指定は panel 幅に収まる最大列数へ auto-wrap)。
    --   旧 render は panel 下端 + 50 固定で軸タイトルと逆順 + 幅超過で右見切れ (J5)。
  , lpLegendYOff :: !Double
  , lpLegendNCol :: !Int
  } deriving (Show, Eq)

-- | [日本語]: 'VisualSpec' の全 layer から 'Resolver' で encX/encY を解決、
--   全 layer横断で extent を計算。 viewport は spec の width/height、 余白は
--   固定 margin。
--   [English]: Resolves encX/encY across every layer of a 'VisualSpec' via
--   'Resolver', computing the extent across all layers. The viewport comes
--   from the spec's width/height; margins are fixed.
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
      -- ★ Phase 63 A12: 解決を mkFontTS と同一情報源へ (theme override の fsSize +
      --   toBaseFontSize 派生既定)。 旧 fontSizeOf (setter のみ・13.2/11/8.8 固定) は
      --   theme*Font 指定 (cowplot preset 等) を予約に反映できていなかった。
      base          = effectiveBaseFontSize spec
      ovT           = vsThemeOverride spec
      titleSize     = effectiveFontSize (vsTitleFont     spec) (toTitleFont     ovT) (base * 1.2)  -- plot.title
      axisLabelSize = effectiveFontSize (vsAxisLabelFont spec) (toAxisLabelFont ovT) base          -- axis.title
      tickSize      = effectiveFontSize (vsTickFont      spec) (toTickFont      ovT) (base * 0.8)  -- axis.text
      -- 左 margin 用の y 目盛りラベル: 離散 limits (yCatLabels) > 明示ラベル
      -- (axisBreaksLabeled = explicitYLabs) > numeric tick の順で採用する。
      -- (明示ラベルを測らないと長い category ラベルが軸外へ溢れる)。
      yTickLabelStrs
        | not (null yCatLabels)    = yCatLabels
        | not (null explicitYLabs) = explicitYLabs
        | otherwise                = formatTicksGG yTicks
      -- ★ Phase 63 A19: axis.text / axis.title 非表示 (ThemeVoid 既定 /
      --   themeAxisText・themeAxisTitle False) はラベル文字・軸タイトルぶんの予約を
      --   丸ごと落とす (ggplot element_blank = zero-size grob)。 tick 線の tickOut は
      --   独立に残る (長さは effectiveTickLength、 ThemeVoid は既定 0)。
      showAxText  = effectiveShowAxisText spec
      showAxTitle = effectiveShowAxisTitle spec
      maxYTickW = if not showAxText || null yTickLabelStrs then 0
                  else 0.6 * tickSize
                         * fromIntegral (maximum (map T.length yTickLabelStrs))
      hasTitle  = case getLast (vsTitle  spec) of Just _ -> True; _ -> False
      hasXLabel = showAxTitle && case getLast (vsXLabel spec) of Just _ -> True; _ -> False
      hasYLabel = showAxTitle && case getLast (vsYLabel spec) of Just _ -> True; _ -> False
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
      -- ★ Phase 63 A14: labs の font size も base 派生 (旧固定 11/9/13 は base 11 の
      --   丸め値。 描画 Render.labels と同じ effective* を参照 = 単一情報源)。
      labsSubExtra = if hasSubtitle then effectiveSubtitleSize spec + sc * hl else 0
      labsTagExtra = if hasTag && not (hasTitle || hasSubtitle) then effectiveTagSize spec + sc * hl else 0
      labsCapExtra = if hasCaption then effectiveCaptionSize spec + sc * hl else 0
      -- Phase 8 C (small-viewport text fix): 間隔定数 (halfLine/tickLen/axTextMar/
      -- axTitleMar) は sc 倍するが、 文字サイズ由来の項 (titleSize/tickSize/maxYTickW/
      -- axisLabelSize) は **等倍** (フォントは実寸描画で縮まないため)。 旧実装は全体を
      -- sc 倍し、 小 viewport (subplots/pairs/inset) で数値が軸に被っていた。 sc=1 では
      -- 新旧同値なので通常プロットは不変。
      -- ★ Phase 63 A5: 外周余白は theme 実効値 (themePlotMargin、 既定 = 各辺 half_line
      --   で従来と同値)。
      -- ★ Phase 63 A13: 内側 spacing (title 下 margin・axis.text/axis.title margin・
      --   凡例 gap) も half_line = base/2 派生の実効値へ (既定 11 で従来定数と bit 同値)。
      pm = effectivePlotMargin spec
      hl = effectiveHalfLine spec
      -- ★ Phase 63 A19: axis.text 非表示なら axis.text margin も 0 (定義 1 箇所で
      --   bM/lM/xTitleOff/yTitleOff/legendYOff の全 stack に波及)。
      axTextMar  = if showAxText then effectiveAxTextMar spec else 0
      axTitleMar = effectiveAxTitleMar spec
      tM = sc * marTop pm + (if hasTitle then titleSize + sc * hl else 0)
                 + labsSubExtra + labsTagExtra
      -- ★ x 目盛りラベルの回転 (axisRotate) 予約: 非回転は tickSize (従来) だが、
      --   回転時はラベル**幅**が下方向に伸びる。 左 margin の maxYTickW と対称に、
      --   x 目盛りラベルの最大文字幅を回転角で投影して予約する (rotX=0 で従来同値)。
      -- ★ Phase 63 A16: 解決順を描画 (Render/Layer resolveAxisAngle) と単一情報源化。
      --   per-axis 明示 > theme (axisTextAngleXOf = 共通 <> X 別) > 0。 旧 axisRotateOf は
      --   theme 経由の回転 (themeAxisTextAngleX 等) を無視し回転マージン未予約だった (J4)。
      xRot = resolveAxisAngle (vsXAxis spec) (axisTextAngleXOf ovT)
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
        | not showAxText = 0   -- ★ A19: ラベル文字が無いので高さ予約もしない
        | xRot == 0 = tickSize
        | otherwise = let rad = xRot * pi / 180
                      in tickSize * abs (cos rad) + maxXTickW * abs (sin rad)
      -- ★ Phase 63 A4: tick の外向き突出量は theme 実効値 (themeTickLength/themeTickDir)。
      --   未指定は ggTickLen/TickOut で従来と同値。 描画 (tickMarks) と単一情報源。
      tickOut = tickOutwardLen spec
      bM | isContainer = sc * marBottom pm + legendH + labsCapExtra
         | otherwise   = sc * (marBottom pm + tickOut + axTextMar) + xTickReserve
                 + (if hasXLabel then sc * axTitleMar + axisLabelSize else 0)
                 + legendH + labsCapExtra
      lM | isContainer = sc * marLeft pm
         | otherwise   = sc * (marLeft pm + tickOut + axTextMar) + maxYTickW
                 + (if hasYLabel then sc * axTitleMar + axisLabelSize else 0)
      -- ★ Phase 63 A15: 軸タイトルの panel 端からの配置 offset。 bM/lM の予約 stack と
      --   同じ構成要素で算出 (単一情報源)。 container は軸 stack を持たないので
      --   axTitleMar のみ。
      xTitleOff | isContainer = sc * axTitleMar
                | otherwise   = sc * (tickOut + axTextMar + axTitleMar) + xTickReserve
      yTitleOff | isContainer = sc * axTitleMar
                | otherwise   = sc * (tickOut + axTextMar + axTitleMar) + maxYTickW
      -- Phase 9 A-5 (PS Layout と同一): 凡例ぶん plotArea を縮めて図内に収める (ggplot は
      -- legend を gtable の一部として扱い panel を縮める)。 Inside/None は予約しない。
      -- ★ Phase 34: facet 時も右凡例を予約する (旧実装は facet で legendW=0 にして凡例を
      --   完全に落としていた = ggplot は facet でも凡例を出す)。
      legendPos = needsLegend spec (effectiveLegendPos spec)
      -- ★ Phase 38: 右凡例幅を「最長ラベル」で算出 (固定 80/+70列 を撤去)。 renderGuideBlock の
      --   描画式に一致する 'legendGuideWidth' を全 guide に適用し、 縦スタックゆえ最大幅を予約。
      --   gap (panel→凡例 = 2*half_line) は renderLegendRight の x0 オフセットと一致。
      --   フォントは既定 (item=base×0.8 / title=base)。 override 無し時 render と一致 (旧固定80は
      --   フォント完全無視だったので後退なし)。
      --   ★ Phase 63 A13: 凡例基準も base 派生 (effectiveLegendBaseSize = 2×half_line)。
      legItemF  = effectiveLegendBaseSize spec * 0.8
      legTitleF = effectiveLegendBaseSize spec
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
          legendGuideWidth spec legItemF legTitleF (effectiveLegendTitle spec) (allColorCategories r (vsLayers spec))
        ColorGuide (ColorByContinuous cr) ->
          legendGuideWidth spec legItemF legTitleF (effectiveLegendTitle spec) (contColorLabels cr)
        ColorGuide (ColorStatic _)        -> 0
        CountBarGuide lo hi               ->
          legendGuideWidth spec legItemF legTitleF "count"
            (map numToText (filter (\b -> b >= lo && b <= hi) (extendedBreaks 5 lo hi)))
        ShapeGuide scr                    ->
          -- 見出しは render と同じく sentinel を空に潰してから幅を見積る。
          let nm = colRefName scr
              t  = if nm == "<inline-num>" || nm == "<inline-txt>" then "" else nm
          in legendGuideWidth spec legItemF legTitleF t (shapeCats scr)
      legendGuidesW = maximum (0 : map guideWidth (collectGuides r spec))
      -- Phase 32 (re-apply): LegendRightCenter も右域に同じ幅を予約 (縦位置のみ違う)。
      legendW = if legendPos == LegendRight || legendPos == LegendRightCenter
                  then 2 * hl + legendGuidesW else 0
      -- ★ Phase 63 A17: bottom 凡例の実寸予約 (旧 50 + (nrow-1)*16 固定を撤去 = J5)。
      --   gap = 2×half_line (ggplot legend.box.spacing)、 行 pitch = effectiveLegendKeyPitch。
      --   列数 = 明示 legendNrow 優先、 未指定は panel 幅 (availW) に収まる最大列数
      --   (item/title 幅は renderLegendBottom の itemAdv/titleW と同式 = 予約と描画の
      --   単一情報源)。 availW は lM/rM のみ依存で legendH と循環しない。
      legendGapB = 2 * hl
      legRowH    = effectiveLegendKeyPitch spec
      legLabels  = map snd (legendOrder spec (allColorCategories r (vsLayers spec)))
      nLeg       = length legLabels
      legItemAdv lbl = 14 + legItemF * textWidthEm lbl + hl
      legTitleWB = let t = effectiveLegendTitle spec
                   in if t == "" then 0 else legTitleF * textWidthEm t + 12
      legFits nc = let colW c = maximum (0 : [ legItemAdv (legLabels !! k)
                                             | k <- [0 .. nLeg - 1], k `mod` nc == c ])
                   in legTitleWB + sum (map colW [0 .. nc - 1]) <= availW
      legNCol
        | nLeg == 0 = 1
        | otherwise = case getLast (vsLegendNrow spec) of
            Just nr -> max 1 ((nLeg + max 1 nr - 1) `div` max 1 nr)
            Nothing -> head ([ nc | nc <- [nLeg, nLeg - 1 .. 2], legFits nc ] ++ [1])
      legNRowB = max 1 ((max 1 nLeg + legNCol - 1) `div` legNCol)
      legendH = if legendPos == LegendBottom
                  then legendGapB + fromIntegral legNRowB * legRowH else 0
      -- panel 下端 → 凡例ブロック上端 = bM の軸 stack (legendH/labsCapExtra を除く
      -- 内側部分) + gap。 caption は凡例のさらに外側 (bM の積み順と同じ)。
      legendYOff | isContainer = legendGapB
                 | otherwise   = sc * (tickOut + axTextMar) + xTickReserve
                       + (if hasXLabel then sc * axTitleMar + axisLabelSize else 0)
                       + legendGapB
      rM = sc * marRight pm + rightAxisW + legendW
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
       , lpXTitleOff = xTitleOff
       , lpYTitleOff = yTitleOff
       , lpLegendYOff = legendYOff
       , lpLegendNCol = legNCol
       }

-- | [日本語]: ggplot 準拠: margin 縮小係数を撤廃 (常に 1)。 ggplot は文字・余白を
--   固定 pt で扱い viewport サイズで縮めない (パネルが残りを埋めるだけ)。 旧実装は
--   小 viewport で sc<1 に縮小していたが、 grid は軸帯確保 (renderSubplots) で対応し、
--   単一小 viewport (inset) も固定 pt で ggplot と同挙動にする。 panel が潰れないよう
--   computeLayout 側で availW/availH に下限を設ける。 シグネチャは互換のため温存。
--   [English]: Following ggplot: the margin-shrink factor has been removed
--   (always 1). ggplot treats text and whitespace as fixed pt and does not
--   shrink them with viewport size (the panel simply fills whatever
--   remains). The previous implementation shrank by sc<1 for small
--   viewports; grid now handles this via axis-band reservation
--   (renderSubplots), and a single small viewport (inset) also behaves like
--   ggplot with fixed pt. computeLayout sets a lower bound on availW/availH
--   to keep the panel from collapsing. The signature is kept for
--   compatibility.
ggMarginScale :: Double -> Double -> Double
ggMarginScale _ _ = 1

-- ===========================================================================
-- Phase 8 C (gtable §E): 汎用 1 次元トラック割付
-- ===========================================================================

-- | [日本語]: gtable のトラック (行 or 列) サイズ種別。 ggplot の grid::unit に
--   対応: Fixed v = 固定 pt (= 軸テキスト/タイトル/strip/plot.margin の grob
--   実寸)、 Null  w = 伸縮トラック (= unit(w,"null")、 残りスペースを重み比で
--   分配 = パネル本体)。
--   [English]: The size kind of a gtable track (row or column), matching
--   ggplot's grid::unit: Fixed v is a fixed pt size (the grob's actual size
--   for axis text/title/strip/plot.margin); Null w is an elastic track
--   (unit(w,"null"), distributing remaining space by weight — the panel
--   body).
data Track = Fixed !Double | Null !Double
  deriving (Show, Eq)

-- | [日本語]: 1 次元トラック割付。 利用可能長 avail から Fixed 合計を先取りし、
--   残りを Null トラックに重み比で配分する (= ggplot gtable の「固定先取り →
--   null 残り均等」)。 残りが負なら Null=0 (= パネルが潰れる、 ggplot と同挙動)。
--   パネル間 spacing は呼び出し側が Fixed トラックとして明示挿入する。 戻り =
--   各トラックの (start, length) (start は absolute)。
--   [English]: 1D track allocation. Claims the sum of Fixed sizes from the
--   available length avail first, then distributes the remainder to Null
--   tracks by weight (matching ggplot gtable's "claim fixed first, then
--   split the null remainder"). If the remainder is negative, Null=0 (the
--   panel collapses, matching ggplot's behaviour). Inter-panel spacing must
--   be inserted explicitly by the caller as a Fixed track. Returns each
--   track's (start, length), where start is absolute.
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

-- | [日本語]: ggplot half_line マージン定数 (pt, sc 適用前)。 layout が純 pt
--   空間になったことで、 これらは ggplot 由来の pt 値 (half_line=
--   base_size/2=5.5pt) そのものとして正しく pt 意味になる (値は不変・k は
--   backend)。 computeLayout の margin 計算と Render の描画オフセットで共有
--   (単一情報源)。
--   [English]: ggplot's half_line margin constants (pt, before sc is
--   applied). Now that layout is a pure pt space, these are correctly
--   interpreted as pt values straight from ggplot (half_line =
--   base_size/2 = 5.5pt; the values are unchanged, and k belongs to the
--   backend). Shared between computeLayout's margin computation and
--   Render's drawing offsets (a single source of truth).
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

-- | [日本語]: 凡例のベースフォント (pt)。 ggplot @base_size@ = 2 × half_line =
--   11pt。
--   [English]: The legend's base font size (pt). ggplot's @base_size@ is
--   2 × half_line = 11pt.
legendBaseSize :: Double
legendBaseSize = 2 * ggHalfLine

-- | [日本語]: 凡例キーの 1 辺 (pt) = ggplot @legend.key.size =
--   unit(1.2,"lines")@。 ★R gtable トレース実測 = 17.34pt (base 11pt 時)。
--   grid の "lines" は行高 (= 1.2 × base × lineheight) なので 1.2×base(=13.2)
--   ではなくこの値。 = 1.2 × base × 1.3133。
--   [English]: The side length of a legend key (pt), matching ggplot's
--   @legend.key.size = unit(1.2,"lines")@. Measured from an R gtable trace
--   as 17.34pt (at base 11pt). Since grid's "lines" is a line height
--   (1.2 × base × lineheight), the value is not 1.2×base(=13.2) but this
--   one: 1.2 × base × 1.3133.
legendKeyW :: Double
legendKeyW = 1.2 * legendBaseSize * 1.3133

-- | [日本語]: 凡例キーの行ピッチ = keyW (= ggplot gtable のキー間 spacing 行 =
--   0pt = キーセル隣接)。
--   [English]: The legend key's row pitch, equal to keyW (ggplot gtable's
--   inter-key spacing row is 0pt, so key cells are adjacent).
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

-- | [日本語]: East Asian Width が全角 (F=Fullwidth / W=Wide) の文字か。 CJK
--   統合漢字・かな・全角記号・ハングル等を 1.0em 扱いにする。 範囲は Unicode
--   EAW (UAX #11) の W/F に対応する代表ブロックを網羅 (厳密 table でなく実用的な
--   近似・凡例幅にのみ使用)。
--   [English]: Whether a character has East Asian Width Fullwidth (F) or
--   Wide (W), treated as 1.0em (CJK unified ideographs, kana, fullwidth
--   punctuation, Hangul, etc.). The ranges cover representative blocks
--   corresponding to Unicode EAW (UAX #11) W/F (a practical approximation
--   rather than an exact table; used only for legend width).
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

-- | [日本語]: 1 文字の advance を em 単位で近似。 ★既定 sans (DejaVu) の実
--   advance を計測して字種別にバケット化 (rsvg trim 実測。 例 i/l≈0.25・
--   a/e≈0.56・M/W≈0.9)。 旧 flat 0.6 は細字主体ラベル (小文字+ハイフン等) で
--   平均 ~0.49em/字を 0.6 と過大予約し右余白を生んでいた。 値は実測平均をやや
--   上回る安全側に丸め 「切れない方向」 を維持。 全角は 'isWideChar' で 1.0em。
--   ★この表は HS=PS で完全一致させること (PS canvas も同値)。
--   [English]: Approximates a single character's advance in em units.
--   Measured from the actual advance of the default sans font (DejaVu) and
--   bucketed by character class (measured via rsvg trim; e.g. i/l≈0.25,
--   a/e≈0.56, M/W≈0.9). The previous flat 0.6 over-reserved space for
--   labels dominated by narrow glyphs (lowercase, hyphens, etc.), whose
--   true average is ~0.49em/char, producing excess right padding. Values
--   are rounded slightly above the measured average, on the safe side of
--   "never truncate". Fullwidth characters are 1.0em via 'isWideChar'.
--   This table must match exactly between Haskell and PureScript (the
--   PureScript canvas uses the same values).
charWidthEm :: Char -> Double
charWidthEm c
  | isWideChar c                              = 1.0
  | c `elem` ("iIl|.,;:'`!()[]{} " :: String) = 0.30  -- 細字・記号・空白
  | c `elem` ("jftr-/\\" :: String)           = 0.42  -- やや細
  | c `elem` ("mwMW@" :: String)              = 0.92  -- 幅広
  | c >= 'A' && c <= 'Z'                       = 0.70  -- 大文字 (M/W は上で処理済)
  | otherwise                                 = 0.58  -- 小文字・数字・その他

-- | [日本語]: 文字列の幅を em 単位で見積もる (字種別 'charWidthEm' の総和)。
--   実 pt 幅 = fontSize × この値。
--   [English]: Estimates a string's width in em units (the sum of per-glyph
--   'charWidthEm'). The actual pt width is fontSize × this value.
textWidthEm :: Text -> Double
textWidthEm = T.foldl' (\acc ch -> acc + charWidthEm ch) 0

-- | [日本語]: DAG node ラベルのフォントサイズ (pt)。 layout (Sugiyama の
--   size-aware 横幅見積り) と render (@nodeExtent@) で共有する単一定義。 旧
--   Render.EdgeRoute から移管。
--   [English]: The font size of a DAG node label (pt). A single definition
--   shared by layout (Sugiyama's size-aware width estimation) and render
--   (@nodeExtent@). Migrated from the previous Render.EdgeRoute.
dagLabelFs :: Double
dagLabelFs = 11

-- | [日本語]: DAG node の __radius 非依存__な横半幅 (px)。 = @nodeExtent@ の
--   rx から @max baseR@ の floor を除いた本体 (ラベル名 / 分布 sublabel 幅に
--   由来)。
--
--   layout の size-aware simplex (Sugiyama @auxSepOf@ / @clusterAuxEdges@)
--   と render の @nodeExtent@ が __同一式__を共有することで、 simplex が
--   確保する node 間隔と描画箱の幅を整合させる (= 兄弟 plate の box 重なりを
--   根治)。 radius は layout 時に未知 (= render-time の lySize) ゆえ floor
--   部分は render 側 (@nodeExtent@) で適用する。
--   [English]: The __radius-independent__ half-width (px) of a DAG node —
--   the body of @nodeExtent@\'s rx with the @max baseR@ floor removed
--   (derived from the label name / distribution sublabel width).
--
--   Layout's size-aware simplex (Sugiyama's @auxSepOf@ / @clusterAuxEdges@)
--   and render's @nodeExtent@ share __the same formula__, keeping the node
--   spacing the simplex reserves consistent with the drawn box width (fixing
--   overlapping boxes between sibling plates at the root). Since the radius
--   is unknown at layout time (it is render-time's lySize), the floor part
--   is applied on the render side (@nodeExtent@).
dagNodeBaseHalfWidth :: DAGNode -> Double
dagNodeBaseHalfWidth n =
  let showDist = case dnKind n of
        NodeDeterministic -> False
        _                 -> maybe False (const True) (dnDist n)
      nameEm = textWidthEm (dnLabel n)
      distEm = case dnDist n of Just d | showDist -> textWidthEm d; _ -> 0
      maxEm  = max 0.5 (max nameEm distEm)
  in dagLabelFs * maxEm / 2 + 8

-- | [日本語]: 単一 guide (右凡例・縦1列) の必要幅 (pt)。 renderGuideBlock の
--   描画式に厳密一致: 列幅 = (key 1辺) + (key→label gap = half_line/2) +
--   (最長ラベル幅) + (右パディング = half_line)。 タイトルがそれより広ければ
--   タイトル幅。 引数: spec (★key 幅/gap を base 派生の実効値で引くため) /
--   item フォント pt / title フォント pt / タイトル文字列 / ラベル群。 ★「最長」
--   は文字数でなく 'textWidthEm' 最大 (全角混在で逆転し得るため幅で選ぶ)。
--   [English]: The required width (pt) of a single guide (a right-side,
--   single-column legend). Matches renderGuideBlock's drawing formula
--   exactly: column width = (key side) + (key-to-label gap = half_line/2) +
--   (longest label width) + (right padding = half_line). If the title is
--   wider, the title width wins instead. Arguments: spec (so key
--   width/gap are drawn from base-derived effective values), item font pt,
--   title font pt, title string, and labels. "Longest" is measured by
--   maximum 'textWidthEm', not character count (since fullwidth mixing can
--   reverse the ordering, width is used).
legendGuideWidth :: VisualSpec -> Double -> Double -> Text -> [Text] -> Double
legendGuideWidth spec fItem fTitle title labels = max titleW colW
  where
    hl         = effectiveHalfLine spec
    maxLabelEm = maximum (0 : map textWidthEm labels)
    colW       = effectiveLegendKeyW spec + hl / 2 + fItem * maxLabelEm + hl
    titleW     = fTitle * textWidthEm title

-- ===========================================================================
-- 凡例ラベル収集 (Phase 35 で導入・Phase 38 で Render/Layer から Layout へ集約)。
--   ★Layout の legendW 予約と Render の renderGuideBlock 描画が**同一関数**でラベル文字列を
--   得るための単一情報源。 別実装だとラベル文字列がズレ予約幅≠描画幅になる。
-- ===========================================================================

-- | [日本語]: 数値 → 表示文字列。 浮動小数点アーチファクト (0.1+0.2=0.300…04
--   等) を 12 桁 round で回避。 整数なら trailing zero / decimal point を
--   除去。 (旧 Render.Common.numToText)
--   [English]: Converts a number to a display string. Avoids
--   floating-point artifacts (0.1+0.2=0.300…04, etc.) by rounding to 12
--   digits. Trailing zeros and the decimal point are stripped for integer
--   values. (Migrated from the previous Render.Common.numToText.)
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

-- | [日本語]: 順序保存 nub (初出順)。 glyph 色 (@colorVector@ の nub) / PS
--   (Array.nub) と揃える。
--   [English]: An order-preserving nub (first-occurrence order), matching
--   the nub used by glyph color (@colorVector@) and PureScript's
--   (Array.nub) behaviour.
nubKeep :: [Text] -> [Text]
nubKeep = nub

-- | [日本語]: 色 aesthetic を持つ最初のレイヤの ColorEnc (categorical /
--   continuous)。
--   [English]: The ColorEnc (categorical or continuous) of the first layer
--   that has a color aesthetic.
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

-- | [日本語]: 明示凡例タイトル (vsLegendTitle = scale name / labs(color=))。
--   未指定なら ""。
--   [English]: The explicit legend title (vsLegendTitle: a scale name or
--   labs(color=)). Empty string "" when unset.
effectiveLegendTitle :: VisualSpec -> Text
effectiveLegendTitle spec = maybe "" id (getLast (vsLegendTitle spec))

-- | [日本語]: 凡例キーの表示順。 (originalIndex, label) を返し、 色は
--   originalIndex で引く (= reverse しても各キーの色は固定)。
--   vsLegendReverse=True で逆順。 ★Render/Layer から移設 (auto-wrap の列幅
--   計算が表示順に依存するため予約 computeLayout と描画で共有 = 単一情報源)。
--   [English]: The display order of legend keys. Returns
--   (originalIndex, label); color is looked up by originalIndex, so each
--   key's color stays fixed even when reversed. Reversed when
--   vsLegendReverse=True. Migrated from Render/Layer, since auto-wrap's
--   column-width computation depends on display order and must be shared
--   between reservation (computeLayout) and rendering (a single source of
--   truth).
legendOrder :: VisualSpec -> [Text] -> [(Int, Text)]
legendOrder spec vals =
  let ix = zip [0 ..] vals
  in if getLast (vsLegendReverse spec) == Just True then reverse ix else ix

-- | [日本語]: 全 ColorByCol レイヤのカテゴリを順序保存で union (= 凡例 swatch /
--   glyph 色の正本)。 明示 @colorCats@ があればそれを先頭に、 無ければデータ
--   水準を 'orderedCats' 順で。
--   [English]: The order-preserving union of categories across all
--   ColorByCol layers (the source of truth for legend swatches / glyph
--   colors). If explicit @colorCats@ are given, they come first; otherwise,
--   data levels are used in 'orderedCats' order.
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

-- | [日本語]: 凡例 guide (色 / 形)。 描画 (renderGuideBlock) と予約 (legendW)
--   が共有。
--   [English]: A legend guide (color or shape). Shared between rendering
--   (renderGuideBlock) and reservation (legendW).
data LegendGuide
  = ColorGuide !ColorEnc      -- [日本語]: 色 guide (categorical / continuous)。 [English]: A color guide (categorical or continuous).
  | ShapeGuide !ColRef        -- [日本語]: 形 guide (色とは別列・または色無しのとき)。 [English]: A shape guide (a column distinct from color, or used when there is no color).
  | CountBarGuide !Double !Double  -- [日本語]: ★ Phase 40: 件数 colorbar (lo,hi)。 hexbin/bin2d-count 用 (列でなく集計値ゆえ ColorByContinuous と別。 ラベル = "count")。
                                   -- [English]: A count colorbar (lo,hi), for hexbin/bin2d-count (distinct from ColorByContinuous since it is an aggregate rather than a column; labeled "count").

-- | [日本語]: spec から guide を ggplot 順 (color → shape) で収集。 形が色と
--   同列なら統合し形 guide なし。
--   [English]: Collects guides from a spec in ggplot order (color, then
--   shape). If shape shares its column with color, they are merged and no
--   separate shape guide is produced.
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

-- | [日本語]: spec 中の hexbin layer の件数域 (min,max)。 colorbar guide +
--   needsLegend が使う。 render (renderHexbin) と同じ 'hexbinLayerCells' で
--   計算するので域が一致する。
--   [English]: The count domain (min, max) of the hexbin layer in a spec,
--   used by the colorbar guide and needsLegend. Computed with the same
--   'hexbinLayerCells' as render (renderHexbin), so the domains agree.
hexbinCountDomain :: Resolver -> VisualSpec -> Maybe (Double, Double)
hexbinCountDomain r spec =
  case [ l | l <- vsLayers spec, getFirst (lyKind l) == Just MHexbin ] of
    (l : _) -> case map hexCount (hexbinLayerCells r l) of
      [] -> Nothing
      cs -> Just (fromIntegral (minimum cs), fromIntegral (maximum cs))
    _ -> Nothing

-- | [日本語]: 凡例を実際に描画する位置 (= None なら凡例なし。 PS Layout と
--   同一)。 color encoding が無ければ位置指定があっても None。 予約
--   (computeLayout) / 描画 (Render) の両方がこれを使い、 「予約したのに
--   描かれない / 描いたのに予約してない」 ズレを防ぐ。
--   [English]: The position at which the legend is actually drawn (None
--   means no legend; identical to the PureScript Layout). Without a color
--   encoding, this is None even if a position was requested. Both
--   reservation (computeLayout) and rendering (Render) use this, preventing
--   the mismatch of "reserved but not drawn" or "drawn but not reserved".
needsLegend :: VisualSpec -> LegendPosition -> LegendPosition
needsLegend spec pos
  | pos == LegendNone                = LegendNone
  -- ★ Phase 35: 形のみ (shapeBy・色無し) でも凡例を出す (= ggplot shape guide)。
  -- ★ Phase 40: hexbin (件数 colorbar) も色 enc 無しで凡例を出す。
  | hasColorEncoding (vsLayers spec)
    || hasShapeEncoding (vsLayers spec)
    || hasHexbinCountGuide spec       = pos
  | otherwise                        = LegendNone

-- | [日本語]: 色 enc を持たない hexbin layer (= 件数 colorbar 駆動) があるか
--   (構造のみ)。
--   [English]: Whether there is a hexbin layer without a color encoding
--   (driven by a count colorbar); a structural check only.
hasHexbinCountGuide :: VisualSpec -> Bool
hasHexbinCountGuide spec =
  not (hasColorEncoding (vsLayers spec))
  && any (\l -> getFirst (lyKind l) == Just MHexbin) (vsLayers spec)

-- | [日本語]: layer 群に shape aesthetic (lyShapeBy) があるか。
--   [English]: Whether any layer in the group has a shape aesthetic
--   (lyShapeBy).
hasShapeEncoding :: [Layer] -> Bool
hasShapeEncoding = any (\l -> case getLast (lyShapeBy l) of
                                Just _  -> True
                                Nothing -> False)

-- | [日本語]: 有効 legend position を解決。 優先順 = 図レベル vsLegend
--   (@legendPos@ setter) > theme (toLegendPos) > 既定 LegendRightCenter
--   (= ggplot legend.position="right" と同じ縦中央)。
--   [English]: Resolves the effective legend position. Priority: the
--   figure-level vsLegend (the @legendPos@ setter) > theme (toLegendPos) >
--   the default LegendRightCenter (matching ggplot's vertically centered
--   legend.position="right").
effectiveLegendPos :: VisualSpec -> LegendPosition
effectiveLegendPos spec = case getLast (vsLegend spec) of
  Just l  -> lgPosition l
  Nothing -> maybe LegendRightCenter id
               (getLast (toLegendPos (vsThemeOverride spec)))

-- | [日本語]: 実効 tick 長 (pt)。 theme (toTickLength) > 既定 half_line/2
--   (ggplot axis.ticks.length。 ★固定 'ggTickLen' 2.75 から base 派生へ、
--   既定 11 で bit 同値。 ★ThemeVoid のみ既定 0 = ggplot theme_void の
--   axis.ticks.length = 0)。
--   [English]: The effective tick length (pt). Priority: theme
--   (toTickLength) > the default half_line/2 (ggplot's
--   axis.ticks.length; changed from the fixed 'ggTickLen' 2.75 to a
--   base-derived value, bit-identical at the default of 11. ThemeVoid
--   alone defaults to 0, matching ggplot theme_void's
--   axis.ticks.length = 0).
effectiveTickLength :: VisualSpec -> Double
effectiveTickLength spec =
  maybe def id (getLast (toTickLength (vsThemeOverride spec)))
  where def = if isVoidTheme spec then 0 else effectiveHalfLine spec / 2

-- | [日本語]: theme preset が ThemeVoid か (void 系の既定分岐用)。
--   [English]: Whether the theme preset is ThemeVoid (used to branch on
--   void-family defaults).
isVoidTheme :: VisualSpec -> Bool
isVoidTheme spec = getLast (vsTheme spec) == Just ThemeVoid

-- | [日本語]: 実効 axis.text (目盛ラベル文字) 表示。 theme (toShowAxisText) >
--   preset 既定 (ThemeVoid のみ False = ggplot theme_void の axis.text
--   element_blank)。 表示 off は tick ラベル分の margin 予約 (axTextMar /
--   xTickReserve / maxYTickW) に波及するため、 computeLayout (予約) と
--   Render.tickMarks (描画) の単一情報源。
--   [English]: Whether axis.text (tick label text) is effectively shown.
--   Priority: theme (toShowAxisText) > the preset default (False only for
--   ThemeVoid, matching ggplot theme_void's axis.text element_blank).
--   Turning display off cascades into the tick-label margin reservation
--   (axTextMar / xTickReserve / maxYTickW), so this is the single source of
--   truth shared by computeLayout (reservation) and Render.tickMarks
--   (drawing).
effectiveShowAxisText :: VisualSpec -> Bool
effectiveShowAxisText spec =
  maybe (not (isVoidTheme spec)) id (getLast (toShowAxisText (vsThemeOverride spec)))

-- | [日本語]: 実効 axis.title (軸タイトル) 表示。 既定は
--   'effectiveShowAxisText' と同じ規則 (ThemeVoid のみ False)。
--   computeLayout (予約) と Render.labels (描画) の単一情報源。
--   [English]: Whether axis.title is effectively shown. The default
--   follows the same rule as 'effectiveShowAxisText' (False only for
--   ThemeVoid). A single source of truth shared by computeLayout
--   (reservation) and Render.labels (drawing).
effectiveShowAxisTitle :: VisualSpec -> Bool
effectiveShowAxisTitle spec =
  maybe (not (isVoidTheme spec)) id (getLast (toShowAxisTitle (vsThemeOverride spec)))

-- | [日本語]: 実効 tick 向き。 theme (toTickDir) > 既定 'TickOut' (ggplot 既定
--   = 外向き)。
--   [English]: The effective tick direction. Priority: theme (toTickDir) >
--   the default 'TickOut' (ggplot's default, pointing outward).
effectiveTickDir :: VisualSpec -> TickDir
effectiveTickDir spec =
  maybe TickOut id (getLast (toTickDir (vsThemeOverride spec)))

-- | [日本語]: tick の panel 外向き突出量 (pt)。 margin 予約 (computeLayout) と
--   軸ラベル offset (Render.tickMarks) の単一情報源。 'TickIn' は panel 外に
--   出ないので 0 (= ラベルが軸に寄る、 ggplot の負 axis.ticks.length と同挙動)。
--   [English]: The amount a tick protrudes outward from the panel (pt). A
--   single source of truth shared by margin reservation (computeLayout) and
--   the axis-label offset (Render.tickMarks). 'TickIn' does not protrude
--   past the panel, so this is 0 (labels sit close to the axis, matching
--   ggplot's behaviour with a negative axis.ticks.length).
tickOutwardLen :: VisualSpec -> Double
tickOutwardLen spec = case effectiveTickDir spec of
  TickIn -> 0
  _      -> effectiveTickLength spec

-- | [日本語]: 実効 plot margin (pt)。 theme (toPlotMargin) > 既定 各辺
--   half_line (★固定 'ggHalfLine' 5.5 から base 派生へ、 既定 11 で bit 同値)。
--   指定時は外周分を __置き換える__ (ggplot plot.margin と同じ)。 軸ラベル・
--   title 帯・凡例などの内側予約は従来どおり自動算出のまま (computeLayout と
--   Render.labels が共有)。
--   [English]: The effective plot margin (pt). Priority: theme
--   (toPlotMargin) > the default, half_line on each side (changed from
--   the fixed 'ggHalfLine' 5.5 to a base-derived value, bit-identical at
--   the default of 11). When specified, it __replaces__ the outer margin
--   entirely (matching ggplot's plot.margin). Inner reservations for axis
--   labels, the title band, the legend, etc. remain auto-computed as before
--   (shared by computeLayout and Render.labels).
effectivePlotMargin :: VisualSpec -> Margin
effectivePlotMargin spec =
  let hl = effectiveHalfLine spec
  in maybe (Margin hl hl hl hl) id
           (getLast (toPlotMargin (vsThemeOverride spec)))

-- | [日本語]: 実効 base font size (pt)。 theme (toBaseFontSize) > 既定 11
--   (ggplot theme_grey base_size)。 各 slot の既定 font size はこれからの
--   相対倍率で派生する。 computeLayout (予約) と Render.mkFontTS (描画) の
--   単一情報源。
--   [English]: The effective base font size (pt). Priority: theme
--   (toBaseFontSize) > the default 11 (ggplot theme_grey's base_size). Each
--   slot's default font size is derived from this by a relative multiplier.
--   A single source of truth shared by computeLayout (reservation) and
--   Render.mkFontTS (drawing).
effectiveBaseFontSize :: VisualSpec -> Double
effectiveBaseFontSize spec =
  maybe 11 id (getLast (toBaseFontSize (vsThemeOverride spec)))

-- | [日本語]: 実効 half_line (pt) = base/2 (ggplot @half_line@)。 spacing 系
--   (外周 margin・title 下 margin・panel.spacing・凡例 gap) の共通派生元。
--   既定 base 11 で 5.5 = 従来 'ggHalfLine' と bit 同値 (golden 不変 gate、
--   ULP 検証済)。
--   [English]: The effective half_line (pt), = base/2 (ggplot's
--   @half_line@). The common derivation source for spacing values (outer
--   margin, title bottom margin, panel.spacing, legend gap). At the default
--   base of 11, this is 5.5, bit-identical to the previous 'ggHalfLine'
--   (verified to ULP precision via a golden-invariance gate).
effectiveHalfLine :: VisualSpec -> Double
effectiveHalfLine spec = effectiveBaseFontSize spec / 2

-- | [日本語]: 実効 axis.text margin (pt) = 0.8 × half_line/2 (ggplot 忠実)。
--   既定 11 で 2.2 = 従来 'ggAxTextMar' と bit 同値。
--   [English]: The effective axis.text margin (pt), = 0.8 × half_line/2
--   (faithful to ggplot). At the default of 11, this is 2.2, bit-identical
--   to the previous 'ggAxTextMar'.
effectiveAxTextMar :: VisualSpec -> Double
effectiveAxTextMar spec = 0.8 * (effectiveHalfLine spec / 2)

-- | [日本語]: 実効 axis.title margin (pt) = half_line/2 (ggplot 忠実)。 既定
--   11 で 2.75 = 従来 'ggAxTitleMar' と bit 同値。
--   [English]: The effective axis.title margin (pt), = half_line/2
--   (faithful to ggplot). At the default of 11, this is 2.75, bit-identical
--   to the previous 'ggAxTitleMar'.
effectiveAxTitleMar :: VisualSpec -> Double
effectiveAxTitleMar spec = effectiveHalfLine spec / 2

-- | [日本語]: 実効凡例ベースフォント (pt) = 2 × half_line = base (ggplot
--   @base_size@ と一致)。 既定 11 で従来 'legendBaseSize' と bit 同値。
--   [English]: The effective legend base font size (pt), = 2 × half_line =
--   base (matching ggplot's @base_size@). At the default of 11,
--   bit-identical to the previous 'legendBaseSize'.
effectiveLegendBaseSize :: VisualSpec -> Double
effectiveLegendBaseSize spec = 2 * effectiveHalfLine spec

-- | [日本語]: 実効凡例キー 1 辺 (pt) = 1.2 lines (行高 1.3133 倍率は
--   'legendKeyW' と同一)。 既定 11 で bit 同値。 pitch = keyW (キーセル隣接)。
--   ★theme (toLegendKeySize、 ggplot legend.key.size 相当) が最優先。
--   cowplot preset は 1.1 × font_size を焼き込む (gold 実測: base14 = 15.4pt
--   = 32px)。
--   [English]: The effective legend key side length (pt), = 1.2 lines (the
--   1.3133 line-height multiplier matches 'legendKeyW'). Bit-identical at
--   the default of 11. pitch = keyW (key cells adjacent). theme
--   (toLegendKeySize, corresponding to ggplot's legend.key.size) takes
--   priority. The cowplot preset bakes in 1.1 × font_size (measured against
--   gold: base14 = 15.4pt = 32px).
effectiveLegendKeyW :: VisualSpec -> Double
effectiveLegendKeyW spec =
  maybe (1.2 * effectiveLegendBaseSize spec * 1.3133) id
        (getLast (toLegendKeySize (vsThemeOverride spec)))

effectiveLegendKeyPitch :: VisualSpec -> Double
effectiveLegendKeyPitch = effectiveLegendKeyW

-- | [日本語]: 実効 subtitle / caption / tag font size (pt) = base 派生
--   (ggplot theme_grey の倍率: plot.subtitle ×1 / plot.caption ×0.8 /
--   plot.tag ×1.2)。 旧固定 11/9/13 は base 11 の丸め値 (caption 8.8→9 /
--   tag 13.2→13) だったのを ggplot 忠実の派生式へ。 computeLayout (labs
--   予約) と Render.labels (描画) の単一情報源。
--   [English]: The effective subtitle / caption / tag font size (pt),
--   derived from base (ggplot theme_grey's multipliers: plot.subtitle ×1,
--   plot.caption ×0.8, plot.tag ×1.2). Replaces the old fixed 11/9/13
--   (rounded values of base 11: caption 8.8→9, tag 13.2→13) with a formula
--   faithful to ggplot. A single source of truth shared by computeLayout
--   (labs reservation) and Render.labels (drawing).
effectiveSubtitleSize :: VisualSpec -> Double
effectiveSubtitleSize = effectiveBaseFontSize

effectiveCaptionSize :: VisualSpec -> Double
effectiveCaptionSize spec = 0.8 * effectiveBaseFontSize spec

effectiveTagSize :: VisualSpec -> Double
effectiveTagSize spec = 1.2 * effectiveBaseFontSize spec

-- | [日本語]: slot の実効 font size (pt)。 解決順は Render.mkFontTS と同一 =
--   theme override (fsSize) > font setter (fsSize) > 既定 (base 派生)。
--   setter と override は Maybe FontSpec の field-wise merge (override の
--   Just が優先)。
--   [English]: The effective font size (pt) of a slot. Resolution order
--   matches Render.mkFontTS: theme override (fsSize) > the font setter
--   (fsSize) > the default (base-derived). The setter and override are
--   field-wise merged as Maybe FontSpec (a Just in override takes
--   priority).
effectiveFontSize :: Last FontSpec -> Last FontSpec -> Double -> Double
effectiveFontSize setterL overrideL def =
  case getLast setterL <> getLast overrideL of
    Just fs -> maybe def id (getLast (fsSize fs))
    Nothing -> def

-- | [日本語]: layer 群に color/fill aesthetic (ColorByCol / ColorByContinuous)
--   があるか。
--   [English]: Whether any layer in the group has a color/fill aesthetic
--   (ColorByCol or ColorByContinuous).
hasColorEncoding :: [Layer] -> Bool
hasColorEncoding = any (\l -> case getLast (lyColor l) of
  Just (ColorByCol _)        -> True
  Just (ColorByContinuous _) -> True
  _                          -> False)

-- | [日本語]: 軸 tick ラベルを ggplot / base-R @format()@ 準拠で __ベクトル整形__
--   する。 ggplot の連続スケール既定 (@labels = waiver()@) は break ベクトル
--   全体に base R @format()@ を掛ける。 その挙動を再現:
--
--     1. 全 break で__小数桁を統一__する (末尾ゼロを残す)。 例 0,.25,.5 →
--        "0.00","0.25","0.50" (旧 numToText は単値ごとにゼロ削りして
--        "0.5" になっていた)。
--     2. __固定小数 vs 指数__を「最大幅が短い方」で選ぶ (base R
--        @scipen = 0@: 固定表記が指数表記より広いときだけ指数にする)。 例
--        density の 0..5e-4 は固定 "0.0005"(6字) > 指数 "5e-04"(5字) ゆえ
--        "0e+00".."5e-04"、 0..1 は固定 "0.50"(4字) ≤ 指数 "5e-01"(5字) ゆえ
--        "0.00".."1.00"。
--
--   R @ggplot_build@ 実測値と一致することを確認済 (density y / 0..1 比率 y /
--   3000..6000 x)。
--   [English]: Vector-formats axis tick labels following ggplot / base-R
--   @format()@. ggplot's default for continuous scales (@labels = waiver()@)
--   applies base R's @format()@ to the whole break vector. This reproduces
--   that behaviour:
--
--     1. Uses __the same decimal digit count for every break__ (keeping
--        trailing zeros). E.g. 0,.25,.5 becomes "0.00","0.25","0.50"
--        (the previous 'numToText' stripped zeros per value,
--        producing "0.5").
--     2. Chooses __fixed decimal vs. exponential__ by whichever has the
--        shorter maximum width (matching base R's @scipen = 0@: switches to
--        exponential only when fixed notation is wider). E.g. for density's
--        0..5e-4, fixed "0.0005" (6 chars) is wider than exponential
--        "5e-04" (5 chars), so "0e+00".."5e-04" is used; for 0..1, fixed
--        "0.50" (4 chars) is no wider than exponential "5e-01" (5 chars),
--        so "0.00".."1.00" is used.
--
--   Verified to match measured R @ggplot_build@ output (density y, 0..1
--   ratio y, 3000..6000 x).
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

-- | [日本語]: v を誤差なく表すのに要する小数桁 (0..10)。 nice tick 前提で
--   10 桁上限。
--   [English]: The decimal digits (0..10) needed to represent v without
--   error. Capped at 10 digits, assuming nice ticks.
decimalsNeeded :: Double -> Int
decimalsNeeded v = go 0
  where
    go k | k >= 10                              = 10
         | abs (v - rounded k) <= 1e-9 * max 1 (abs v) = k
         | otherwise                            = go (k + 1)
    rounded k = let tk = 10 ^^ k :: Double
                in fromIntegral (round (v * tk) :: Integer) / tk

-- | [日本語]: v を仮数 m∈[1,10) と指数 e に正規化 (v = m * 10^e)。 0 は
--   (0,0)。
--   [English]: Normalizes v to a mantissa m∈[1,10) and an exponent e
--   (v = m * 10^e). 0 becomes (0,0).
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

-- | [日本語]: 指数表記 1 個 (仮数 d 桁 + "e±NN")。
--   [English]: A single exponential-notation string (a d-digit mantissa
--   plus "e±NN").
sciStr :: Int -> Double -> Text
sciStr d v =
  let (m, e) = sciParts v
      mant   = showFFloat (Just d) m ""
      sign   = if e < 0 then "-" else "+"
      ae     = abs e
      expt   = (if ae < 10 then "0" else "") ++ show ae
  in T.pack (mant ++ "e" ++ sign ++ expt)

-- | [日本語]: Categorical axis labels (= ColTxt の distinct 値、 layer 横断)。
--   どの encoding (encX / encY) を見るかは accessor 引数で指定。
--
--   既定順を ggplot2 の factor 既定と同じ __アルファベット順__ ('orderedCats')
--   にした (= R4DS と凡例・色・軸並びを一致させる)。 明示順が要るときは
--   @scale_x_discrete(limits=)@ 相当の discrete-limits override (第 4 引数) を
--   渡す (= fct_infreq / fct_reorder 相当)。 override 指定時はデータ内に
--   在る水準だけをその順で返す (applyDiscreteLimits がデータ側を既に
--   filter/並べ替え済)。
--   [English]: Categorical axis labels (the distinct ColTxt values, across
--   layers). Which encoding (encX / encY) is inspected is chosen by the
--   accessor argument.
--
--   The default order matches ggplot2's default factor order, __alphabetical__
--   ('orderedCats'), keeping legend/color/axis ordering consistent with
--   R4DS. When an explicit order is needed, pass a discrete-limits override
--   (the 4th argument) equivalent to @scale_x_discrete(limits=)@ (comparable
--   to fct_infreq / fct_reorder). When an override is given, only the
--   levels present in the data are returned, in that order
--   (applyDiscreteLimits has already filtered/reordered the data side).
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

-- | [日本語]: scale の range (rLo/rHi) を入替えて軸反転。 domain は不変なので
--   tick (= domain 値) は scaleApply 経由で自動的に逆向き座標へ写る。 全
--   Scale variant が lsRangeLo/lsRangeHi を共有するため record update 1 つで
--   賄える。
--   [English]: Reverses an axis by swapping the scale's range (rLo/rHi).
--   Since the domain is unchanged, ticks (domain values) map automatically
--   to reversed coordinates via scaleApply. All Scale variants share
--   lsRangeLo/lsRangeHi, so a single record update suffices.
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

-- | [日本語]: 'Pos' を pt 座標へ解決する context。panel rect と x/y scale を
--   与える。
--   [English]: The context for resolving a 'Pos' to pt coordinates. Supplies
--   the panel rect and the x/y scales.
data UCtx = UCtx
  { uDpi    :: !Double   -- ^ [日本語]: PAbs の Px を pt 化する dpi。 [English]: The dpi used to convert PAbs's Px to pt.
  , uRect   :: !Rect     -- ^ [日本語]: panel rect (pt)。PNpc 解決に使う。 [English]: The panel rect (pt), used to resolve PNpc.
  , uXScale :: !Scale    -- ^ [日本語]: PNative (x) 解決。 [English]: Used to resolve PNative (x).
  , uYScale :: !Scale    -- ^ [日本語]: PNative (y) 解決。 [English]: Used to resolve PNative (y).
  } deriving (Show, Eq)

-- | [日本語]: x 座標の 'Pos' を pt へ。PNpc 0=左端 (rX), 1=右端 (rX+rW)。
--   [English]: Resolves an x-coordinate 'Pos' to pt. For PNpc, 0 is the
--   left edge (rX) and 1 is the right edge (rX+rW).
resolvePosX :: UCtx -> Pos -> Double
resolvePosX c p = case p of
  PAbs len  -> rX (uRect c) + lengthToPt (uDpi c) len
  PNpc t    -> rX (uRect c) + t * rW (uRect c)
  PNative v -> scaleApply (uXScale c) v

-- | [日本語]: y 座標の 'Pos' を pt へ。device 座標は y 下向き (rY=上端) ゆえ
--   PNpc 1=上端 (rY), 0=下端 (rY+rH)。PNative は反転済 scale が処理。
--   [English]: Resolves a y-coordinate 'Pos' to pt. Since device coordinates
--   point downward (rY is the top edge), PNpc 1 is the top edge (rY) and 0
--   is the bottom edge (rY+rH). PNative is handled by the already-flipped
--   scale.
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

-- | [日本語]: spec の座標系 (Nothing = Cartesian)。
--   [English]: A spec's coordinate system (Nothing means Cartesian).
coordOf :: VisualSpec -> Coord
coordOf spec = maybe CoordCartesian id (getLast (vsCoord spec))

-- | [日本語]: データ空間 (dx, dy) → px (横, 縦)。 Cartesian は (sx dx, sy dy)、
--   Flip はデータ x を縦 px・データ y を横 px に (= 軸入替)。
--   [English]: Maps data space (dx, dy) to px (horizontal, vertical).
--   Cartesian is (sx dx, sy dy); Flip maps data x to vertical px and data y
--   to horizontal px (swapping the axes).
projectXY :: Coord -> Layout -> Double -> Double -> (Double, Double)
projectXY CoordCartesian l dx dy =
  (scaleApply (lpXScale l) dx, scaleApply (lpYScale l) dy)
projectXY CoordFlip l dx dy =
  (scaleApply (lpYScaleFlipped l) dy, scaleApply (lpXScaleFlipped l) dx)
-- Phase 11 A7-c: 極座標。 theta 軸 (PolarX=x / PolarY=y) を角度 (0..2π、 上始点・
--   時計回り; start/direction は Phase 64 A16 で可変)、 他軸を半径 (中心=domain
--   下端、 外周=domain 上端) に写す。
projectXY (CoordPolarX _) l dx dy = polarPoint l (domFrac (lpXScale l) dx) (domFrac (lpYScale l) dy)
projectXY (CoordPolarY _) l dx dy = polarPoint l (domFrac (lpYScale l) dy) (domFrac (lpXScale l) dx)
-- ★ Phase 64 §3 (A12) で ternaryPoint による正三角座標を実装予定。 A10 時点では
--   投影は未実装のため Cartesian に fallback する placeholder (ternary を当てた図は
--   まだ無い = render される経路が無い)。
projectXY CoordTernary l dx dy =
  (scaleApply (lpXScale l) dx, scaleApply (lpYScale l) dy)

-- | [日本語]: scale の domain における正規化位置 [0,1] (= (v - dLo)/(dHi -
--   dLo))。 極座標で角度/半径の比率を出すのに使う。 domain が退化していれば
--   0。
--   [English]: The normalized position [0,1] within a scale's domain
--   ((v - dLo)/(dHi - dLo)). Used to compute angle/radius ratios in polar
--   coordinates. Returns 0 if the domain is degenerate.
domFrac :: Scale -> Double -> Double
domFrac s v = let lo = lsDomainLo s; hi = lsDomainHi s
              in if hi == lo then 0 else (v - lo) / (hi - lo)

-- | [日本語]: 極座標の中心とデータ最大半径。
--
--   ★ Phase 64 A8: 半径は **ggplot2 の npc 定数に合わせる** (それまでは
--   @min(w,h)/2@ = panel 内接円で、 外周が panel の縁にぴったり接するため
--   θ ラベル (外周のやや外) が必ず panel の外へ出てタイトルと重なっていた)。
--   ggplot2 @coord-polar.R@ (2026-08-06 時点 main) の実装:
--
--     * @r_rescale(x, range, donut = c(0, 0.4))@ (287-290 行) =
--       __データの最大半径は npc 0.4__
--     * @render_fg@ (240-249 行) が θ ラベルを @0.45 * sin/cos + 0.5@ =
--       __npc 半径 0.45__ に中心揃えで置く
--     * panel 矩形の縁は中心から npc 0.5
--
--   つまり 0.4 (データ) \< 0.45 (θ ラベル・外周円) \< 0.5 (panel 縁) で、
--   ggplot2 は__ラベルを panel の内側に収めることで余白予約を不要にしている__
--   (@render_axis_h@ は空軸を返すだけ・@layout.R@ に θ 用の帯予約は無い)。
--   本実装もこれに倣うので 'computeLayout' は座標系を見る必要が無い。
--
--   [English]: The center and maximum data radius of polar coordinates.
--   Phase 64 A8 switched the radius to ggplot2's npc constants (it used to
--   be @min(w,h)/2@, the circle inscribed in the panel, which forced the
--   theta labels just outside it to leave the panel and collide with the
--   title). In ggplot2's @coord-polar.R@: data is rescaled into the donut
--   @c(0, 0.4)@, theta labels are centered at npc radius 0.45, and the
--   panel edge is at 0.5 — so labels stay inside the panel and no margin
--   reservation is needed. We follow the same ratios.
polarCenter :: Layout -> (Double, Double, Double)
polarCenter l = let a = lpPlotArea l
                    cx = rX a + rW a / 2
                    cy = rY a + rH a / 2
                    maxR = 0.4 * min (rW a) (rH a)
                in (cx, cy, maxR)

-- | [日本語]: θ ラベルを置く半径を、 データ最大半径 ('polarCenter' の maxR) との比で
--   表したもの = @0.45 / 0.4 = 1.125@。 ggplot2 の npc 定数から導出
--   (根拠は 'polarCenter' の注記)。 境界円 (= clip 境界) は maxR のままなので、
--   ラベルは境界円の少し外・panel の内側という位置になる。
--   [English]: The radius at which theta labels sit, as a ratio to the max
--   data radius: @0.45 / 0.4 = 1.125@, derived from ggplot2's npc constants
--   (see the note on 'polarCenter'). The boundary circle (and the clip
--   boundary) stays at maxR, so labels land just outside it but still
--   inside the panel.
polarOuterFrac :: Double
polarOuterFrac = 0.45 / 0.4

-- | [日本語]: 境界円 (= データ最大半径の円) を多角形近似した clip path
--   (Phase 64 A8 = B-2)。 @PClipPath@ に渡して「外周円の外に glyph が
--   描かれない」 を実現する。 180 分割 (2°刻み) で、 半径 200px でも矢高誤差は
--   0.03px 未満。
--   [English]: A polygonal approximation of the boundary circle (at the max
--   data radius), for use with @PClipPath@ (Phase 64 A8 / B-2) so that no
--   glyph is drawn outside it. 180 segments (2 degrees each): the sagitta
--   error stays below 0.03px even at a 200px radius.
polarClipPath :: Layout -> [(Double, Double)]
polarClipPath l =
  [ polarPoint l (fromIntegral i / n) 1.0 | i <- [0 .. round n - 1 :: Int] ]
  where n = 180 :: Double

-- | [日本語]: 極座標の開始角/回転方向を Layout の 'lpCoord' から取り出す
--   (= Phase 64 A16 = coord_polar(start=, direction=))。 polar でなければ既定
--   (start=0, dir=+1)。 投影が 'polarPoint' 1 箇所に閉じているので、 grid /
--   スポーク / θ ラベルもこの opts を通る。
--   [English]: Extracts the polar start angle / direction from the Layout's
--   'lpCoord' (Phase 64 A16 = coord_polar(start=, direction=)). Returns the
--   default (start=0, dir=+1) for non-polar coords. Since projection is
--   confined to 'polarPoint', grid / spokes / theta labels all honor these.
polarOptsOf :: Coord -> (Double, Double)
polarOptsOf (CoordPolarX o) = (polarStart o, polarDirection o)
polarOptsOf (CoordPolarY o) = (polarStart o, polarDirection o)
polarOptsOf _               = (0, 1)

-- | [日本語]: (角度 frac, 半径 frac) → px。 角度は 'lpCoord' の 'PolarOpts' で
--   @theta = start + dir * frac * 2π@ (既定 start=0=真上・dir=+1=時計回り)、
--   半径 frac=1 が外周。
--   [English]: Maps (angle fraction, radius fraction) to px. The angle is
--   @theta = start + dir * frac * 2π@ from the 'PolarOpts' in 'lpCoord'
--   (default start=0 = top, dir=+1 = clockwise); radius fraction 1 is the
--   outer edge.
polarPoint :: Layout -> Double -> Double -> (Double, Double)
polarPoint l thetaFrac rFrac =
  let (cx, cy, maxR) = polarCenter l
      (start, dir)   = polarOptsOf (lpCoord l)
      theta = start + dir * thetaFrac * 2 * pi
      r     = rFrac * maxR
  in (cx + r * sin theta, cy - r * cos theta)

-- | [日本語]: データ空間の矩形 (x/y の min/max) → px Rect。 Flip では bbox が
--   縦横転置される。 2 隅を projectXY して min/abs で正規化するだけ (= 向きに
--   依らず正しい Rect)。
--   [English]: Maps a data-space rectangle (x/y min/max) to a px Rect. Under
--   Flip, the bbox is transposed. Simply projects two corners via projectXY
--   and normalizes with min/abs (correct regardless of orientation).
projectRectData :: Coord -> Layout -> Double -> Double -> Double -> Double -> Rect
projectRectData c l xminD xmaxD yminD ymaxD =
  let (x0, y0) = projectXY c l xminD yminD
      (x1, y1) = projectXY c l xmaxD ymaxD
  in Rect (min x0 x1) (min y0 y1) (abs (x1 - x0)) (abs (y1 - y0))

-- | [日本語]: bar/box 用: 中心線の data 座標 (centerD = x 群位置) と
--   base..value の data 区間、 厚み thicknessPx (= px 単位の bar 幅) から px
--   Rect を作る。 Cartesian では横位置 = centerD ± 厚み/2、 縦 = base..value。
--   Flip では縦位置 = centerD ± 厚み/2、 横 = base..value (= 厚みは常に px の
--   まま = 軸スケールに依らない)。
--   [English]: For bar/box: builds a px Rect from the centerline's data
--   coordinate (centerD, the x-group position), the base..value data
--   interval, and the thickness thicknessPx (the bar width in px). Under
--   Cartesian, horizontal position = centerD ± thickness/2 and
--   vertical = base..value. Under Flip, vertical position =
--   centerD ± thickness/2 and horizontal = base..value (thickness always
--   stays in px, independent of the axis scale).
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
-- 極座標の bar は wedge (扇形) で描くため Rect では表せない。
--   renderBar が極座標を検出して PPath で arc を描く (= projectBarRect は使わない)。
--   ここは totality 維持のための placeholder (Cartesian 同式・極座標 bar 経路では未使用)。
projectBarRect (CoordPolarX _) l centerD baseD valueD thicknessPx =
  projectBarRect CoordCartesian l centerD baseD valueD thicknessPx
projectBarRect (CoordPolarY _) l centerD baseD valueD thicknessPx =
  projectBarRect CoordCartesian l centerD baseD valueD thicknessPx
-- ★ Phase 64 §3 (A12) までは ternary も Cartesian placeholder (未 render 経路)。
projectBarRect CoordTernary l centerD baseD valueD thicknessPx =
  projectBarRect CoordCartesian l centerD baseD valueD thicknessPx

-- | [日本語]: 扇形 (annular sector) の path。 (tf0..tf1) = 角度 frac 帯、
--   (rf0..rf1) = 半径 frac 帯。 円弧は 0.1 rad 刻みの折線近似 (nSeg ≥ 2)。
--   polar bar (rose/pie) と 'projectBar' が共有する。
--   [English]: The path of an annular sector. (tf0..tf1) is the angular
--   fraction band and (rf0..rf1) the radial fraction band. Arcs are
--   approximated by a polyline at 0.1 rad per segment (nSeg >= 2). Shared by
--   polar bars (rose / pie) and 'projectBar'.
wedgeSegments :: Layout -> Double -> Double -> Double -> Double -> [PathSegment]
wedgeSegments l tf0 tf1 rf0 rf1 =
  let dθ    = abs (tf1 - tf0) * 2 * pi
      nSeg  = max 2 (ceiling (dθ / 0.1)) :: Int
      steps = [ tf0 + (tf1 - tf0) * fromIntegral i / fromIntegral nSeg | i <- [0 .. nSeg] ]
      mk t rf = uncurry Point (polarPoint l t rf)
      outer = [ mk t rf1 | t <- steps ]
      inner = [ mk t rf0 | t <- reverse steps ]
  in case outer ++ inner of
       (p0 : rest) -> MoveTo p0 : map LineTo rest ++ [ClosePath]
       []          -> []

-- | [日本語]: データ空間の線分 → px polyline。 直線座標系 (Cartesian/Flip) は
--   両端の 2 点 (= 従来の直線結線と bit 一致)、 極座標は data 空間で線形補間した
--   中間点を 'projectXY' で投影し θ 0.1 rad 刻み ('wedgeSegments' と同粒度) の
--   折線に曲げる。 θ 不変 (= 純 radial) な線分は 2 点のまま。 geom はこの関数を
--   通すことで「線分が座標系でどう曲がるか」 を知らずに済む。
--   [English]: Projects a segment in data space to a pixel polyline. Linear
--   coordinate systems (Cartesian / Flip) give just the two endpoints (bit
--   identical to the previous straight-line joining); polar systems bend the
--   segment into a polyline by projecting intermediate points — linearly
--   interpolated in data space — through 'projectXY' at 0.1 rad per segment
--   (the same granularity as 'wedgeSegments'). A segment at constant theta
--   (purely radial) stays two points. Going through this function frees each
--   geom from knowing how a segment bends under the coordinate system.
projectSegment :: Coord -> Layout -> (Double, Double) -> (Double, Double) -> [Point]
projectSegment coord l (dx0, dy0) (dx1, dy1)
  | not (isPolar coord) =
      [ uncurry Point (projectXY coord l dx0 dy0)
      , uncurry Point (projectXY coord l dx1 dy1) ]
  | otherwise =
      let (tf0, tf1) = case coord of
            CoordPolarY _ -> (domFrac (lpYScale l) dy0, domFrac (lpYScale l) dy1)
            _             -> (domFrac (lpXScale l) dx0, domFrac (lpXScale l) dx1)
          dTheta = abs (tf1 - tf0) * 2 * pi
          nSeg   = max 1 (ceiling (dTheta / 0.1)) :: Int
          lerp a b t = a + (b - a) * t
          ts     = [ fromIntegral i / fromIntegral nSeg | i <- [0 .. nSeg] ]
      in [ uncurry Point (projectXY coord l (lerp dx0 dx1 t) (lerp dy0 dy1 t))
         | t <- ts ]

-- | [日本語]: bar/box 系「data 空間の棒」 の座標系対応形状。 geom 側の
--   @case coord of@ を「形状の case」 に置き換えるための戻り値型。
--   [English]: The coordinate-aware shape of a "bar in data space" for the
--   bar / box family. This return type replaces each geom's @case coord of@
--   with a case on the shape instead.
data BarShape = BarRect !Rect | BarWedge ![PathSegment]
  deriving (Show, Eq)

-- | [日本語]: 棒 (中心 centerD ± halfWidthD、 base..value) の投影 dispatcher。
--   直線座標系は 'projectBarRect' の px Rect (= bit 一致、 厚みは従来通り px 指定)、
--   極座標は 'wedgeSegments' の扇形。 halfWidthD は data 単位の半幅 (bar 既定 0.45
--   = resolution 0.9 の半分)、 thicknessPx は直線座標系専用の px 厚み (極座標では
--   未使用)。 PolarX = 角度帯 centerD±halfWidthD × 半径 base..value (rose)、
--   PolarY = 角度 base..value × 半径帯 centerD±halfWidthD (内径は 0 で clamp)。
--   [English]: The projection dispatcher for a bar (centered at centerD with
--   half width halfWidthD, spanning base..value). Linear coordinate systems
--   give the pixel 'Rect' of 'projectBarRect' (bit identical; thickness is
--   still given in pixels), polar systems the annular sector of
--   'wedgeSegments'. halfWidthD is the half width in data units (0.45 by
--   default for bars, half of the 0.9 resolution); thicknessPx is the pixel
--   thickness used only by linear systems. PolarX gives an angular band of
--   centerD±halfWidthD over radius base..value (a rose); PolarY gives angle
--   base..value over the radial band centerD±halfWidthD (inner radius clamped
--   at 0).
--   domain 退化 (span=0) 時の半幅 frac は 0.5 (= 旧 hwFrac の既定と同一)。
projectBar :: Coord -> Layout -> Double -> Double -> Double -> Double -> Double
           -> BarShape
projectBar coord l centerD baseD valueD halfWidthD thicknessPx = case coord of
  CoordPolarX _ -> BarWedge (wedgeSegments l (cfx - hf) (cfx + hf)
                                             (dfy baseD) (dfy valueD))
  CoordPolarY _ -> BarWedge (wedgeSegments l (dfy baseD) (dfy valueD)
                                             (max 0 (cfx - hf)) (cfx + hf))
  _             -> BarRect (projectBarRect coord l centerD baseD valueD thicknessPx)
  where
    cfx   = domFrac (lpXScale l) centerD
    dfy   = domFrac (lpYScale l)
    spanX = lsDomainHi (lpXScale l) - lsDomainLo (lpXScale l)
    hf    = if spanX == 0 then 0.5 else halfWidthD / spanX

-- | [日本語]: categorical-cross geom (box/violin/strip/swarm) の群中心指定。
--   'CrossAt' = cross 軸の data 座標 (categorical slot 位置 / dodge sub-slot 中心)、
--   'CrossMid' = 単一群 (カテゴリ軸なし) の「plotArea 中央」。 CrossMid を px で
--   なく変種として持つのは、 中央 px 自体が coord (Cartesian=横 / Flip=縦) に
--   依存するため (= geom 側から case coord of を無くす)。
--   [English]: Specifies the group center for categorical-cross geoms (box /
--   violin / strip / swarm). 'CrossAt' is a data coordinate on the cross axis
--   (a categorical slot position, or the center of a dodge sub-slot);
--   'CrossMid' is "the middle of the plot area" for a single group with no
--   categorical axis. 'CrossMid' is a constructor rather than a pixel value
--   because that middle pixel itself depends on the coordinate system
--   (horizontal under Cartesian, vertical under Flip) — which is exactly what
--   lets each geom drop its @case coord of@.
data CrossLoc = CrossAt !Double | CrossMid
  deriving (Show, Eq)

-- | [日本語]: CrossLoc の cross 軸 data 座標 (polar 経路用)。 CrossMid は
--   x domain 中点 (= scale が plotArea を張る前提で plotArea 中央と affine 一致)。
--   [English]: The cross-axis data coordinate of a 'CrossLoc', used by the
--   polar path. 'CrossMid' is the midpoint of the x domain, which coincides
--   affinely with the middle of the plot area given that the scale spans it.
crossLocD :: Layout -> CrossLoc -> Double
crossLocD _ (CrossAt d) = d
crossLocD l CrossMid    = (lsDomainLo (lpXScale l) + lsDomainHi (lpXScale l)) / 2

-- | [日本語]: 極座標で「投影済み点を cross 軸方向へ offPx (px) ずらす」。
--   PolarX (cross=角度) は接線方向 = 中心まわりの回転 (弧長 offPx)、
--   PolarY (cross=半径) は radial。 jitter / beeswarm / violin 幅は視覚 px 量
--   (点径・重なり回避) なので、 polar でも data 角度でなく px 弧長で当てるのが正
--   (半径によらず点間隔が保たれる)。 半径 ≈ 0 は接線方向が定義できないため
--   動かさない。 直線座標系は恒等 (linear 経路は projectCross* が px 加算で
--   処理し、 ここへは来ない)。
--   [English]: Nudges an already-projected point by offPx pixels along the
--   cross axis, in polar coordinates. Under PolarX (cross = angle) this is
--   tangential — a rotation about the center by arc length offPx; under
--   PolarY (cross = radius) it is radial. Jitter, beeswarm spread and violin
--   width are visual pixel quantities (point diameter, overlap avoidance), so
--   applying them as a pixel arc length rather than a data angle is the
--   correct choice even in polar: point spacing is then preserved regardless
--   of radius. At radius near 0 the tangential direction is undefined, so the
--   point is left alone. Linear coordinate systems are the identity here (the
--   linear path adds pixels inside projectCross* and never reaches this
--   function).
polarNudgePx :: Coord -> Layout -> Double -> Point -> Point
polarNudgePx coord l offPx p@(Point px py) =
  let (cx, cy, _) = polarCenter l
      dx = px - cx
      dy = py - cy
      r  = sqrt (dx * dx + dy * dy)
  in if r < 1e-9 then p else case coord of
       CoordPolarX _ ->
         -- 弧長 offPx = 角度 offPx/r の回転 (polarPoint と同じ時計回りが正)。
         let dTh = offPx / r
             c = cos dTh
             s = sin dTh
         in Point (cx + dx * c - dy * s) (cy + dx * s + dy * c)
       CoordPolarY _ ->
         let k = (r + offPx) / r
         in Point (cx + dx * k) (cy + dy * k)
       _ -> p

-- | [日本語]: 「categorical cross × 連続 value」 geom の点投影。 box の外れ値・
--   strip の jitter 点・swarm の beeswarm 点・violin outline は全てここを通す。
--   offPx = cross 軸方向の px offset (nudge / jitter / 幅)。 Cartesian/Flip は
--   旧 geom 内 px 式と bit 一致: Cartesian = Point (sx cross + offPx) (sy v)、
--   Flip = Point (syF v) (sxF cross + offPx)。 polar は projectXY 投影後に
--   'polarNudgePx' で px nudge。
--   [English]: Projects a point for a "categorical cross by continuous value"
--   geom. Box outliers, strip jitter points, swarm beeswarm points and violin
--   outlines all go through this. offPx is the pixel offset along the cross
--   axis (nudge / jitter / width). Cartesian and Flip are bit identical to
--   the pixel formulas previously inlined in each geom: Cartesian gives
--   @Point (sx cross + offPx) (sy v)@ and Flip gives
--   @Point (syF v) (sxF cross + offPx)@. Polar projects through 'projectXY'
--   first and then nudges in pixels via 'polarNudgePx'.
projectCrossPoint :: Coord -> Layout -> CrossLoc -> Double -> Double -> Point
projectCrossPoint CoordCartesian l loc offPx v =
  let base = case loc of
        CrossAt d -> scaleApply (lpXScale l) d
        CrossMid  -> let ar = lpPlotArea l in rX ar + rW ar / 2
  in Point (base + offPx) (scaleApply (lpYScale l) v)
projectCrossPoint CoordFlip l loc offPx v =
  let base = case loc of
        CrossAt d -> scaleApply (lpXScaleFlipped l) d
        CrossMid  -> let ar = lpPlotArea l in rY ar + rH ar / 2
  in Point (scaleApply (lpYScaleFlipped l) v) (base + offPx)
projectCrossPoint coord l loc offPx v =
  polarNudgePx coord l offPx
    (uncurry Point (projectXY coord l (crossLocD l loc) v))

-- | [日本語]: value 軸方向の px 座標 (単調)。 beeswarm binning 等「値どうしの
--   px 間隔」 が要る geom 用。 Cartesian/Flip は旧 sy / flip 式と bit 一致。
--   polar は PolarX (value=半径) = 半径 px、 PolarY (value=角度) = 外周弧長 px。
--   [English]: A monotone pixel coordinate along the value axis, for geoms
--   that need the pixel spacing between values (beeswarm binning and the
--   like). Cartesian and Flip are bit identical to the previous sy / flipped
--   formulas. In polar, PolarX (value = radius) gives the radius in pixels
--   and PolarY (value = angle) gives the arc length along the outer circle.
valueAxisPx :: Coord -> Layout -> Double -> Double
valueAxisPx CoordCartesian l v = scaleApply (lpYScale l) v
valueAxisPx CoordFlip      l v = scaleApply (lpYScaleFlipped l) v
valueAxisPx (CoordPolarX _) l v =
  let (_, _, maxR) = polarCenter l in domFrac (lpYScale l) v * maxR
valueAxisPx (CoordPolarY _) l v =
  let (_, _, maxR) = polarCenter l in domFrac (lpYScale l) v * (2 * pi * maxR)
-- ★ Phase 64 §3 (A11) までは ternary の value 軸は Cartesian に fallback。
valueAxisPx CoordTernary   l v = scaleApply (lpYScale l) v

-- | [日本語]: value 一定で cross 方向へ ±半幅の短線 (box の median / whisker cap)。
--   直線座標系は px 半幅 halfPx の 2 点 (旧式 bit 一致: [base+offPx-halfPx,
--   base+offPx+halfPx])、 polar は data 半幅 halfD の弧 ('projectSegment') を
--   offPx だけ nudge した polyline (点列 ≥ 2)。
--   [English]: A short span of ±half width along the cross axis at a fixed
--   value (a box's median line or whisker cap). Linear coordinate systems
--   give two points at pixel half width halfPx (bit identical to the previous
--   formula: @[base+offPx-halfPx, base+offPx+halfPx]@); polar gives a
--   polyline of at least two points — the arc of data half width halfD from
--   'projectSegment', nudged by offPx.
projectCrossSpan :: Coord -> Layout -> CrossLoc -> Double -> Double -> Double
                 -> Double -> [Point]
projectCrossSpan coord l loc offPx halfPx halfD v
  | not (isPolar coord) =
      [ projectCrossPoint coord l loc (offPx - halfPx) v
      , projectCrossPoint coord l loc (offPx + halfPx) v ]
  | otherwise =
      let d = crossLocD l loc
      in map (polarNudgePx coord l offPx)
             (projectSegment coord l (d - halfD, v) (d + halfD, v))

-- | [日本語]: box 本体等「cross 中心 ± 半幅 × value 区間」 の投影。 直線座標系は
--   px 半幅 halfPx の Rect (旧 geom 内 mkRect 式と bit 一致)、 polar は 'projectBar'
--   と同じ data 半幅 halfD の扇形 (wedge) を offPx だけ nudge。
--   [English]: Projects "cross center ± half width by value interval" — the
--   body of a box and the like. Linear coordinate systems give a 'Rect' at
--   pixel half width halfPx (bit identical to the mkRect formula previously
--   inlined in each geom); polar gives the annular sector of data half width
--   halfD, as in 'projectBar', nudged by offPx.
projectCrossBar :: Coord -> Layout -> CrossLoc -> Double -> Double -> Double
                -> Double -> Double -> BarShape
projectCrossBar CoordCartesian l loc offPx halfPx _halfD vLo vHi =
  let base = case loc of
        CrossAt d -> scaleApply (lpXScale l) d
        CrossMid  -> let ar = lpPlotArea l in rX ar + rW ar / 2
      cc = base + offPx
      sy = scaleApply (lpYScale l)
  in BarRect (Rect (cc - halfPx) (min (sy vLo) (sy vHi))
                   (2 * halfPx) (abs (sy vHi - sy vLo)))
projectCrossBar CoordFlip l loc offPx halfPx _halfD vLo vHi =
  let base = case loc of
        CrossAt d -> scaleApply (lpXScaleFlipped l) d
        CrossMid  -> let ar = lpPlotArea l in rY ar + rH ar / 2
      cc  = base + offPx
      syF = scaleApply (lpYScaleFlipped l)
  in BarRect (Rect (min (syF vLo) (syF vHi)) (cc - halfPx)
                   (abs (syF vHi - syF vLo)) (2 * halfPx))
projectCrossBar coord l loc offPx _halfPx halfD vLo vHi =
  let d = crossLocD l loc
      nudgeP = polarNudgePx coord l offPx
      nudgeSeg seg = case seg of
        MoveTo p        -> MoveTo (nudgeP p)
        LineTo p        -> LineTo (nudgeP p)
        CurveTo a b c   -> CurveTo (nudgeP a) (nudgeP b) (nudgeP c)
        ClosePath       -> ClosePath
  in case projectBar coord l d vLo vHi halfD 0 of
       BarWedge segs | offPx /= 0 -> BarWedge (map nudgeSeg segs)
       shape                      -> shape

-- | [日本語]: categorical 1 スロットの cross 軸 px 幅 (bar/box 等の厚みに使う)。
--   Cartesian は x 軸 (sx) の 1 単位、 Flip は category が縦に来るので
--   flipped scale の縦 1 単位。 これを使わず常に (sx 1 - sx 0) を厚みにすると
--   flip 時に縦スロットを超えて bar が重なる。 Cartesian では (sx 1 - sx 0)
--   と完全一致 (= ゼロ diff)。
--   [English]: The cross-axis px width of a single categorical slot (used
--   for bar/box thickness). Under Cartesian, this is 1 unit of the x axis
--   (sx); under Flip, categories run vertically, so it is 1 vertical unit
--   of the flipped scale. Always using (sx 1 - sx 0) instead would let bars
--   overrun their vertical slot and overlap when flipped. Under Cartesian,
--   it is exactly (sx 1 - sx 0) (zero diff).
-- | [日本語]: ggplot @resolution(x)@ = ソート済み一意値の最小正間隔。
--   errorbar/crossbar の cap 幅をデータ単位化する基準 (width = markWidth ×
--   resolution)。 一意値が 1 個以下なら 1 (categorical = 整数位置 0,1,2… で
--   間隔 1・単一点も 1)。
--   [English]: ggplot's @resolution(x)@: the smallest positive gap between
--   sorted unique values. The basis for converting errorbar/crossbar cap
--   width to data units (width = markWidth × resolution). If there is at
--   most one unique value, returns 1 (categorical positions are integers
--   0,1,2… with gap 1; a single point also gives 1).
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
catUnitPx (CoordPolarX _) l = scaleApply (lpXScale l) 1 - scaleApply (lpXScale l) 0
catUnitPx (CoordPolarY _) l = scaleApply (lpXScale l) 1 - scaleApply (lpXScale l) 0
catUnitPx CoordTernary l = scaleApply (lpXScale l) 1 - scaleApply (lpXScale l) 0

-- | [日本語]: 軸が物理的にどの辺に来るか。 Cartesian: データ x=下・y=左。
--   Flip: データ x=左・y=下。 極座標は直交的な辺軸を持たない (Render の polar
--   分岐が独自に grid/軸を描く)。
--   [English]: Which physical edge an axis is placed on. Cartesian: data
--   x=bottom, y=left. Flip: data x=left, y=bottom. Polar coordinates have no
--   orthogonal edge axis (Render's polar branch draws its own grid/axes).
data AxisPlacement = AxisBottom | AxisLeft | AxisTop | AxisRight
  deriving (Show, Eq)

coordXAxisPlacement :: Coord -> AxisPlacement
coordXAxisPlacement CoordFlip      = AxisLeft
coordXAxisPlacement _              = AxisBottom

coordYAxisPlacement :: Coord -> AxisPlacement
coordYAxisPlacement CoordFlip      = AxisBottom
coordYAxisPlacement _              = AxisLeft

-- | [日本語]: データ x の grid line が縦線か (Cartesian) 横線か (Flip)。
--   [English]: Whether the grid line for data x is vertical (Cartesian) or
--   horizontal (Flip).
coordXGridIsVertical :: Coord -> Bool
coordXGridIsVertical CoordFlip      = False
coordXGridIsVertical _              = True

-- | [日本語]: 極座標か (= CoordPolarX / CoordPolarY)。
--   [English]: Whether this is polar (CoordPolarX or CoordPolarY).
isPolar :: Coord -> Bool
isPolar (CoordPolarX _) = True
isPolar (CoordPolarY _) = True
isPolar _               = False

-- | [日本語]: D3 風 nice tick (= 1/2/5 × 10^k の刻み)。
--   [English]: D3-style nice ticks (steps of 1/2/5 × 10^k).
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

-- | [日本語]: R labeling::extended (Talbot, Lin & Hanrahan 2010 "An
--   Extension of Wilkinson's Algorithm…") の移植。 ggplot2 の既定 breaks
--   (`scales::extended_breaks(n)`) と同一: 候補刻み Q=[1,5,2,2.5,4,3]、 重み
--   w=[simplicity 0.25, coverage 0.2, density 0.5, legibility 0.05]、
--   only.loose=False、 legibility は常に 1 (R 実装も placeholder)。
--   simplicity/coverage/density の重み付きスコアを最大化する (lmin, lmax,
--   lstep) を選び、 等間隔 break 列を返す。 入力 (dmin,dmax) は
--   __expansion 前のデータ範囲__、 m は目標ラベル数。 旧 niceTicks (1/2/5×10^k) を linear
--   軸で置換 (端点・本数が ggplot と一致する)。
--
--   j→q→k→z→start のネストループは R 実装をそのまま再現。 各段の上界
--   (simplicityMax / densityMax / coverageMax) による枝刈りで停止するが、
--   浮動小数の保険として j/k/z に上限ガードを置く (実用域では枝刈りが先に
--   効く)。
--   [English]: A port of R's labeling::extended (Talbot, Lin & Hanrahan
--   2010, "An Extension of Wilkinson's Algorithm…"), matching ggplot2's
--   default breaks (`scales::extended_breaks(n)`): candidate steps
--   Q=[1,5,2,2.5,4,3], weights w=[simplicity 0.25, coverage 0.2,
--   density 0.5, legibility 0.05], only.loose=False, legibility always 1
--   (a placeholder in the R implementation too). Selects (lmin, lmax,
--   lstep) that maximises the weighted score of simplicity/coverage/
--   density, and returns an evenly spaced break sequence. The input
--   (dmin,dmax) is __the data range before expansion__; m is the target
--   label count. Replaces the previous niceTicks (1/2/5×10^k) on linear
--   axes (matching ggplot's endpoints and tick count).
--
--   The nested j→q→k→z→start loops reproduce the R implementation
--   directly. Each level stops via pruning on its upper bound
--   (simplicityMax / densityMax / coverageMax), with upper-bound guards on
--   j/k/z as a floating-point safety net (pruning kicks in first in
--   practical ranges).
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

-- | [日本語]: Log scale 用 tick (= 10^k グリッド)。 domain 内の整数 exponent
--   を出す。
--   [English]: Ticks for the Log scale (a 10^k grid). Emits integer
--   exponents within the domain.
niceTicksLog :: Int -> Double -> Double -> [Double]
niceTicksLog _n lo hi
  | lo <= 0 || hi <= 0 || hi <= lo = [lo]
  | otherwise =
      let kLo = floor   (logBase 10 lo) :: Int
          kHi = ceiling (logBase 10 hi) :: Int
      in [ 10 ** fromIntegral k | k <- [kLo .. kHi], let v = 10 ** fromIntegral k :: Double
                                                 , v >= lo, v <= hi ]

-- | [日本語]: Sqrt scale 用 tick: sqrt 後を niceTicks に通し、 二乗して戻す。
--   domain が非負前提。 負値 lo は 0 にクランプ。
--   [English]: Ticks for the Sqrt scale: passes sqrt-transformed values
--   through niceTicks, then squares them back. Assumes a non-negative
--   domain; a negative lo is clamped to 0.
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

-- | [日本語]: Time scale 用 tick: unix epoch (= seconds since 1970) を入力に、
--   「綺麗な」 間隔 (= 1m / 1h / 1d / 1w / 1M / 1y) で tick を生成。 簡略実装:
--   linear nice ticks を秒単位で取り、 1m / 1h / 1d / 1w 単位に丸め。 月 / 年
--   単位の境界調整は将来。
--   [English]: Ticks for the Time scale: takes a unix epoch (seconds since
--   1970) as input and generates ticks at "nice" intervals (1m / 1h / 1d /
--   1w / 1M / 1y). A simplified implementation: takes linear nice ticks in
--   seconds and rounds to 1m / 1h / 1d / 1w units. Boundary adjustment for
--   month / year units is future work.
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
