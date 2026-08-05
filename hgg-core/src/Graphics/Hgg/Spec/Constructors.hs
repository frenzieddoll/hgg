-- |
-- Module      : Graphics.Hgg.Spec.Constructors
-- Description : Layer constructors for plot marks (the mark catalogue)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 'Graphics.Hgg.Spec' の module 分割で切り出し。 mark ごとの Layer
-- 構築子 ('scatter' / 'line' / 'bar' / ... / 'customMark') と mark 固有 setter
-- ('binCount' / 'jitterX' / 'shape' / 'statLm' 系等)、 hexbin の binning
-- ('HexCell') を持つ。 中身は等質な mark カタログ (辞書的) ゆえ 1 module に
-- まとめる (user 合意)。 公開 API は従来どおり
-- 'Graphics.Hgg.Spec' (facade) が re-export する。 挙動・出力は完全に不変。
-- [English]: Split out of 'Graphics.Hgg.Spec' as part of its module split.
-- Holds the per-mark Layer constructors ('scatter' / 'line' / 'bar' / ... /
-- 'customMark'), mark-specific setters ('binCount' / 'jitterX' / 'shape' /
-- the 'statLm' family, etc.), and hexbin binning ('HexCell'). Since the
-- contents form a homogeneous mark catalogue (dictionary-like), they are
-- kept in a single module (agreed with the user). The public API is
-- unchanged: 'Graphics.Hgg.Spec' (the facade) still re-exports everything.
-- Behavior and output are fully unchanged.
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Constructors
  ( -- * 基本 mark
    scatter, line, bar, histogram, histogramDensity
  , heatmap, boxplot, density, densityFill, freqpoly
  , scatterPoints, linePoints, unzipPoint2
    -- * custom mark
  , customMark, customMarkWith, encX, encY
    -- * 統計 / 分布 mark
  , trace, traceLines, forest, forestNull, funnel, autocorr, autocorrMaxLag, ess
  , violin, strip, swarm, raincloud, ridge
  , qq, ecdf, lineRange, pointRange, crossbar
  , statMean, statMedian, statLm, statLmLevel, statSmooth, statSmoothCI
  , statPoly, statResid, statFunction
    -- * 特化 mark
  , band, step, stem, stream, pie, waterfall, parallelCoords
  , countXY, quiver, contour, contourFilled, bin2d, bin2dCount, tile
  , hexbin, hexbinBins, HexCell(..), hexbinCells, hexbinLayerCells
  , dag, dagNode, dagNodeDist, dagEdge, dagFromLists, dagFromListsWithPlates
    -- * mark 固有 setter / 補助
  , (<+>)
  , alphaBy, arrowColorByMagnitude, arrowScale
  , binCount, binWidth, histBinning, histBorder, hollow
  , chain, colorCats, orderedCats, compositeLanes, densityNorm
  , contourBreaks, contourLevels
  , groupBy, jitterX, jitterY, label
  , linetype, linetypeBy, markWidth, nudge, position
  , shape, shapeBy, shapeMapEntry, side, sizeBy, text
  ) where

import           Data.Aeson      (Value)
import qualified Data.Aeson      as Aeson
import qualified Data.List
import           Data.Monoid     (First (..), Last (..))
import           Data.Text       (Text)
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

import           Graphics.Hgg.Color (fromHex)
import           Graphics.Hgg.Primitive (Primitive)
import           Graphics.Hgg.Spec.Column
import           Graphics.Hgg.Spec.CustomMark
import           Graphics.Hgg.Spec.Layer
import           Graphics.Hgg.Spec.Mark

-- ===========================================================================
-- Layer constructors (= 各 mark の最小起点)
-- ===========================================================================

scatter, line, bar :: ColRef -> ColRef -> Layer
scatter x y = mempty
  { lyKind = First (Just MScatter), lyEncX = Last (Just x), lyEncY = Last (Just y) }
line    x y = mempty
  { lyKind = First (Just MLine),    lyEncX = Last (Just x), lyEncY = Last (Just y) }
bar     x y = mempty
  { lyKind = First (Just MBar),     lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: custom mark を定義する公開 API。 core (@MarkKind@ の閉列挙) を触らず
--   新しいプロット型を足す拡張点。 @cid@ = 安定 mark 識別子 (PS registry dispatch の
--   鍵)、 @draw@ = 'RenderCtx' を受け取り 'Primitive' 列を返す描画 closure。 データは
--   closure に閉じ込めても、 'rcResolver' 経由で layer 束縛列を引いてもよい。
--
--   HS は closure を直接呼んで描く (SVG/PDF/Rasterific)。 PS canvas で parity が欲しい
--   時は同じ @cid@ で PS registry に draw 関数を手登録する (無ければ HS 専用)。
--   [English]: The public API for defining a custom mark. An extension
--   point for adding new plot types without touching the core (the closed
--   @MarkKind@ enum). @cid@ is the stable mark identifier (the key for PS
--   registry dispatch); @draw@ is a drawing closure that takes a
--   'RenderCtx' and returns a list of 'Primitive'. Data can either be
--   captured in the closure or pulled from the layer's bound columns via
--   'rcResolver'.
--
--   HS calls the closure directly to draw (SVG/PDF/Rasterific). When
--   parity with the PS canvas is needed, register a draw function by hand
--   in the PS registry under the same @cid@ (otherwise the mark is
--   HS-only).
--
-- > customMark "myElbow" $ \ctx -> [ PLine (uncurry Point (rcProjectXY ctx 0 0)) ... ]
customMark :: Text -> (RenderCtx -> [Primitive]) -> Layer
customMark cid draw = mempty
  { lyKind   = First (Just MCustom)
  , lyCustom = Last (Just (CustomMark cid Aeson.Null draw)) }

-- | [日本語]: option 付き 'customMark'。 @opts@ は PS registry の draw 関数へ渡る
--   serializable JSON。
--   [English]: 'customMark' with options. @opts@ is serializable JSON
--   passed through to the PS registry's draw function.
customMarkWith :: Text -> Value -> (RenderCtx -> [Primitive]) -> Layer
customMarkWith cid opts draw = mempty
  { lyKind   = First (Just MCustom)
  , lyCustom = Last (Just (CustomMark cid opts draw)) }

-- | [日本語]: x / y encoding 列を単独で束ねる 'Layer' setter。 mark 種別に依らず
--   合成でき、 custom mark を「一級 mark」化する (= 軸 range が 'lyEncX'/'lyEncY'
--   から自動計算され、 @df |>>@ とも連携)。 既存 mark の encoding 上書きにも使える。
--   custom mark の名前付き combinator は普通こう書く:
--   [English]: A 'Layer' setter that binds the x / y encoding column on its
--   own. It composes with any mark kind and turns a custom mark into a
--   "first-class mark" (axis ranges are computed automatically from
--   'lyEncX'/'lyEncY', and it works with @df |>>@ too). It can also
--   override the encoding of an existing mark. A named custom-mark
--   combinator is typically written like this:
--
-- > dendrogram :: ColRef -> ColRef -> Layer
-- > dendrogram x y = customMark "dendrogram" (drawFromCols x y) <> encX x <> encY y
-- > -- usage: layer (dendrogram "leaf" "height")  -- same feel as scatter x y
encX :: ColRef -> Layer
encX x = mempty { lyEncX = Last (Just x) }

encY :: ColRef -> Layer
encY y = mempty { lyEncY = Last (Just y) }

-- | [日本語]: 2D scatter ('Point2' 直入れ・3D
--   'Graphics.Hgg.ThreeD.Spec.scatter3DPoints' と対称)。 内部は
--   @scatter (inline xs) (inline ys)@ に等価 (= x/y を inline 列に分解) なので
--   Render/JSON/PS 無改修。
--   [English]: 2D scatter that takes 'Point2' values directly (the
--   counterpart of the 3D 'Graphics.Hgg.ThreeD.Spec.scatter3DPoints').
--   Internally equivalent to @scatter (inline xs) (inline ys)@ (x/y are
--   split into inline columns), so Render/JSON/PS need no changes.
--
-- > scatterPoints [Point2 1 2, Point2 3 4]
scatterPoints :: [Point2] -> Layer
scatterPoints pts = scatter (inline xs) (inline ys)
  where (xs, ys) = unzipPoint2 pts

-- | [日本語]: 2D line ('Point2' 直入れ・3D 'Graphics.Hgg.ThreeD.Spec.line3DPoints' と
--   対称)。 内部は @line (inline xs) (inline ys)@ に等価。
--   [English]: 2D line that takes 'Point2' values directly (the
--   counterpart of the 3D 'Graphics.Hgg.ThreeD.Spec.line3DPoints').
--   Internally equivalent to @line (inline xs) (inline ys)@.
linePoints :: [Point2] -> Layer
linePoints pts = line (inline xs) (inline ys)
  where (xs, ys) = unzipPoint2 pts

-- | [日本語]: '[Point2]' を x / y の 'Double' リストに分解 ('scatterPoints' /
--   'linePoints' 用)。
--   [English]: Splits a '[Point2]' into separate x / y 'Double' lists (used
--   by 'scatterPoints' / 'linePoints').
unzipPoint2 :: [Point2] -> ([Double], [Double])
unzipPoint2 = unzip . map (\(Point2 x y) -> (x, y))

-- | [日本語]: vector field (quiver)。 各 (x,y) に成分 (u,v) の矢印を描く
--   (= matplotlib @quiver@)。 矢印長は autoscale (= 最長矢印がデータ対角の ~8%)
--   に 'arrowScale' 倍を掛けた長さ。 列バインドは
--   @df |>> quiver \"x\" \"y\" \"u\" \"v\"@。 矢印を magnitude (= √(u²+v²)) で
--   連続色マップするには 'arrowColorByMagnitude'。
--   [English]: A vector field (quiver). Draws an arrow with components
--   (u,v) at each (x,y), like matplotlib's @quiver@. Arrow length is the
--   autoscale length (the longest arrow is ~8% of the data diagonal)
--   multiplied by 'arrowScale'. Column binding looks like
--   @df |>> quiver \"x\" \"y\" \"u\" \"v\"@. Use 'arrowColorByMagnitude' to
--   map arrows to a continuous color by magnitude (= √(u²+v²)).
quiver :: ColRef -> ColRef -> ColRef -> ColRef -> Layer
quiver x y u v = mempty
  { lyKind = First (Just MQuiver)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyEncU = Last (Just u), lyEncV = Last (Just v) }

-- | [日本語]: quiver 矢印長の倍率 (autoscale × この値・既定 1)。 値を上げると矢印が
--   長く。
--   [English]: The scale factor for quiver arrow length (autoscale × this
--   value, default 1). Raising it makes arrows longer.
arrowScale :: Double -> Layer
arrowScale s = mempty { lyArrowScale = Last (Just s) }

-- | [日本語]: quiver の矢印を magnitude (= √(u²+v²)) で連続色マップする (既定
--   OFF)。 色は連続パレット (viridis 系)。 OFF 時は単色 ('color' / theme)。
--   [English]: Maps quiver arrows to a continuous color by magnitude
--   (= √(u²+v²)); default OFF. The color uses a continuous (viridis-family)
--   palette. When OFF, arrows use a single color ('color' / theme).
arrowColorByMagnitude :: Layer
arrowColorByMagnitude = mempty { lyArrowMagnitude = Last (Just True) }

-- | [日本語]: データ駆動テキストラベル (= ggplot @geom_text@)。 各 (x,y) 点に lab
--   列の文字を描く。 'Graphics.Hgg.Spec.Setters.annotate' (固定 1 点) と違い列駆動で点数ぶん出る。
--   [English]: A data-driven text label (like ggplot's @geom_text@). Draws
--   the text of the lab column at each (x,y) point. Unlike 'Graphics.Hgg.Spec.Setters.annotate'
--   (a single fixed point), it is column-driven and produces one label per
--   row.
text :: ColRef -> ColRef -> ColRef -> Layer
text x y lab = mempty
  { lyKind = First (Just MText), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyLabel = Last (Just lab) }

-- | [日本語]: 背景付きテキストラベル (= ggplot @geom_label@)。 'text' と同じだが
--   各文字の背後に角丸矩形を敷く (= 重なる点の上でも読みやすい)。
--   [English]: A text label with a background (like ggplot's
--   @geom_label@). Same as 'text' but draws a rounded rectangle behind
--   each label (readable even over overlapping points).
label :: ColRef -> ColRef -> ColRef -> Layer
label x y lab = mempty
  { lyKind = First (Just MLabel), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyLabel = Last (Just lab) }

-- | [日本語]: Q-Q plot (= ggplot @stat_qq@ / @geom_qq@)。 sample 列のみを取り、
--   ソートした order statistic y_(i) を y、 理論正規分位点 Φ⁻¹((i-0.5)/n) を x に
--   置いて点を描く (= 正規性の視覚診断)。 理論分位点は render / range 側で算出する
--   ため、 ここでは sample を encY に保持するだけ (encX 列は持たない)。
--   [English]: A Q-Q plot (like ggplot's @stat_qq@ / @geom_qq@). Takes only
--   a sample column and plots points with the sorted order statistic
--   y_(i) as y and the theoretical normal quantile Φ⁻¹((i-0.5)/n) as x
--   (a visual diagnostic for normality). Since the theoretical quantiles
--   are computed on the render / range side, this only stores sample in
--   encY (there is no encX column).
qq :: ColRef -> Layer
qq sample = mempty
  { lyKind = First (Just MQQ), lyEncY = Last (Just sample) }

-- | [日本語]: ECDF plot (= ggplot @stat_ecdf@)。 sample 列 (encX) をソートして
--   右連続の経験累積分布 F(x)=#(≤x)/n を階段状に描く (y∈[0,1])。
--   [English]: An ECDF plot (like ggplot's @stat_ecdf@). Sorts the sample
--   column (encX) and draws the right-continuous empirical CDF
--   F(x)=#(≤x)/n as a step curve (y∈[0,1]).
ecdf :: ColRef -> Layer
ecdf sample = mempty
  { lyKind = First (Just MEcdf), lyEncX = Last (Just sample) }

-- | [日本語]: linerange (= ggplot @geom_linerange@)。 各 (x,y) に縦線 y±err を描く。
--   [English]: A linerange (like ggplot's @geom_linerange@). Draws a
--   vertical segment y±err at each (x,y).
lineRange :: ColRef -> ColRef -> ColRef -> Layer
lineRange x y err = mempty
  { lyKind = First (Just MLineRange), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | [日本語]: pointrange (= ggplot @geom_pointrange@)。 linerange + 中心点。
--   [English]: A pointrange (like ggplot's @geom_pointrange@): a linerange
--   plus a center point.
pointRange :: ColRef -> ColRef -> ColRef -> Layer
pointRange x y err = mempty
  { lyKind = First (Just MPointRange), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | [日本語]: crossbar (= ggplot @geom_crossbar@)。 幅付き箱 (y±err) + 中央水平線。
--   [English]: A crossbar (like ggplot's @geom_crossbar@): a box of width
--   y±err with a horizontal center line.
crossbar :: ColRef -> ColRef -> ColRef -> Layer
crossbar x y err = mempty
  { lyKind = First (Just MCrossbar), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | [日本語]: stat_function (= ggplot @stat_function@ / @geom_function@)。
--   関数 f を [xLo, xHi] で n 点サンプルし、 inline 列の line layer を生成する。
--   関数自体は JSON 化できないため __構成時にサンプル点へ焼き込む__ (= spec には
--   点列が入り、 canvas backend は通常の line として描く)。 n<2 は 2 に切り上げ。
--   [English]: stat_function (like ggplot's @stat_function@ /
--   @geom_function@). Samples the function f at n points over
--   [xLo, xHi] and produces a line layer with inline columns. Since the
--   function itself cannot be serialized to JSON, __the sample points are baked in at construction time__
--   (the spec holds the point series, and the canvas backend draws it as
--   an ordinary line). n<2 is rounded up to 2.
statFunction :: (Double -> Double) -> Double -> Double -> Int -> Layer
statFunction f xLo xHi n =
  let m  = max 2 n
      xs = [ xLo + (xHi - xLo) * fromIntegral i / fromIntegral (m - 1) | i <- [0 .. m - 1] ]
      ys = map f xs
  in line (ColNum (V.fromList xs)) (ColNum (V.fromList ys))

histogram :: ColRef -> Layer
histogram x = mempty
  { lyKind = First (Just MHistogram), lyEncX = Last (Just x) }

-- | [日本語]: 頻度多角形 (Ch10 EDA): @geom_freqpoly(aes(x = …))@ 相当。 histogram
--   と同じ bin 化で各 bin の count を求め、 bin 中心を折れ線で結ぶ。 bin 幅は
--   'binWidth' / 'binCount'、 after_stat(density) は 'histogramDensity' True で
--   流用 (= histogram と同じフラグ)。 color aesthetic ('colorBy') で群分割すると
--   群ごとに別色の折れ線を重ねる (cut 別 price 分布の比較等)。
--   [English]: A frequency polygon (Ch10 EDA): the equivalent of
--   @geom_freqpoly(aes(x = …))@. Uses the same binning as histogram to
--   compute each bin's count, then connects the bin centers with a line.
--   Bin width follows 'binWidth' / 'binCount', and after_stat(density) is
--   reused via 'histogramDensity' True (the same flag as histogram). When
--   split by a color aesthetic ('colorBy'), overlays one differently
--   colored line per group (e.g. comparing price distributions by cut).
freqpoly :: ColRef -> Layer
freqpoly x = mempty
  { lyKind = First (Just MFreqPoly), lyEncX = Last (Just x) }

-- | [日本語]: Ch10 EDA: 2 カテゴリ変数の件数 (= ggplot @geom_count()@ /
--   @stat_sum@)。 @countXY x y@ は (x,y) のカテゴリ組合せごとに観測件数を集計し、
--   各セル中心に面積 ∝ 件数 (= 半径 ∝ √件数) の点を描く。 'size' で最大半径 px を
--   上書き可。
--   [English]: Ch10 EDA: counts for two categorical variables (like
--   ggplot's @geom_count()@ / @stat_sum@). @countXY x y@ tallies the
--   observed count for each (x,y) category combination and draws a point
--   at each cell center with area ∝ count (radius ∝ √count). 'size'
--   overrides the maximum radius in px.
countXY :: ColRef -> ColRef -> Layer
countXY x y = mempty
  { lyKind = First (Just MCount), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: MCMC autocorrelation plot (P19): 1 列の時系列から lag-k 自己相関
--   r(τ) を計算し bar chart で表示。 max lag は 'autocorrMaxLag'、 default は 40。
--   ±1.96/√N の significance band も同時描画。
--
--   r(τ) = Σ(x_t - μ)(x_{t+τ} - μ) / Σ(x_t - μ)²
--
--   matplotlib との対応: `plt.acorr(x, maxlags=40)` 相当 (= 但し片側のみ)。
--   [English]: MCMC autocorrelation plot (P19): computes the lag-k
--   autocorrelation r(τ) from a single time-series column and displays it
--   as a bar chart. The max lag is set with 'autocorrMaxLag' (default 40),
--   and the ±1.96/√N significance band is drawn alongside.
--
--   r(τ) = Σ(x_t - μ)(x_{t+τ} - μ) / Σ(x_t - μ)²
--
--   Corresponds to matplotlib's `plt.acorr(x, maxlags=40)` (one-sided
--   only).
autocorr :: ColRef -> Layer
autocorr c = mempty
  { lyKind = First (Just MAutocorr)
  , lyEncX = Last (Just c)
  }

-- | [日本語]: autocorr の max lag (= 'autocorr' と '<>' で組合せ)。 default 40。
--   [English]: The max lag for autocorr (combine with 'autocorr' via
--   '<>'). Default 40.
autocorrMaxLag :: Int -> Layer
autocorrMaxLag n = mempty { lyMaxLag = Last (Just n) }

-- | [日本語]: Effective Sample Size plot (P20): chain ごとに ESS bar を描画。
--   chain group は 'chain' で指定 (= 'ess vals <> chain chainCol')。
--   chain 未指定なら全体を 1 chain として 1 bar。
--
--   ESS = N / (1 + 2 Σ |r(τ)|)  (= τ=1 から r(τ) > 0 まで)
--
--   matplotlib / arviz 対応: `az.plot_ess(idata)` の chain ごと bar (= 簡略版)。
--   [English]: Effective Sample Size plot (P20): draws an ESS bar per
--   chain. The chain group is set with 'chain' (= 'ess vals <> chain
--   chainCol'); with no chain specified, the whole series is treated as a
--   single chain with a single bar.
--
--   ESS = N / (1 + 2 Σ |r(τ)|)  (from τ=1 up to where r(τ) > 0)
--
--   Corresponds to arviz/matplotlib's `az.plot_ess(idata)`, per-chain bars
--   (a simplified version).
-- | [日本語]: ESS 棒グラフ: encX = パラメータ/chain 名 (categorical)、
--   encY = 計算済み ESS 値。 ESS の計算は統計ライブラリ (analyze 側) の責務で、
--   plot は値を棒にするだけ (= ggplot/bayesplot mcmc_neff 流の「計算と描画の
--   分離」)。
--   [English]: ESS bar chart: encX is the parameter/chain name
--   (categorical), encY is the precomputed ESS value. Computing ESS is the
--   responsibility of the statistics library (the analyze side); plot
--   merely turns the values into bars (the same separation of computation
--   and drawing as ggplot/bayesplot's mcmc_neff).
ess :: ColRef -> ColRef -> Layer
ess nameCol essCol = mempty
  { lyKind = First (Just MEss)
  , lyEncX = Last (Just nameCol)
  , lyEncY = Last (Just essCol)
  }

-- | [日本語]: chain group 列を設定 (= 'autocorr' / 'ess' で chain 分け、 MTrace でも
--   将来使用)。
--   [English]: Sets the chain-group column (used by 'autocorr' / 'ess' to
--   split by chain; will also be used by MTrace in the future).
chain :: ColRef -> Layer
chain c = mempty { lyChain = Last (Just c) }

-- | [日本語]: Forest plot: 各 row が「label + 点推定 + CI」 の horizontal CI bar
--   群。
--
--   引数: label 列 (= categorical/text)、 点推定 列、 ± 半幅 列 (= 対称 CI)。
--
--   * y 軸: label
--   * x 軸: estimate
--   * 中央 vertical 線: 'forestNull' (= default 0、 メタ解析慣例で OR は 1)
--
--   asymmetric CI (= lo / hi 個別) は将来。 現状は対称 CI のみ。
--   [English]: A forest plot: a group of horizontal CI bars, one row per
--   "label + point estimate + CI".
--
--   Arguments: the label column (categorical/text), the point-estimate
--   column, and the ± half-width column (a symmetric CI).
--
--   * y axis: label
--   * x axis: estimate
--   * center vertical line: 'forestNull' (default 0; meta-analysis
--     convention uses 1 for an odds ratio)
--
--   Asymmetric CI (separate lo / hi) is a future addition; only symmetric
--   CI is supported today.
forest :: ColRef -> ColRef -> ColRef -> Layer
forest labelCol estCol errCol = mempty
  { lyKind   = First (Just MForest)
  , lyEncX   = Last (Just estCol)
  , lyEncY   = Last (Just labelCol)
  , lyErrorX = Last (Just errCol)
  }

-- | [日本語]: Forest plot の null effect 位置 (= 縦 0 線、 メタ解析の reference)。
--   default 0。 リスク比 / オッズ比 を log scale で扱う場合は 0 (= log 1)、 線形なら
--   0 (= 差)。
--   [English]: The null-effect position for a forest plot (the vertical
--   reference line at 0, the meta-analysis reference). Default 0. When
--   handling risk ratios / odds ratios on a log scale, 0 (= log 1); on a
--   linear scale, 0 (= no difference).
forestNull :: Double -> Layer
forestNull v = mempty { lyMaxLag = Last (Just (round v)) }
  -- 流用: lyMaxLag を null position の Int で再利用 (= round)。
  -- TODO: Double-precision null position field を別途 (= 当面 Int で十分)

-- | [日本語]: Funnel plot: 効果量 vs 標準誤差の散布図 + 95% 信頼区間 envelope。
--
--   引数: 効果量 (effect) 列、 標準誤差 (SE) 列。 出版 bias 確認に使う。
--
--   * x 軸: effect (= estimate)
--   * y 軸: SE (= 上方が精度高、 下方が精度低)
--   * 中央 vertical 線: pooled mean (= データから算出)
--   * diagonal 線: pooled ± 1.96 * SE の envelope
--   [English]: A funnel plot: a scatter of effect size vs. standard error
--   plus a 95% confidence envelope.
--
--   Arguments: the effect-size (effect) column and the standard-error (SE)
--   column. Used to check for publication bias.
--
--   * x axis: effect (the estimate)
--   * y axis: SE (higher = more precise, lower = less precise)
--   * center vertical line: the pooled mean (computed from the data)
--   * diagonal lines: the pooled ± 1.96 * SE envelope
funnel :: ColRef -> ColRef -> Layer
funnel effectCol seCol = mempty
  { lyKind = First (Just MFunnel)
  , lyEncX = Last (Just effectCol)
  , lyEncY = Last (Just seCol)
  }

-- | [日本語]: Box plot。 ★ 値 1 列を受ける。 群分けは @<> groupBy "g"@ (色一律) /
--   @<> colorBy "g"@ (群色+凡例) で付ける (ggplot 同型)。 群指定なしなら単一 box。
--   [English]: A box plot. ★ Takes a single value column. Grouping is added
--   with @<> groupBy "g"@ (uniform color) / @<> colorBy "g"@ (per-group
--   color + legend), matching ggplot. With no group specified, it draws a
--   single box.
boxplot :: ColRef -> Layer
boxplot vals = mempty
  { lyKind = First (Just MBox), lyEncY = Last (Just vals) }

-- | [日本語]: ★ 群で分けて配置するチャネル (= ggplot @aes(group=)@)。 色は付けない
--   (一律。 色は 'color' / 'colorBy' で別途)。 distribution mark (boxplot/violin
--   等) では群ごとに集約を作りカテゴリ x に並べる。 内部表現は encX (= 既存の
--   群配置機構を流用)。 ⚠ @Data.List.groupBy@ と同名なので、 両方 import する場合は
--   qualified 推奨。
--   [English]: ★ A channel for grouping and positioning marks (like
--   ggplot's @aes(group=)@). It does not add color (uniform; use 'color' /
--   'colorBy' separately for that). Distribution marks (boxplot/violin,
--   etc.) build one aggregate per group and lay them out along categorical
--   x. Internally represented as encX (reusing the existing group-layout
--   mechanism). ⚠ Shares a name with @Data.List.groupBy@; use a qualified
--   import if importing both.
groupBy :: ColRef -> Layer
groupBy g = mempty { lyEncX = Last (Just g) }

-- | [日本語]: Density plot: x 列の値ベクターで Gaussian KDE 曲線。
--   [English]: A density plot: a Gaussian KDE curve over the x column's
--   values.
density :: ColRef -> Layer
density x = mempty
  { lyKind = First (Just MDensity), lyEncX = Last (Just x) }

-- | [日本語]: pairs 対角用 density。 y 軸目盛りは値範囲 (= 行の変数値、 散布図行と
--   共有)、 KDE 曲線は panel 高さに独立正規化して描く (= seaborn pairplot 対角の
--   挙動)。
--   [English]: A density mark for the diagonal of a pairs plot. The y-axis
--   ticks use the value range (the row's variable values, shared with the
--   scatter row), while the KDE curve is independently normalized to the
--   panel height (matching seaborn pairplot's diagonal behavior).
densityNorm :: ColRef -> Layer
densityNorm x = mempty
  { lyKind = First (Just MDensity), lyEncX = Last (Just x)
  , lyDensityNorm = Last (Just True) }

-- | [日本語]: Pie chart (= encX cat, encY 値合計の扇)。
--   [English]: A pie chart (encX gives the category, encY the value whose
--   sum defines each slice).
pie :: ColRef -> ColRef -> Layer
pie x y = mempty
  { lyKind = First (Just MPie), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: Waterfall chart (= encX cat, encY delta、 累積 bar)。
--   [English]: A waterfall chart (encX gives the category, encY the
--   delta; bars accumulate).
waterfall :: ColRef -> ColRef -> Layer
waterfall x y = mempty
  { lyKind = First (Just MWaterfall), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: (= Heatmap): x = カテゴリ, y = カテゴリ, value = 数値。 各 (x,y) セルを
--   value の連続色 (Viridis) で塗る。 value は ColorByContinuous で表現。 PS heatmap
--   と対応。
--   [English]: A heatmap: x is a category, y is a category, value is
--   numeric. Each (x,y) cell is colored using value's continuous color
--   (Viridis); value is represented via ColorByContinuous. Corresponds to
--   the PS heatmap.
heatmap :: ColRef -> ColRef -> ColRef -> Layer
heatmap x y v = mempty
  { lyKind = First (Just MHeatmap)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous v))
  }

-- | [日本語]: Contour / binned heatmap (= 連続 x/y/z、 grid 化してセル平均を
--   Viridis 色マッピング)。 ResponseSurface の基盤。 color は ColorByContinuous で
--   z 列を表現。
--   [English]: Contour / binned heatmap (continuous x/y/z, gridded and
--   the per-cell average mapped to a Viridis color). The foundation for
--   ResponseSurface. color represents the z column via ColorByContinuous.
contour :: ColRef -> ColRef -> ColRef -> Layer
contour x y z = mempty
  { lyKind = First (Just MContour)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | [日本語]: filled contour (= matplotlib @contourf@ / ggplot
--   @geom_contour_filled@)。 等値帯を Viridis 連続色で塗る。 入力が規則 grid
--   (x 固有値 × y 固有値が全組存在) なら補間せず直入力、 散布なら k 近傍 IDW で
--   格子化 ('Graphics.Hgg.Math.Griddata')。 線の 'contour' と重畳すると
--   matplotlib の contourf+contour 同等。
--   [English]: A filled contour (like matplotlib's @contourf@ / ggplot's
--   @geom_contour_filled@). Fills iso-bands with a continuous Viridis
--   color. If the input is a regular grid (every combination of the x and
--   y distinct values is present), it is used as-is with no
--   interpolation; scattered input is gridded via k-nearest-neighbor IDW
--   ('Graphics.Hgg.Math.Griddata'). Overlaying it with the line-based
--   'contour' matches matplotlib's contourf+contour.
contourFilled :: ColRef -> ColRef -> ColRef -> Layer
contourFilled x y z = mempty
  { lyKind = First (Just MContourFilled)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | [日本語]: 等高線の本数 (既定 8)。 @contour x y z <> contourLevels 12@。
--   [English]: The number of contour levels (default 8). Example:
--   @contour x y z <> contourLevels 12@.
contourLevels :: Int -> Layer
contourLevels n = mempty { lyContourLevels = Last (Just n) }

-- | [日本語]: 等高線レベルの明示指定 (本数指定より優先)。
--   [English]: Explicitly specifies contour levels (takes priority over
--   the level-count setting).
contourBreaks :: [Double] -> Layer
contourBreaks bs = mempty { lyContourBreaks = Last (Just bs) }

-- | [日本語]: binned heatmap (= ggplot geom_bin2d / stat_summary_2d)。 連続
--   x/y/z を nBins×nBins の grid に binning し、 各セルの z 平均を連続色
--   (Viridis) で塗る。 'contour' (等高線) の塗り版。 ResponseSurface の塗り基盤。
--   [English]: A binned heatmap (like ggplot's geom_bin2d /
--   stat_summary_2d). Bins continuous x/y/z into an nBins×nBins grid and
--   colors each cell by its z average using a continuous (Viridis) color.
--   The filled counterpart of the contour-line 'contour'; the foundation
--   for the filled ResponseSurface.
bin2d :: ColRef -> ColRef -> ColRef -> Layer
bin2d x y z = mempty
  { lyKind = First (Just MBin2d)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | [日本語]: Ch10 EDA: 2D bin の__件数__を連続色で塗る (= ggplot @geom_bin2d()@
--   既定)。 @bin2dCount x y@ は連続 x/y を 12×12 grid に binning し、 各セルの
--   __観測件数__を Viridis で塗る (z 列なし = 'bin2d' の count 版)。 'bin2d'
--   (z 平均) は stat_summary_2d 相当。
--   [English]: Ch10 EDA: colors the __count__ of a 2D bin with a
--   continuous color (ggplot's @geom_bin2d()@ default). @bin2dCount x y@
--   bins continuous x/y into a 12×12 grid and colors each cell by its
--   __observed count__ using Viridis (no z column — the count version of
--   'bin2d'). 'bin2d' (z average) corresponds to stat_summary_2d.
bin2dCount :: ColRef -> ColRef -> Layer
bin2dCount x y = mempty
  { lyKind = First (Just MBin2d)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  }

-- | [日本語]: geom_tile / geom_raster 相当。 連続 x/y を__セル中心__、 fill を
--   __離散カテゴリ__として矩形をベタ塗りする (幅/高さは格子間隔から自動・隙間
--   なし)。 bin2d と違い再ビニングせず 1 行=1 セルをそのまま塗る (= 決定境界の
--   res×res グリッド塗り)。 fill の離散色と離散凡例は colorBy 経路で自動 (重ねる
--   散布点と同じカテゴリ空間ならパレット一致)。 連続 fill の塗りは 'bin2d'
--   (再ビニング) を使う。
--   [English]: The equivalent of geom_tile / geom_raster. Treats
--   continuous x/y as a __cell center__ and fill as a __discrete category__,
--   filling rectangles solidly (width/height are derived
--   automatically from the grid spacing, with no gaps). Unlike bin2d, it
--   does not re-bin — one row is drawn as one cell as-is (used for
--   painting a decision-boundary res×res grid). The discrete fill color
--   and legend are handled automatically via the colorBy path (palettes
--   match an overlaid scatter's category space). For continuous fill, use
--   'bin2d' (which re-bins).
tile :: ColRef -> ColRef -> ColRef -> Layer
tile x y fill = mempty
  { lyKind = First (Just MTile)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByCol fill))
  }

-- | [日本語]: hexbin (= matplotlib @hexbin@ / ggplot @geom_hex@)。 連続 x/y を
--   __六角格子__で binning し、 各セルの__観測件数__を Viridis 連続色で塗る (=
--   散布過密の密度可視化)。 セル分割数は 'hexbinBins' で上書き (既定 30)。 矩形
--   ビンの 'bin2dCount' の六角版。 アルゴは d3-hexbin (Carr 1987) を binwidth
--   正規化空間で適用 (pointy-top)。
--   [English]: hexbin (like matplotlib's @hexbin@ / ggplot's @geom_hex@).
--   Bins continuous x/y into a __hexagonal grid__ and colors each cell by
--   its __observed count__ using a continuous Viridis color (a density
--   visualization for dense scatter). The number of cell divisions is
--   overridden via 'hexbinBins' (default 30); the hexagonal counterpart of
--   the rectangular-bin 'bin2dCount'. The algorithm applies d3-hexbin
--   (Carr 1987) in binwidth-normalized space (pointy-top).
hexbin :: ColRef -> ColRef -> Layer
hexbin x y = mempty
  { lyKind = First (Just MHexbin)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  }

-- | [日本語]: hexbin の x 方向セル分割数を指定 (= ggplot @bins@ / matplotlib
--   @gridsize@)。 既定 30。 'hexbin' に @<>@ で重ねる:
--   @layer (hexbin "x" "y" <> hexbinBins 40)@。 内部は 'lyBinCount' を流用
--   (histogram と共有フィールド)。
--   [English]: Sets the number of x-direction cell divisions for hexbin
--   (like ggplot's @bins@ / matplotlib's @gridsize@). Default 30. Layer it
--   onto 'hexbin' with @<>@: @layer (hexbin "x" "y" <> hexbinBins 40)@.
--   Internally reuses 'lyBinCount' (a field shared with histogram).
hexbinBins :: Int -> Layer
hexbinBins n = mempty { lyBinCount = Last (Just n) }

-- | [日本語]: P12: step plot (= 階段状 line)。
--   [English]: P12: a step plot (a staircase-shaped line).
step :: ColRef -> ColRef -> Layer
step x y = mempty
  { lyKind = First (Just MStep), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: stat-in 線形回帰 (= ggplot @geom_smooth(method="lm")@)。 純タグ
--   Layer。 回帰 fit は描画前に analyze-bridge の @resolveStats@ が hanalyze で
--   行い、 信頼帯 (band) + 回帰線 (line) に展開する。 装飾は通常 geom と同じ:
--   @statLm "x" "y" <> color N.red <> stroke 2@。 ★単体では描画されない (renderer
--   は MStatLM を skip)。 必ず bridge の saveSVGBoundStats 等で解決する。
--   [English]: An in-place (stat-in) linear regression (like ggplot's
--   @geom_smooth(method="lm")@). A pure tag Layer. Before drawing, the
--   analyze-bridge's @resolveStats@ performs the fit via hanalyze and
--   expands it into a confidence band (band) plus a regression line
--   (line). Decoration works the same as an ordinary geom:
--   @statLm "x" "y" <> color N.red <> stroke 2@. ★It is never drawn on its
--   own (the renderer skips MStatLM) — it must be resolved via the
--   bridge's saveSVGBoundStats or similar.
statLm :: ColRef -> ColRef -> Layer
statLm x y = mempty
  { lyKind = First (Just MStatLM), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: 信頼水準を指定できる線形回帰 stat。 'statLm' は 0.95 固定だが、
--   こちらは @lvl@ (例 0.99) を 'lyStatLevel' に持たせる。 resolveStats が band
--   幅に反映する。
--   [English]: A linear-regression stat that lets you specify the
--   confidence level. Whereas 'statLm' is fixed at 0.95, this one stores
--   @lvl@ (e.g. 0.99) in 'lyStatLevel'; resolveStats reflects it in the
--   band width.
statLmLevel :: ColRef -> ColRef -> Double -> Layer
statLmLevel x y lvl = (statLm x y)
  { lyStatLevel = Last (Just lvl) }

-- | [日本語]: stat-in B-spline 平滑 (= ggplot @geom_smooth()@)。 knot 数 n。
--   曲線のみ (帯なし)。 resolveStats が hanalyze で fit し line に展開。 装飾は
--   line に引き継がれる。
--   [English]: An in-place (stat-in) B-spline smoother (like ggplot's
--   @geom_smooth()@), with n knots. Curve only (no band). resolveStats fits
--   it via hanalyze and expands it into a line; decoration carries over to
--   the line.
statSmooth :: ColRef -> ColRef -> Int -> Layer
statSmooth x y n = mempty
  { lyKind = First (Just MStatSmooth), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyBinCount = Last (Just n) }

-- | [日本語]: 信頼帯つき B-spline 平滑。 'statSmooth' は曲線のみだが、 こちらは
--   'lyStatLevel' を Just にして「帯あり」を signal する。 resolveStats が bs
--   設計行列の confidenceBand で band+line に展開する。 既定水準は 0.95
--   (@statSmoothCI x y n@)。
--   [English]: A B-spline smoother with a confidence band. Whereas
--   'statSmooth' is curve-only, this one sets 'lyStatLevel' to Just to
--   signal "band included". resolveStats expands it into band+line via the
--   bs design matrix's confidenceBand. The default level is 0.95
--   (@statSmoothCI x y n@).
statSmoothCI :: ColRef -> ColRef -> Int -> Layer
statSmoothCI x y n = (statSmooth x y n)
  { lyStatLevel = Last (Just 0.95) }

-- | [日本語]: 多項式回帰 stat
--   (= ggplot @geom_smooth(method="lm", formula=y~poly(x,deg))@)。 次数 deg は
--   'lyBinCount' を流用。 resolveStats が @y ~ poly(x,deg)@ で fit し band+line に
--   展開。 信頼帯の水準は 'lyStatLevel' (既定 0.95)。 ★単体では描画されない
--   (renderer は MStatPoly を skip)。
--   [English]: A polynomial-regression stat (like ggplot's
--   @geom_smooth(method="lm", formula=y~poly(x,deg))@). The degree deg
--   reuses 'lyBinCount'. resolveStats fits @y ~ poly(x,deg)@ and expands it
--   into band+line; the confidence band's level is 'lyStatLevel' (default
--   0.95). ★It is never drawn on its own (the renderer skips MStatPoly).
statPoly :: ColRef -> ColRef -> Int -> Layer
statPoly x y deg = mempty
  { lyKind = First (Just MStatPoly), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyBinCount = Last (Just deg) }

-- | [日本語]: 残差 vs fitted 診断散布 (= base R @plot(lm)@ #1)。 @y ~ x@ で fit し
--   各点を (fitted, residual) に写した scatter に展開する (回帰診断)。 装飾は
--   scatter に引き継ぐ。 ★単体では描画されない (renderer は MStatResid を
--   skip)。 bridge resolveStats が必要。
--   [English]: A residual-vs-fitted diagnostic scatter (base R's
--   @plot(lm)@ #1). Fits @y ~ x@ and expands each point into a scatter
--   mapped to (fitted, residual) — a regression diagnostic. Decoration
--   carries over to the scatter. ★It is never drawn on its own (the
--   renderer skips MStatResid); it requires the bridge's resolveStats.
statResid :: ColRef -> ColRef -> Layer
statResid x y = mempty
  { lyKind = First (Just MStatResid), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: P11: stem / lollipop plot。
--   [English]: P11: a stem / lollipop plot.
stem :: ColRef -> ColRef -> Layer
stem x y = mempty
  { lyKind = First (Just MStem), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: TODO-11 (2026-05-27): area band (= 信頼区間 / 予測帯)。
--     x       = 共通 x 軸
--     yLow    = 下境界 y
--     yHigh   = 上境界 y
--   Render は PPath fill 1 枚 (= forward x-yLow + backward x-yHigh + close)。
--   alpha は layer modifier の `alpha` で指定 (= default 0.2)。
--   [English]: TODO-11 (2026-05-27): an area band (a confidence interval /
--   prediction band).
--     x       = the shared x axis
--     yLow    = the lower bound y
--     yHigh   = the upper bound y
--   Rendered as a single PPath fill (forward along x-yLow, backward along
--   x-yHigh, then close). alpha is set via the layer modifier `alpha`
--   (default 0.2).
band :: ColRef -> ColRef -> ColRef -> Layer
band x yLow yHigh = mempty
  { lyKind = First (Just MBand)
  , lyEncX = Last (Just x)
  , lyEncY = Last (Just yLow)
  , lyEncY2 = Last (Just yHigh)
  }

-- | [日本語]: streamgraph (= 中心化積層 area)。
--     x = 共通 x 軸 (連続、 例: 時間)
--     y = 各系列の値
--   系列分割は color aesthetic で行う (= 'bar' の群分けと同じ機構)。
--
--   > stream "t" "value" <> colorBy "series"
--
--   各 x 点で系列を積層し baseline を -(Σy)/2 から開始する (silhouette 中心化)。
--   wiggle 最小化 (ThemeRiver) は行わない。
--   [English]: A streamgraph (a centered, stacked area).
--     x = the shared x axis (continuous, e.g. time)
--     y = each series' value
--   Series are split via the color aesthetic (the same mechanism used for
--   grouping in 'bar').
--
--   > stream "t" "value" <> colorBy "series"
--
--   At each x point, series are stacked with the baseline starting at
--   -(Σy)/2 (silhouette centering). Wiggle minimization (ThemeRiver) is not
--   performed.
stream :: ColRef -> ColRef -> Layer
stream x y = mempty
  { lyKind = First (Just MStream), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | [日本語]: P2: violin plot。 ★ boxplot と同じく __値 1 列__を受ける。 群分けは
--   @<> groupBy "g"@ (色一律) / @<> colorBy "g"@ (群色+凡例) で付ける (ggplot
--   同型)。 群指定なしなら単一 violin。
--   [English]: P2: a violin plot. ★ Like boxplot, it takes a __single value column__.
--   Grouping is added with @<> groupBy "g"@ (uniform color) /
--   @<> colorBy "g"@ (per-group color + legend), matching ggplot. With no
--   group specified, it draws a single violin.
violin :: ColRef -> Layer
violin v = mempty { lyKind = First (Just MViolin), lyEncY = Last (Just v) }

-- | [日本語]: P3: strip plot。 ★ 値 1 列 + groupBy/colorBy で群分け。
--   [English]: P3: a strip plot. ★ A single value column, grouped via
--   groupBy/colorBy.
strip :: ColRef -> Layer
strip v = mempty { lyKind = First (Just MStrip), lyEncY = Last (Just v) }

-- | [日本語]: P3: swarm plot。 ★ 値 1 列 + groupBy/colorBy で群分け。
--   [English]: P3: a swarm plot. ★ A single value column, grouped via
--   groupBy/colorBy.
swarm :: ColRef -> Layer
swarm v = mempty { lyKind = First (Just MSwarm), lyEncY = Last (Just v) }

-- | [日本語]: P22: raincloud (= violin + box + strip 合成)。 ★ 値 1 列 +
--   groupBy/colorBy。 mark 直結合成。 @a \<+\> b@ は a を base、 b を重畳
--   sub-mark とする __単一 Layer__ を返す (= 戻り型 Layer 維持ゆえ
--   @raincloud v \<+\> ... \<\> groupBy g@ のような群修飾が従来どおり効く)。
--   b 側の overlay も平坦化して取り込む。 render は base + 各 overlay を
--   「親の群 (encX)・色 (colorBy)・値 (encY) を継承・自前の
--   kind/nudge/markWidth/side で」 描く。 1D 分布 mark (box/violin/strip/swarm)
--   の重畳を想定 (= raincloud / 自作 composite)。
--   [English]: P22: raincloud (a composite of violin + box + strip). ★ A
--   single value column, grouped via groupBy/colorBy. A direct mark
--   combinator: @a \<+\> b@ returns a __single Layer__ with a as the base
--   and b as the overlaid sub-mark (since the return type stays Layer,
--   group modifiers such as @raincloud v \<+\> ... \<\> groupBy g@ keep
--   working as before). b's own overlays are flattened in as well.
--   Rendering draws the base plus each overlay by "inheriting the
--   parent's group (encX), color (colorBy) and value (encY), with its own
--   kind/nudge/markWidth/side". Intended for overlaying 1D distribution
--   marks (box/violin/strip/swarm) — used by raincloud / custom
--   composites.
infixl 7 <+>
(<+>) :: Layer -> Layer -> Layer
a <+> b = a { lyOverlay = lyOverlay a ++ [b { lyOverlay = [] }] ++ lyOverlay b }

-- | [日本語]: P22: raincloud (= 半 violin + box + jitter strip の合成)。 ★ 専用
--   mark を廃し '<+>' による 3 sub-mark 合成の preset に降格 (= 位置決めは D1
--   つまみ nudge/markWidth/side に委譲)。 戻り型は Layer なので
--   @raincloud v \<\> groupBy g@ / @\<\> colorBy g@ は従来どおり群分けする。
--   [English]: P22: raincloud (a composite of a half violin + box +
--   jitter strip). ★ Retired as a dedicated mark and demoted to a preset
--   built from a 3-sub-mark composite via '<+>' (positioning is delegated
--   to the D1 knobs nudge/markWidth/side). Since the return type is Layer,
--   @raincloud v \<\> groupBy g@ / @\<\> colorBy g@ still group as before.
raincloud :: ColRef -> Layer
raincloud v =
      (violin  v <> side SideRight <> nudge 0.15    <> markWidth 0.40)
  <+> (boxplot v               <> nudge 0.00    <> markWidth 0.10)
  <+> (strip   v               <> nudge (-0.25) <> markWidth 0.18)

-- | [日本語]: 合成 Layer (base + overlay sub-mark) の値列レーン (= encY の
--   distinct・base 先頭・'colRefName' で重複除去)。 描画/Layout は各マークの
--   slot を「自 encY が此のレーン列の何番目か」で決める。 同一列なら 1 レーン
--   (= raincloud の重畳)、 複数列なら横並び (= distCols)。
--   [English]: The value-column lanes of a composite Layer (base + overlay
--   sub-marks) — the distinct encY values, base first, deduplicated via
--   'colRefName'. Drawing/layout decides each mark's slot by "which
--   position its own encY occupies among these lane columns". The same
--   column collapses to a single lane (raincloud's overlay); different
--   columns lay out side by side (distCols).
compositeLanes :: Layer -> [ColRef]
compositeLanes ly = foldl add [] [ c | l <- ly : lyOverlay ly, Just c <- [getLast (lyEncY l)] ]
  where add acc c = if any ((== colRefName c) . colRefName) acc then acc else acc ++ [c]

-- | [日本語]: P21: ridge / joyplot。 ★ 他 distribution mark と統一して
--   __値 1 列__を受ける。 群分けは @<> groupBy "g"@ / @<> colorBy "g"@ (=
--   box/violin と同じ)。 群指定なしは単一 density 風。 ridge は値→x・群→y の
--   向きが要るため、 ridge レイヤを含む spec は 'Graphics.Hgg.Spec.Setters.ridgeAutoFlip' で coord_flip
--   を自動適用する (値が x、 群が y に回る)。 内部表現は violin と同じ encY=値。
--   [English]: P21: ridge / joyplot. ★ Unified with the other distribution
--   marks, it takes a __single value column__. Grouping is added via
--   @<> groupBy "g"@ / @<> colorBy "g"@ (the same as box/violin); with no
--   group specified it looks like a single density curve. Since ridge
--   needs value→x and group→y, a spec containing a ridge layer
--   automatically applies coord_flip via 'Graphics.Hgg.Spec.Setters.ridgeAutoFlip' (value becomes x,
--   group becomes y). Internally represented the same way as violin,
--   encY=value.
ridge :: ColRef -> Layer
ridge v = mempty { lyKind = First (Just MRidge), lyEncY = Last (Just v) }

-- | [日本語]: P14: scatter jitter (= plotArea 比率 0..1)。
--   [English]: P14: scatter jitter (a plotArea ratio in 0..1).
jitterX, jitterY :: Double -> Layer
jitterX a = mempty { lyJitterX = Last (Just a) }
jitterY a = mempty { lyJitterY = Last (Just a) }

-- | [日本語]: frontend-settings v0.1 §2.4: histogram の bin 数 (= default 10)。
--   [English]: frontend-settings v0.1 §2.4: the number of histogram bins
--   (default 10).
binCount :: Int -> Layer
binCount n = mempty { lyBinCount = Last (Just n) }

-- | [日本語]: histogram の bin 幅 (= ggplot @geom_histogram(binwidth = w)@)。
--   'binWidth' を指定すると 'binCount' より優先され、 'histBinning' が ggplot
--   流 (boundary = w/2 で bin 原点を定める) の bin 化を行う。
--   [English]: The histogram bin width (like ggplot's
--   @geom_histogram(binwidth = w)@). Specifying 'binWidth' takes priority
--   over 'binCount', and 'histBinning' bins in the ggplot style
--   (determining the bin origin with boundary = w/2).
binWidth :: Double -> Layer
binWidth w = mempty { lyBinWidth = Last (Just w) }

-- | [日本語]: histogram の bin 化パラメタ (origin, binW, nBin) を決める単一
--   情報源。 render (Render.Basic) と y/x range (Layout.RangeOf) の双方がこれを
--   使い、 bin 境界・棒高・軸範囲を一致させる。
--
--   * 'lyBinWidth' 指定時: ggplot @bin_breaks_width@ と同式。 boundary = w/2 とし、
--     origin = boundary + floor((lo - boundary)/w) * w、 nBin = ceil((hi - origin)/w)。
--     これで R4DS の @binwidth=@ と同じ bin 境界・棒高になる。
--   * 未指定時: 従来どおり 'lyBinCount' (既定 30) で [lo,hi] を等分。
--
--   bin i は @[origin + i*binW, origin + (i+1)*binW)@、 値 v の所属は
--   @clamp 0 (nBin-1) (floor ((v - origin)/binW))@。
--   [English]: The single source of truth that determines the histogram
--   binning parameters (origin, binW, nBin). Both render (Render.Basic)
--   and the y/x range (Layout.RangeOf) use it, so bin boundaries, bar
--   heights and axis ranges stay consistent.
--
--   * When 'lyBinWidth' is given: the same formula as ggplot's
--     @bin_breaks_width@. With boundary = w/2, origin = boundary +
--     floor((lo - boundary)/w) * w and nBin = ceil((hi - origin)/w). This
--     gives the same bin boundaries and bar heights as R4DS's
--     @binwidth=@.
--   * When unspecified: as before, divides [lo,hi] evenly using
--     'lyBinCount' (default 30).
--
--   Bin i is @[origin + i*binW, origin + (i+1)*binW)@; a value v belongs to
--   @clamp 0 (nBin-1) (floor ((v - origin)/binW))@.
histBinning :: Layer -> (Double, Double) -> (Double, Double, Int)
histBinning ly (lo, hi) =
  case getLast (lyBinWidth ly) of
    Just w | w > 0 ->
      let boundary = w / 2
          shift    = fromIntegral (floor ((lo - boundary) / w) :: Int)
          origin   = boundary + shift * w
          nBin     = max 1 (ceiling ((hi - origin) / w))
      in (origin, w, nBin)
    _ ->
      let nBin = case getLast (lyBinCount ly) of
                   Just n | n > 0 -> n
                   _              -> 30
          binW = if hi > lo then (hi - lo) / fromIntegral nBin else 1
      in (lo, binW, nBin)

-- | [日本語]: hexbin の六角セル (中心 + 件数 + 6 頂点、 すべてデータ座標)。
--   [English]: A hexbin hex cell (center + count + six vertices, all in
--   data coordinates).
data HexCell = HexCell
  { hexCx    :: !Double             -- ^ [日本語]: セル中心 x (データ座標)。 [English]: Cell center x (data coordinates).
  , hexCy    :: !Double             -- ^ [日本語]: セル中心 y。 [English]: Cell center y.
  , hexCount :: !Int                -- ^ [日本語]: セルに入った点数。 [English]: Number of points in the cell.
  , hexVerts :: ![(Double, Double)] -- ^ [日本語]: 6 頂点 (pointy-top、 データ座標)。 [English]: The six vertices (pointy-top, data coordinates).
  } deriving (Show, Eq)

-- | [日本語]: 六角ビニング (d3-hexbin = Carr 1987)。 @bins@ = x 方向セル分割数。
--   (xmin,xmax)/(ymin,ymax) = データ範囲、 @pts@ = (x,y) 点列。 binwidth で正規化
--   した (u,v) 空間で点を六角セルに割当て件数を数え、 中心・6 頂点をデータ座標で
--   返す (= scale パイプラインでそのまま screen へ。 pointy-top)。 ★HS/PS で同式・
--   JS Math.round (= @floor (z+0.5)@) を使い byte 一致させる。
--   [English]: Hexagonal binning (d3-hexbin = Carr 1987). @bins@ is the
--   number of x-direction cell divisions; (xmin,xmax)/(ymin,ymax) is the
--   data range and @pts@ is the list of (x,y) points. Points are assigned
--   to hex cells in (u,v) space normalized by binwidth, counted, and
--   returned with center + six vertices in data coordinates (fed straight
--   into the scale pipeline to reach screen space; pointy-top). ★HS and PS
--   use the identical formula and JS Math.round (= @floor (z+0.5)@) to
--   match byte-for-byte.
hexbinCells :: Int -> (Double, Double) -> (Double, Double)
            -> [(Double, Double)] -> [HexCell]
hexbinCells bins (xmin, xmax) (ymin, ymax) pts
  | bins <= 0 || bwx <= 0 || bwy <= 0 || null pts = []
  | otherwise =
      [ mkCell (head grp) (length grp)
      | grp <- Data.List.group (Data.List.sort (map assign pts)) ]
  where
    bwx = (xmax - xmin) / fromIntegral bins
    bwy = (ymax - ymin) / fromIntegral bins
    dyv = sqrt 3 / 2                       -- = 1.5·r  (r = 1/√3、 dx=√3·r=1 に正規化)
    ruv = 1 / sqrt 3
    jsRound z = floor (z + 0.5) :: Int     -- JS Math.round (half-up)・HS=PS 一致用
    -- 点 → セルキー (pi, pj) (d3-hexbin verbatim)
    assign :: (Double, Double) -> (Int, Int)
    assign (x, y) =
      let u   = (x - xmin) / bwx
          v   = (y - ymin) / bwy
          py  = v / dyv
          pj  = jsRound py
          px  = u - (if odd pj then 0.5 else 0)     -- dx=1、 奇数行 0.5 シフト
          pii = jsRound px
          py1 = py - fromIntegral pj
      in if abs py1 * 3 > 1
           then let px1 = px - fromIntegral pii
                    pi2 = fromIntegral pii + (if px < fromIntegral pii then -1 else 1) / 2 :: Double
                    pj2 = pj + (if py < fromIntegral pj then -1 else 1)
                    px2 = px - pi2
                    py2 = py - fromIntegral pj2
                in if px1 * px1 + py1 * py1 > px2 * px2 + py2 * py2
                     then (jsRound (pi2 + (if odd pj then 1 else -1) / 2), pj2)
                     else (pii, pj)
           else (pii, pj)
    -- セルキー → HexCell (中心・頂点をデータ座標へ)
    mkCell :: (Int, Int) -> Int -> HexCell
    mkCell (pii, pj) n =
      let cu = fromIntegral pii + (if odd pj then 0.5 else 0)   -- × dx(=1)
          cv = fromIntegral pj * dyv
          cx = xmin + cu * bwx
          cy = ymin + cv * bwy
          vert k = let ang = fromIntegral k * pi / 3
                       vu  = sin ang * ruv
                       vv  = negate (cos ang) * ruv
                   in (xmin + (cu + vu) * bwx, ymin + (cv + vv) * bwy)
      in HexCell cx cy n (map vert [0 .. 5 :: Int])

-- | [日本語]: hexbin layer を解決して六角セルを返す (renderHexbin と count
--   colorbar が共有)。 x/y を 'resolveNum' で取り NaN を除いて zip、 bins (既定
--   30) で 'hexbinCells'。 render と凡例で__同じ count 域__を得るために 1 本に
--   集約する。
--   [English]: Resolves a hexbin layer and returns its hex cells (shared
--   by renderHexbin and the count colorbar). Takes x/y via 'resolveNum',
--   drops NaN, zips them, and calls 'hexbinCells' with bins (default 30).
--   Consolidated into a single function so render and the legend see the
--   __same count domain__.
hexbinLayerCells :: Resolver -> Layer -> [HexCell]
hexbinLayerCells r ly =
  case (getLast (lyEncX ly), getLast (lyEncY ly)) of
    (Just xr, Just yr) ->
      case (resolveNum r xr, resolveNum r yr) of
        (Just xv, Just yv) ->
          let pts = [ (x, y) | (x, y) <- zip (V.toList xv) (V.toList yv)
                             , not (isNaN x), not (isNaN y) ]
              bins = case getLast (lyBinCount ly) of Just b | b > 0 -> b; _ -> 30
          in if null pts then []
             else let xs = map fst pts; ys = map snd pts
                  in hexbinCells bins (minimum xs, maximum xs) (minimum ys, maximum ys) pts
        _ -> []
    _ -> []

-- | [日本語]: TODO-3a (2026-05-29): histogram の y 軸を密度
--   (= count / (total * binW)) に正規化。 PS Spec.histogramDensity と同等。
--   SVG export でも動くように HS 側にも実装 (= 旧来 HS は count のみで density
--   mode が機能しなかった)。
--   [English]: TODO-3a (2026-05-29): normalizes the histogram's y axis to
--   density (count / (total * binW)). Equivalent to PS's
--   Spec.histogramDensity. Also implemented on the HS side so it works for
--   SVG export too (previously HS only supported count, so density mode
--   didn't work).
histogramDensity :: Bool -> Layer
histogramDensity b = mempty { lyHistDensity = Last (Just b) }

-- | [日本語]: histogram / bar の bin 境界線 (= 各バーの白枠) を表示するか。
--   デフォルトは False (= ggplot 流フラットバー、 枠なし)。 True で bin 区切りが
--   見える。
--   [English]: Whether to show the bin border lines (a white outline per
--   bar) for histogram / bar. Default False (a flat ggplot-style bar with
--   no border); True makes the bin boundaries visible.
histBorder :: Bool -> Layer
histBorder b = mempty { lyHistBorder = Last (Just b) }

-- | [日本語]: density 曲線の下を塗りつぶす (= ggplot
--   @geom_density(aes(fill = …))@)。 群別 ('color') と 'alpha' を併用すると、
--   各群を群色 × alpha で塗る (R4DS Ch1 §1.5)。 既定 (未指定/False) は ggplot
--   同様 fill=NA = 線のみ。
--   [English]: Fills below the density curve (like ggplot's
--   @geom_density(aes(fill = …))@). Combined with per-group color
--   ('color') and 'alpha', each group is filled with its group color ×
--   alpha (R4DS Ch1 §1.5). The default (unspecified/False) is fill=NA as
--   in ggplot — line only.
densityFill :: Bool -> Layer
densityFill b = mempty { lyDensityFill = Last (Just b) }

-- | [日本語]: マーカーを中抜き (= ggplot @shape="circle open"@ /
--   @geom_point(fill = NA)@)。 塗りを透明にし、 点色で輪郭 (stroke) のみ描く。
--   'size' で輪郭円の直径、 'stroke' で線幅 (既定 1pt)。 重畳して「点を輪で
--   囲む」 強調に使う (R4DS Ch9 §9.6)。
--   [English]: Makes markers hollow (like ggplot's
--   @shape="circle open"@ / @geom_point(fill = NA)@). Fill is made
--   transparent, drawing only the outline (stroke) in the point color.
--   'size' controls the outline circle's diameter, 'stroke' its line
--   width (default 1pt). Used as an overlay to emphasize points by
--   "circling" them (R4DS Ch9 §9.6).
hollow :: Layer
hollow = mempty { lyHollow = Last (Just True) }

-- | [日本語]: 分布 mark (box/violin/strip/swarm) の slot 内横 offset。 値は
--   __slot 幅比__ (= ggplot @position_nudge@)。 正で右、 負で左。 raincloud の
--   「box を中央・strip を左・雲を右」のような重畳配置を組むのに使う (= 旧
--   raincloud のハードコード offset を置換)。
--   [English]: The horizontal offset within a slot for distribution marks
--   (box/violin/strip/swarm). The value is a __ratio of the slot width__
--   (like ggplot's @position_nudge@); positive moves right, negative
--   moves left. Used to build raincloud-style overlaid layouts such as
--   "box centered, strip to the left, cloud to the right" (replacing the
--   old hardcoded offsets in raincloud).
nudge :: Double -> Layer
nudge x = mempty { lyNudge = Last (Just x) }

-- | [日本語]: 分布 mark の幅 (= __slot 幅比・占有率__)。 各 mark の既定占有率
--   (box 0.5 / violin 0.7 / strip 0.4 / swarm 0.8) を上書きする。 raincloud
--   では box を細く (= 0.1 等) するのに使う。
--   [English]: The width of a distribution mark (a __ratio of the slot width — its occupancy__).
--   Overrides each mark's default occupancy
--   (box 0.5 / violin 0.7 / strip 0.4 / swarm 0.8). Used in raincloud to
--   thin the box (e.g. to 0.1).
markWidth :: Double -> Layer
markWidth w = mempty { lyMarkWidth = Last (Just w) }

-- | [日本語]: violin の片側化 (= 半 violin)。 @violin "v" <> side SideRight@ で
--   右半分のみ。 raincloud の「雲」 (= 片側 violin) に使う。 box/strip 等には
--   影響しない。
--   [English]: Makes a violin one-sided (a half violin). Example:
--   @violin "v" <> side SideRight@ shows only the right half. Used for
--   raincloud's "cloud" (a one-sided violin); has no effect on box/strip,
--   etc.
side :: Side -> Layer
side s = mempty { lySide = Last (Just s) }

-- | [日本語]: bar の position adjustment (= ggplot `position`)。 群分け (=
--   color/group aesthetic) があるとき 'PosDodge' / 'PosStack' / 'PosFill' で
--   並べ方を選ぶ。 既定 ('PosIdentity') は従来通り単色棒 (color を見ない)。
--   [English]: The position adjustment for bar (like ggplot's `position`).
--   When there is grouping (a color/group aesthetic), choose the layout
--   with 'PosDodge' / 'PosStack' / 'PosFill'. The default ('PosIdentity')
--   draws a single-color bar as before (ignoring color).
--
--   > bar "cat" "y" <> colorBy "grp" <> position PosDodge
position :: Position -> Layer
position p = mempty { lyPosition = Last (Just p) }

-- | [日本語]: 固定 shape (= layer 全体に適用・ggplot @shape=@)。 bare=固定。
--   'shapeBy' (列で map) より優先される ('Graphics.Hgg.Render.Common.pointShapeAt' 参照)。
--   [English]: A fixed shape (applies to the whole layer; like ggplot's
--   @shape=@). A bare value is fixed and takes priority over 'shapeBy'
--   (which maps from a column) — see 'Graphics.Hgg.Render.Common.pointShapeAt'.
shape :: MarkShape -> Layer
shape s = mempty { lyShape = Last (Just s) }

-- | [日本語]: C-6: shape categorical encoding 列。
--   [English]: C-6: the shape categorical-encoding column.
shapeBy :: ColRef -> Layer
shapeBy c = mempty { lyShapeBy = Last (Just c) }

-- | [日本語]: C-6: cat 名 → MarkShape 1 件追加 (= 複数 entry は <> で合成)。
--   [English]: C-6: adds a single cat name → MarkShape entry (combine
--   multiple entries with <>).
shapeMapEntry :: Text -> MarkShape -> Layer
shapeMapEntry v s = mempty { lyShapeMap = [ ShapeMapEntry { smeValue = v, smeShape = s } ] }

-- | [日本語]: C-6: size continuous encoding 列。
--   [English]: C-6: the size continuous-encoding column.
sizeBy :: ColRef -> Layer
sizeBy c = mempty { lySizeBy = Last (Just c) }

-- | [日本語]: alpha (= 不透明度) を連続値の列で encode する (= ggplot
--   @scale_alpha@・@aes(alpha = col)@)。 列値 min..max を alpha
--   @[0.1, 1.0]@ に線形 map (ggplot 既定 range)。 固定 alpha は bare 'alpha'
--   (案2 = bare 固定 / `*By` = map)。
--   [English]: Encodes alpha (opacity) from a continuous-valued column
--   (like ggplot's @scale_alpha@ / @aes(alpha = col)@). Linearly maps the
--   column's min..max to alpha @[0.1, 1.0]@ (ggplot's default range). A
--   fixed alpha uses the bare 'alpha' (convention: bare = fixed, `*By` =
--   mapped).
--
-- > scatter "x" "y" <> alphaBy "weight"
alphaBy :: ColRef -> Layer
alphaBy c = mempty { lyAlphaBy = Last (Just c) }

-- | [日本語]: 固定 linetype (= ggplot linetype="dashed")。 line 系 mark に適用。
--   例: @line "x" "y" <> linetype LtDashed@
--   [English]: A fixed linetype (like ggplot's linetype="dashed"), applied
--   to line-family marks. Example: @line "x" "y" <> linetype LtDashed@
linetype :: LineType -> Layer
linetype lt = mempty { lyLinetype = Last (Just lt) }

-- | [日本語]: categorical linetype encoding 列 (= ggplot linetype=factor(g))。
--   line を群ごとに分割し各群へ巡回 LineType ('lineTypeForIndex') を割当。
--   例: @line "x" "y" <> linetypeBy (ColByName "grp")@
--   [English]: A categorical linetype-encoding column (like ggplot's
--   linetype=factor(g)). Splits the line by group and assigns each group
--   a cycled LineType ('lineTypeForIndex'). Example:
--   @line "x" "y" <> linetypeBy (ColByName "grp")@
linetypeBy :: ColRef -> Layer
linetypeBy c = mempty { lyLinetypeBy = Last (Just c) }

-- | [日本語]: C-step trellis 色一貫性: 全データ cat 出現順を Layer に注入。
--   [English]: C-step trellis color consistency: injects the full
--   dataset's category order into the Layer.
colorCats :: [Text] -> Layer
colorCats cs = mempty { lyColorCats = cs }

-- | [日本語]: categorical 水準の既定順 (= ggplot2 の factor 既定 = アルファベット
--   順)。 色 / x 軸 / shape の distinct を取るときに使い、 R4DS と凡例・色・並びを
--   一致させる。 明示順が要るとき (fct_infreq 等) は 'colorCats' / @xCatOrder@ で
--   上書きする。
--   [English]: The default order of categorical levels (matching
--   ggplot2's factor default — alphabetical). Used when taking distinct
--   values for color / x axis / shape, to keep legend, color and ordering
--   consistent with R4DS. When an explicit order is needed (e.g.
--   fct_infreq), override it with 'colorCats' / @xCatOrder@.
orderedCats :: [Text] -> [Text]
orderedCats = Data.List.sort . Data.List.nub

-- | [日本語]: 列の平均値を水平線として描画 (= PlotConfig.showMean)。
--   [English]: Draws the column's mean as a horizontal line
--   (PlotConfig.showMean).
statMean :: ColRef -> Layer
statMean c = mempty
  { lyKind = First (Just MStatMean), lyEncY = Last (Just c) }

-- | [日本語]: 列の中央値を水平線として描画 (= PlotConfig.showMedian)。
--   [English]: Draws the column's median as a horizontal line
--   (PlotConfig.showMedian).
statMedian :: ColRef -> Layer
statMedian c = mempty
  { lyKind = First (Just MStatMedian), lyEncY = Last (Just c) }

-- | [日本語]: parallel coordinates plot。 各 col が縦軸となり、 各 row を全軸
--   cross する折線で表現。 hover で row 強調 (= 後追い)。
--   [English]: A parallel-coordinates plot. Each col becomes a vertical
--   axis, and each row is drawn as a polyline crossing all axes. Row
--   highlighting on hover is a follow-up feature.
parallelCoords :: [ColRef] -> Layer
parallelCoords cols = mempty
  { lyKind = First (Just MParallel), lyHover = cols }

-- | [日本語]: HBM ModelGraph DAG を描画する layer。 内部 builder で使う直接
--   constructor。 ユーザは 'Graphics.Hgg.DAG.dagPlot' (= Graph a + ~> 経由) を
--   使う方が良い。
--   [English]: A layer that draws an HBM ModelGraph DAG. A direct
--   constructor used by the internal builder. Users are better off using
--   'Graphics.Hgg.DAG.dagPlot' (via Graph a + ~>).
dagFromLists :: [DAGNode] -> [DAGEdge] -> DAGLayoutAlgorithm -> Layer
dagFromLists nodes edges algo = mempty
  { lyKind = First (Just MDAG)
  , lyDAG  = Last (Just (DAGSpec nodes edges algo [])) }

-- | [日本語]: dsPlates も指定する版。
--   [English]: The variant that also specifies dsPlates.
dagFromListsWithPlates
  :: [DAGNode] -> [DAGEdge] -> DAGLayoutAlgorithm -> [DAGPlate] -> Layer
dagFromListsWithPlates nodes edges algo plates = mempty
  { lyKind = First (Just MDAG)
  , lyDAG  = Last (Just (DAGSpec nodes edges algo plates)) }

-- | [日本語]: DAGNode constructor (= kind + 分布名なし)。
--   [English]: A DAGNode constructor (kind, with no distribution name).
dagNode :: Text -> Text -> DAGNodeKind -> Double -> Double -> DAGNode
dagNode i l k x y = DAGNode i l k Nothing x y

-- | [日本語]: 分布名付き DAGNode constructor (= PyMC 風 "name ~ dist" 表示用)。
--   [English]: A DAGNode constructor with a distribution name (for a
--   PyMC-style "name ~ dist" display).
dagNodeDist :: Text -> Text -> DAGNodeKind -> Text -> Double -> Double -> DAGNode
dagNodeDist i l k dist x y = DAGNode i l k (Just dist) x y

-- | [日本語]: DAGEdge constructor。
--   [English]: A DAGEdge constructor.
dagEdge :: Text -> Text -> DAGEdge
dagEdge f t = DAGEdge f t Nothing Nothing

-- | [日本語]: 互換用 shortcut: 既存 demo / test 用 (= NodeLatent +
--   LayoutManual)。 新規 API は Graphics.Hgg.DAG.dagPlot を使う。
--   [English]: A compatibility shortcut for existing demos / tests
--   (NodeLatent + LayoutManual). New code should use
--   Graphics.Hgg.DAG.dagPlot.
dag :: [DAGNode] -> [DAGEdge] -> Layer
dag nodes edges = dagFromLists nodes edges LayoutManual

-- | [日本語]: MCMC trace plot (single chain)。 iteration vs parameter 値の
--   line。 mark kind は MTrace (= alias for MLine、 frontend で区別可能)。
--   [English]: An MCMC trace plot (single chain): a line of iteration vs.
--   parameter value. Its mark kind is MTrace (an alias for MLine,
--   distinguishable by the frontend).
trace :: ColRef -> ColRef -> Layer
trace iterCol valCol = mempty
  { lyKind = First (Just MTrace)
  , lyEncX = Last (Just iterCol)
  , lyEncY = Last (Just valCol)
  }

-- | [日本語]: multi-chain trace。 chain 列で色分け、 connect group も chain 列
--   (= chain 内で連結、 chain 跨ぎ無し)。 PlotConfig.StreamingTracePlot 等価。
--   [English]: A multi-chain trace. Colored by the chain column; the
--   connect group is also the chain column (connected within a chain, no
--   crossing between chains). Equivalent to
--   PlotConfig.StreamingTracePlot.
traceLines :: ColRef -> ColRef -> ColRef -> Layer
traceLines iterCol valCol chainCol =
  trace iterCol valCol
    <> colorBy chainCol
    <> connectGroup chainCol
    <> stroke 1.0
