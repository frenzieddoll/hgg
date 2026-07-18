-- |
-- Module      : Graphics.Hgg.Spec.Constructors
-- Description : Layer constructors (= 各 mark の最小起点、 mark カタログ)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Graphics.Hgg.Spec' の module 分割で切り出し。 mark ごとの Layer
-- 構築子 ('scatter' / 'line' / 'bar' / ... / 'customMark') と mark 固有 setter
-- ('binCount' / 'jitterX' / 'shape' / 'statLm' 系等)、 hexbin の binning
-- ('HexCell') を持つ。 中身は等質な mark カタログ (辞書的) ゆえ 1 module に
-- まとめる (Phase 55 A1 で user 合意)。 公開 API は従来どおり
-- 'Graphics.Hgg.Spec' (facade) が re-export する。 挙動・出力は完全に不変。
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
module Graphics.Hgg.Spec.Constructors
  ( -- * 基本 mark
    scatter, line, bar, histogram, histogramDensity
  , heatmap, boxplot, density, densityFill, freqpoly
  , scatterPoints, linePoints, unzipPoint2
    -- * custom mark (Phase 51)
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

-- | Phase 51: custom mark を定義する公開 API。 core (@MarkKind@ の閉列挙) を触らず
-- 新しいプロット型を足す拡張点。 @cid@ = 安定 mark 識別子 (PS registry dispatch の鍵)、
-- @draw@ = 'RenderCtx' を受け取り 'Primitive' 列を返す描画 closure。 データは closure に
-- 閉じ込めても、 'rcResolver' 経由で layer 束縛列を引いてもよい。
--
-- HS は closure を直接呼んで描く (SVG/PDF/Rasterific)。 PS canvas で parity が欲しい時は
-- 同じ @cid@ で PS registry に draw 関数を手登録する (無ければ HS 専用)。
--
-- > customMark "myElbow" $ \ctx -> [ PLine (uncurry Point (rcProjectXY ctx 0 0)) ... ]
customMark :: Text -> (RenderCtx -> [Primitive]) -> Layer
customMark cid draw = mempty
  { lyKind   = First (Just MCustom)
  , lyCustom = Last (Just (CustomMark cid Aeson.Null draw)) }

-- | option 付き 'customMark'。 @opts@ は PS registry の draw 関数へ渡る serializable JSON。
customMarkWith :: Text -> Value -> (RenderCtx -> [Primitive]) -> Layer
customMarkWith cid opts draw = mempty
  { lyKind   = First (Just MCustom)
  , lyCustom = Last (Just (CustomMark cid opts draw)) }

-- | x / y encoding 列を単独で束ねる 'Layer' setter。 mark 種別に依らず合成でき、 custom mark を
-- 「一級 mark」化する (= 軸 range が 'lyEncX'/'lyEncY' から自動計算され、 @df |>>@ とも連携)。
-- 既存 mark の encoding 上書きにも使える。 custom mark の名前付き combinator は普通こう書く:
--
-- > dendrogram :: ColRef -> ColRef -> Layer
-- > dendrogram x y = customMark "dendrogram" (drawFromCols x y) <> encX x <> encY y
-- > -- 使う側: layer (dendrogram "leaf" "height")  ← scatter x y と同じ使い勝手
encX :: ColRef -> Layer
encX x = mempty { lyEncX = Last (Just x) }

encY :: ColRef -> Layer
encY y = mempty { lyEncY = Last (Just y) }

-- | Phase 30 A7: 2D scatter ('Point2' 直入れ・3D 'Graphics.Hgg.ThreeD.Spec.scatter3DPoints'
--   と対称)。 内部は @scatter (inline xs) (inline ys)@ に等価 (= x/y を inline 列に分解)
--   なので Render/JSON/PS 無改修。
--
-- > scatterPoints [Point2 1 2, Point2 3 4]
scatterPoints :: [Point2] -> Layer
scatterPoints pts = scatter (inline xs) (inline ys)
  where (xs, ys) = unzipPoint2 pts

-- | Phase 30 A7: 2D line ('Point2' 直入れ・3D 'Graphics.Hgg.ThreeD.Spec.line3DPoints'
--   と対称)。 内部は @line (inline xs) (inline ys)@ に等価。
linePoints :: [Point2] -> Layer
linePoints pts = line (inline xs) (inline ys)
  where (xs, ys) = unzipPoint2 pts

-- | '[Point2]' を x / y の 'Double' リストに分解 ('scatterPoints' / 'linePoints' 用)。
unzipPoint2 :: [Point2] -> ([Double], [Double])
unzipPoint2 = unzip . map (\(Point2 x y) -> (x, y))

-- | Phase 26 A2: vector field (quiver)。 各 (x,y) に成分 (u,v) の矢印を描く
--   (= matplotlib @quiver@)。 矢印長は autoscale (= 最長矢印がデータ対角の ~8%)
--   に 'arrowScale' 倍を掛けた長さ。 列バインドは @df |>> quiver \"x\" \"y\" \"u\" \"v\"@。
--   矢印を magnitude (= √(u²+v²)) で連続色マップするには 'arrowColorByMagnitude'。
quiver :: ColRef -> ColRef -> ColRef -> ColRef -> Layer
quiver x y u v = mempty
  { lyKind = First (Just MQuiver)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyEncU = Last (Just u), lyEncV = Last (Just v) }

-- | Phase 26 A2: quiver 矢印長の倍率 (autoscale × この値・既定 1)。 値を上げると矢印が長く。
arrowScale :: Double -> Layer
arrowScale s = mempty { lyArrowScale = Last (Just s) }

-- | Phase 26 A2: quiver の矢印を magnitude (= √(u²+v²)) で連続色マップする (+ 既定 OFF)。
--   色は連続パレット (viridis 系)。 OFF 時は単色 ('color' / theme)。
arrowColorByMagnitude :: Layer
arrowColorByMagnitude = mempty { lyArrowMagnitude = Last (Just True) }

-- | Phase 11 A6: データ駆動テキストラベル (= ggplot @geom_text@)。 各 (x,y) 点に lab 列の
--   文字を描く。 'annotate' (固定 1 点) と違い列駆動で点数ぶん出る。
text :: ColRef -> ColRef -> ColRef -> Layer
text x y lab = mempty
  { lyKind = First (Just MText), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyLabel = Last (Just lab) }

-- | Phase 11 A6: 背景付きテキストラベル (= ggplot @geom_label@)。 'text' と同じだが
--   各文字の背後に角丸矩形を敷く (= 重なる点の上でも読みやすい)。
label :: ColRef -> ColRef -> ColRef -> Layer
label x y lab = mempty
  { lyKind = First (Just MLabel), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyLabel = Last (Just lab) }

-- | Phase 11 A6-2: Q-Q plot (= ggplot @stat_qq@ / @geom_qq@)。 sample 列のみを取り、
--   ソートした order statistic y_(i) を y、 理論正規分位点 Φ⁻¹((i-0.5)/n) を x に置いて
--   点を描く (= 正規性の視覚診断)。 理論分位点は render / range 側で算出するため、
--   ここでは sample を encY に保持するだけ (encX 列は持たない)。
qq :: ColRef -> Layer
qq sample = mempty
  { lyKind = First (Just MQQ), lyEncY = Last (Just sample) }

-- | Phase 11 A6-4: ECDF plot (= ggplot @stat_ecdf@)。 sample 列 (encX) をソートして
--   右連続の経験累積分布 F(x)=#(≤x)/n を階段状に描く (y∈[0,1])。
ecdf :: ColRef -> Layer
ecdf sample = mempty
  { lyKind = First (Just MEcdf), lyEncX = Last (Just sample) }

-- | Phase 11 A6-4b: linerange (= ggplot @geom_linerange@)。 各 (x,y) に縦線 y±err を描く。
lineRange :: ColRef -> ColRef -> ColRef -> Layer
lineRange x y err = mempty
  { lyKind = First (Just MLineRange), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | Phase 11 A6-4b: pointrange (= ggplot @geom_pointrange@)。 linerange + 中心点。
pointRange :: ColRef -> ColRef -> ColRef -> Layer
pointRange x y err = mempty
  { lyKind = First (Just MPointRange), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | Phase 11 A6-4b: crossbar (= ggplot @geom_crossbar@)。 幅付き箱 (y±err) + 中央水平線。
crossbar :: ColRef -> ColRef -> ColRef -> Layer
crossbar x y err = mempty
  { lyKind = First (Just MCrossbar), lyEncX = Last (Just x)
  , lyEncY = Last (Just y), lyErrorY = Last (Just err) }

-- | Phase 11 A6-4c: stat_function (= ggplot @stat_function@ / @geom_function@)。
--   関数 f を [xLo, xHi] で n 点サンプルし、 inline 列の line layer を生成する。
--   関数自体は JSON 化できないため **構成時にサンプル点へ焼き込む** (= spec には点列が入り、
--   canvas backend は通常の line として描く)。 n<2 は 2 に切り上げ。
statFunction :: (Double -> Double) -> Double -> Double -> Int -> Layer
statFunction f xLo xHi n =
  let m  = max 2 n
      xs = [ xLo + (xHi - xLo) * fromIntegral i / fromIntegral (m - 1) | i <- [0 .. m - 1] ]
      ys = map f xs
  in line (ColNum (V.fromList xs)) (ColNum (V.fromList ys))

histogram :: ColRef -> Layer
histogram x = mempty
  { lyKind = First (Just MHistogram), lyEncX = Last (Just x) }

-- | 頻度多角形 (Ch10 EDA, Phase 28): @geom_freqpoly(aes(x = …))@ 相当。 histogram と
--   同じ bin 化で各 bin の count を求め、 bin 中心を折れ線で結ぶ。 bin 幅は
--   'binWidth' / 'binCount'、 after_stat(density) は 'histogramDensity' True で
--   流用 (= histogram と同じフラグ)。 color aesthetic ('colorBy') で群分割すると
--   群ごとに別色の折れ線を重ねる (cut 別 price 分布の比較等)。
freqpoly :: ColRef -> Layer
freqpoly x = mempty
  { lyKind = First (Just MFreqPoly), lyEncX = Last (Just x) }

-- | Ch10 EDA (Phase 28): 2 カテゴリ変数の件数 (= ggplot @geom_count()@ / @stat_sum@)。
--   @countXY x y@ は (x,y) のカテゴリ組合せごとに観測件数を集計し、 各セル中心に
--   面積 ∝ 件数 (= 半径 ∝ √件数) の点を描く。 'size' で最大半径 px を上書き可。
countXY :: ColRef -> ColRef -> Layer
countXY x y = mempty
  { lyKind = First (Just MCount), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | MCMC autocorrelation plot (P19、 Phase 6 A4): 1 列の時系列から lag-k 自己相関 r(τ)
--   を計算し bar chart で表示。 max lag は 'autocorrMaxLag'、 default は 40。
--   ±1.96/√N の significance band も同時描画。
--
--   r(τ) = Σ(x_t - μ)(x_{t+τ} - μ) / Σ(x_t - μ)²
--
--   matplotlib との対応: `plt.acorr(x, maxlags=40)` 相当 (= 但し片側のみ)。
autocorr :: ColRef -> Layer
autocorr c = mempty
  { lyKind = First (Just MAutocorr)
  , lyEncX = Last (Just c)
  }

-- | autocorr の max lag (= 'autocorr' と '<>' で組合せ)。 default 40。
autocorrMaxLag :: Int -> Layer
autocorrMaxLag n = mempty { lyMaxLag = Last (Just n) }

-- | Effective Sample Size plot (P20、 Phase 6 A5): chain ごとに ESS bar を描画。
--   chain group は 'chain' で指定 (= 'ess vals <> chain chainCol')。
--   chain 未指定なら全体を 1 chain として 1 bar。
--
--   ESS = N / (1 + 2 Σ |r(τ)|)  (= τ=1 から r(τ) > 0 まで)
--
--   matplotlib / arviz 対応: `az.plot_ess(idata)` の chain ごと bar (= 簡略版)。
-- | ESS 棒グラフ (Phase 8 B13): encX = パラメータ/chain 名 (categorical)、
-- encY = 計算済み ESS 値。 ESS の計算は統計ライブラリ (analyze 側) の責務で、
-- plot は値を棒にするだけ (= ggplot/bayesplot mcmc_neff 流の「計算と描画の分離」)。
ess :: ColRef -> ColRef -> Layer
ess nameCol essCol = mempty
  { lyKind = First (Just MEss)
  , lyEncX = Last (Just nameCol)
  , lyEncY = Last (Just essCol)
  }

-- | chain group 列を設定 (= 'autocorr' / 'ess' で chain 分け、 MTrace でも将来使用)。
chain :: ColRef -> Layer
chain c = mempty { lyChain = Last (Just c) }

-- | Forest plot (Phase 6 A2): 各 row が「label + 点推定 + CI」 の horizontal CI bar 群。
--
--   引数: label 列 (= categorical/text)、 point estimate 列、 ± 半幅 列 (= 対称 CI)。
--
--   * y 軸: label
--   * x 軸: estimate
--   * 中央 vertical 線: 'forestNull' (= default 0、 メタ解析慣例で OR は 1)
--
--   asymmetric CI (= lo / hi 個別) は将来。 現状は対称 CI のみ。
forest :: ColRef -> ColRef -> ColRef -> Layer
forest labelCol estCol errCol = mempty
  { lyKind   = First (Just MForest)
  , lyEncX   = Last (Just estCol)
  , lyEncY   = Last (Just labelCol)
  , lyErrorX = Last (Just errCol)
  }

-- | Forest plot の null effect 位置 (= 縦 0 線、 メタ解析の reference)。 default 0。
-- リスク比 / オッズ比 を log scale で扱う場合は 0 (= log 1)、 線形なら 0 (= 差)。
forestNull :: Double -> Layer
forestNull v = mempty { lyMaxLag = Last (Just (round v)) }
  -- 流用: lyMaxLag を null position の Int で再利用 (= round)。
  -- TODO: Double-precision null position field を別途 (= 当面 Int で十分)

-- | Funnel plot (Phase 6 A3): 効果量 vs 標準誤差の散布図 + 95% 信頼区間 envelope。
--
--   引数: 効果量 (effect) 列、 標準誤差 (SE) 列。 出版 bias 確認に使う。
--
--   * x 軸: effect (= estimate)
--   * y 軸: SE (= 上方が精度高、 下方が精度低)
--   * 中央 vertical 線: pooled mean (= データから算出)
--   * diagonal 線: pooled ± 1.96 * SE の envelope
funnel :: ColRef -> ColRef -> Layer
funnel effectCol seCol = mempty
  { lyKind = First (Just MFunnel)
  , lyEncX = Last (Just effectCol)
  , lyEncY = Last (Just seCol)
  }

-- | Box plot。 ★ Phase 36: 値 1 列を受ける。 群分けは @<> groupBy "g"@ (色一律) /
--   @<> colorBy "g"@ (群色+凡例) で付ける (ggplot 同型)。 群指定なしなら単一 box。
boxplot :: ColRef -> Layer
boxplot vals = mempty
  { lyKind = First (Just MBox), lyEncY = Last (Just vals) }

-- | ★ Phase 36: 群で分けて配置するチャネル (= ggplot @aes(group=)@)。 色は付けない
--   (一律。 色は 'color' / 'colorBy' で別途)。 distribution mark (boxplot/violin 等) では
--   群ごとに集約を作りカテゴリ x に並べる。 内部表現は encX (= 既存の群配置機構を流用)。
--   ⚠ @Data.List.groupBy@ と同名なので、 両方 import する場合は qualified 推奨。
groupBy :: ColRef -> Layer
groupBy g = mempty { lyEncX = Last (Just g) }

-- | Density plot: x 列の値ベクター で Gaussian KDE 曲線。
density :: ColRef -> Layer
density x = mempty
  { lyKind = First (Just MDensity), lyEncX = Last (Just x) }

-- | Phase 8 B16: pairs 対角用 density。 y 軸目盛りは値範囲 (= 行の変数値、 散布図行と
-- 共有)、 KDE 曲線は panel 高さに独立正規化して描く (= seaborn pairplot 対角の挙動)。
densityNorm :: ColRef -> Layer
densityNorm x = mempty
  { lyKind = First (Just MDensity), lyEncX = Last (Just x)
  , lyDensityNorm = Last (Just True) }

-- | Phase 26 S4-d: Pie chart (= encX cat, encY 値合計の扇)。
pie :: ColRef -> ColRef -> Layer
pie x y = mempty
  { lyKind = First (Just MPie), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 26 S5-c: Waterfall chart (= encX cat, encY delta、 累積 bar)。
waterfall :: ColRef -> ColRef -> Layer
waterfall x y = mempty
  { lyKind = First (Just MWaterfall), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 26 S4 / Phase 11 A6-3 (= Heatmap): x = カテゴリ, y = カテゴリ, value = 数値。
-- |   各 (x,y) セルを value の連続色 (Viridis) で塗る。 value は ColorByContinuous で表現。
-- |   PS heatmap と対応。
heatmap :: ColRef -> ColRef -> ColRef -> Layer
heatmap x y v = mempty
  { lyKind = First (Just MHeatmap)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous v))
  }

-- | Phase 26 S5-e-1: Contour / binned heatmap (= 連続 x/y/z、 grid 化して
-- |   セル平均を Viridis 色マッピング)。 ResponseSurface の基盤。
-- |   color は ColorByContinuous で z 列を表現。
contour :: ColRef -> ColRef -> ColRef -> Layer
contour x y z = mempty
  { lyKind = First (Just MContour)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | Phase 24 A4: filled contour (= matplotlib @contourf@ / ggplot
-- @geom_contour_filled@)。 等値帯を Viridis 連続色で塗る。 入力が規則 grid
-- (x 固有値 × y 固有値が全組存在) なら補間せず直入力、 散布なら k 近傍 IDW で
-- 格子化 ('Graphics.Hgg.Math.Griddata')。 線の 'contour' と重畳すると
-- matplotlib の contourf+contour 同等。
contourFilled :: ColRef -> ColRef -> ColRef -> Layer
contourFilled x y z = mempty
  { lyKind = First (Just MContourFilled)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | Phase 24 A4: 等高線の本数 (既定 8)。 @contour x y z <> contourLevels 12@。
contourLevels :: Int -> Layer
contourLevels n = mempty { lyContourLevels = Last (Just n) }

-- | Phase 24 A4: 等高線レベルの明示指定 (本数指定より優先)。
contourBreaks :: [Double] -> Layer
contourBreaks bs = mempty { lyContourBreaks = Last (Just bs) }

-- | binned heatmap (= ggplot geom_bin2d / stat_summary_2d)。 連続 x/y/z を
-- nBins×nBins の grid に binning し、 各セルの z 平均を連続色 (Viridis) で塗る。
-- 'contour' (等高線) の塗り版。 ResponseSurface の塗り基盤。
bin2d :: ColRef -> ColRef -> ColRef -> Layer
bin2d x y z = mempty
  { lyKind = First (Just MBin2d)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByContinuous z))
  }

-- | Ch10 EDA (Phase 28): 2D bin の**件数**を連続色で塗る (= ggplot @geom_bin2d()@ 既定)。
--   @bin2dCount x y@ は連続 x/y を 12×12 grid に binning し、 各セルの**観測件数**を
--   Viridis で塗る (z 列なし = 'bin2d' の count 版)。 'bin2d' (z 平均) は stat_summary_2d 相当。
bin2dCount :: ColRef -> ColRef -> Layer
bin2dCount x y = mempty
  { lyKind = First (Just MBin2d)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  }

-- | geom_tile / geom_raster 相当 (Phase 60)。 連続 x/y を**セル中心**、 fill を**離散カテゴリ**
-- として矩形をベタ塗りする (幅/高さは格子間隔から自動・隙間なし)。 bin2d と違い再ビニングせず
-- 1 行=1 セルをそのまま塗る (= 決定境界の res×res グリッド塗り)。 fill の離散色と離散凡例は
-- colorBy 経路で自動 (重ねる散布点と同じカテゴリ空間ならパレット一致)。 連続 fill の塗りは
-- 'bin2d' (再ビニング) を使う。
tile :: ColRef -> ColRef -> ColRef -> Layer
tile x y fill = mempty
  { lyKind = First (Just MTile)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyColor = Last (Just (ColorByCol fill))
  }

-- | Phase 40: hexbin (= matplotlib @hexbin@ / ggplot @geom_hex@)。 連続 x/y を**六角格子**で
--   binning し、 各セルの**観測件数**を Viridis 連続色で塗る (= 散布過密の密度可視化)。
--   セル分割数は 'hexbinBins' で上書き (既定 30)。 矩形ビンの 'bin2dCount' の六角版。
--   アルゴは d3-hexbin (Carr 1987) を binwidth 正規化空間で適用 (pointy-top)。
hexbin :: ColRef -> ColRef -> Layer
hexbin x y = mempty
  { lyKind = First (Just MHexbin)
  , lyEncX = Last (Just x), lyEncY = Last (Just y)
  }

-- | Phase 40: hexbin の x 方向セル分割数を指定 (= ggplot @bins@ / matplotlib @gridsize@)。
--   既定 30。 'hexbin' に @<>@ で重ねる: @layer (hexbin "x" "y" <> hexbinBins 40)@。
--   内部は 'lyBinCount' を流用 (histogram と共有フィールド)。
hexbinBins :: Int -> Layer
hexbinBins n = mempty { lyBinCount = Last (Just n) }

-- | P12: step plot (= 階段状 line)。
step :: ColRef -> ColRef -> Layer
step x y = mempty
  { lyKind = First (Just MStep), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 16: stat-in 線形回帰 (= ggplot @geom_smooth(method="lm")@)。 純タグ Layer。
--   回帰 fit は描画前に analyze-bridge の @resolveStats@ が hanalyze で行い、 信頼帯 (band) +
--   回帰線 (line) に展開する。 装飾は通常 geom と同じ: @statLm "x" "y" <> color N.red <> stroke 2@。
--   ★単体では描画されない (renderer は MStatLM を skip)。 必ず bridge の saveSVGBoundStats 等で解決する。
statLm :: ColRef -> ColRef -> Layer
statLm x y = mempty
  { lyKind = First (Just MStatLM), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | Phase 16 B1: 信頼水準を指定できる線形回帰 stat。 'statLm' は 0.95 固定だが、
--   こちらは @lvl@ (例 0.99) を 'lyStatLevel' に持たせる。 resolveStats が band 幅に反映する。
statLmLevel :: ColRef -> ColRef -> Double -> Layer
statLmLevel x y lvl = (statLm x y)
  { lyStatLevel = Last (Just lvl) }

-- | Phase 16: stat-in B-spline 平滑 (= ggplot @geom_smooth()@)。 knot 数 n。 曲線のみ (帯なし)。
--   resolveStats が hanalyze で fit し line に展開。 装飾は line に引き継がれる。
statSmooth :: ColRef -> ColRef -> Int -> Layer
statSmooth x y n = mempty
  { lyKind = First (Just MStatSmooth), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyBinCount = Last (Just n) }

-- | Phase 16 B1: 信頼帯つき B-spline 平滑。 'statSmooth' は曲線のみだが、 こちらは
--   'lyStatLevel' を Just にして「帯あり」を signal する。 resolveStats が bs 設計行列の
--   confidenceBand で band+line に展開する。 既定水準は 0.95 (@statSmoothCI x y n@)。
statSmoothCI :: ColRef -> ColRef -> Int -> Layer
statSmoothCI x y n = (statSmooth x y n)
  { lyStatLevel = Last (Just 0.95) }

-- | Phase 16 B3: 多項式回帰 stat (= ggplot @geom_smooth(method="lm", formula=y~poly(x,deg))@)。
--   次数 deg は 'lyBinCount' を流用。 resolveStats が @y ~ poly(x,deg)@ で fit し band+line に展開。
--   信頼帯の水準は 'lyStatLevel' (既定 0.95)。 ★単体では描画されない (renderer は MStatPoly を skip)。
statPoly :: ColRef -> ColRef -> Int -> Layer
statPoly x y deg = mempty
  { lyKind = First (Just MStatPoly), lyEncX = Last (Just x), lyEncY = Last (Just y)
  , lyBinCount = Last (Just deg) }

-- | Phase 16 B3: 残差 vs fitted 診断散布 (= base R @plot(lm)@ #1)。 @y ~ x@ で fit し
--   各点を (fitted, residual) に写した scatter に展開する (回帰診断)。 装飾は scatter に引き継ぐ。
--   ★単体では描画されない (renderer は MStatResid を skip)。 bridge resolveStats が必要。
statResid :: ColRef -> ColRef -> Layer
statResid x y = mempty
  { lyKind = First (Just MStatResid), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | P11: stem / lollipop plot。
stem :: ColRef -> ColRef -> Layer
stem x y = mempty
  { lyKind = First (Just MStem), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | TODO-11 (2026-05-27): area band (= 信頼区間 / 予測帯)。
-- |   x       = 共通 x 軸
-- |   yLow    = 下境界 y
-- |   yHigh   = 上境界 y
-- | Render は PPath fill 1 枚 (= forward x-yLow + backward x-yHigh + close)。
-- | alpha は layer modifier の `alpha` で指定 (= default 0.2)。
band :: ColRef -> ColRef -> ColRef -> Layer
band x yLow yHigh = mempty
  { lyKind = First (Just MBand)
  , lyEncX = Last (Just x)
  , lyEncY = Last (Just yLow)
  , lyEncY2 = Last (Just yHigh)
  }

-- | Phase 52.D2: streamgraph (= 中心化積層 area)。
-- |   x = 共通 x 軸 (連続、 例: 時間)
-- |   y = 各系列の値
-- | 系列分割は color aesthetic で行う (= 'bar' の群分けと同型)。
-- |
-- |   > stream "t" "value" <> colorBy "series"
-- |
-- | 各 x 点で系列を積層し baseline を -(Σy)/2 から開始する (silhouette 中心化)。
-- | wiggle 最小化 (ThemeRiver) は行わない。
stream :: ColRef -> ColRef -> Layer
stream x y = mempty
  { lyKind = First (Just MStream), lyEncX = Last (Just x), lyEncY = Last (Just y) }

-- | P2: violin plot。 ★ Phase 36 B1c: boxplot と同じく **値 1 列**を受ける。 群分けは
--   @<> groupBy "g"@ (色一律) / @<> colorBy "g"@ (群色+凡例) で付ける (ggplot 同型)。
--   群指定なしなら単一 violin。
violin :: ColRef -> Layer
violin v = mempty { lyKind = First (Just MViolin), lyEncY = Last (Just v) }

-- | P3: strip plot。 ★ Phase 36 B1c: 値 1 列 + groupBy/colorBy で群分け。
strip :: ColRef -> Layer
strip v = mempty { lyKind = First (Just MStrip), lyEncY = Last (Just v) }

-- | P3: swarm plot。 ★ Phase 36 B1c: 値 1 列 + groupBy/colorBy で群分け。
swarm :: ColRef -> Layer
swarm v = mempty { lyKind = First (Just MSwarm), lyEncY = Last (Just v) }

-- | P22: raincloud (= violin + box + strip 合成)。 ★ Phase 36 B1c: 値 1 列 + groupBy/colorBy。
-- | Phase 36 D2: mark 直結合成。 @a \<+\> b@ は a を base、 b を重畳 sub-mark とする
--   **単一 Layer** を返す (= 戻り型 Layer 維持ゆえ @raincloud v \<+\> ... \<\> groupBy g@ の
--   ような群修飾が従来どおり効く)。 b 側の overlay も平坦化して取り込む。 render は base +
--   各 overlay を「親の群 (encX)・色 (colorBy)・値 (encY) を継承・自前の kind/nudge/markWidth/side
--   で」 描く。 1D 分布 mark (box/violin/strip/swarm) の重畳を想定 (= raincloud / 自作 composite)。
infixl 7 <+>
(<+>) :: Layer -> Layer -> Layer
a <+> b = a { lyOverlay = lyOverlay a ++ [b { lyOverlay = [] }] ++ lyOverlay b }

-- | P22: raincloud (= 半 violin + box + jitter strip の合成)。 ★ Phase 36 D2: 専用 mark を廃し
--   '<+>' による 3 sub-mark 合成の preset に降格 (= 位置決めは D1 つまみ nudge/markWidth/side に委譲)。
--   戻り型は Layer なので @raincloud v \<\> groupBy g@ / @\<\> colorBy g@ は従来どおり群分けする。
raincloud :: ColRef -> Layer
raincloud v =
      (violin  v <> side SideRight <> nudge 0.15    <> markWidth 0.40)
  <+> (boxplot v               <> nudge 0.00    <> markWidth 0.10)
  <+> (strip   v               <> nudge (-0.25) <> markWidth 0.18)

-- | Phase 36 D3: 合成 Layer (base + overlay sub-mark) の値列レーン (= encY の distinct・base 先頭・
--   'colRefName' で重複除去)。 描画/Layout は各マークの slot を「自 encY が此のレーン列の何番目か」
--   で決める。 同一列なら 1 レーン (= raincloud の重畳)、 複数列なら横並び (= distCols)。
compositeLanes :: Layer -> [ColRef]
compositeLanes ly = foldl add [] [ c | l <- ly : lyOverlay ly, Just c <- [getLast (lyEncY l)] ]
  where add acc c = if any ((== colRefName c) . colRefName) acc then acc else acc ++ [c]

-- | P21: ridge / joyplot。 ★ Phase 36 B1c: 他 distribution mark と統一して **値 1 列**を
-- |   受ける。 群分けは @<> groupBy "g"@ / @<> colorBy "g"@ (= box/violin と同じ)。
-- |   群指定なしは単一 density 風。 ridge は値→x・群→y の向きが要るため、 ridge レイヤを
-- |   含む spec は 'ridgeAutoFlip' で coord_flip を自動適用する (値が x、 群が y に回る)。
-- |   内部表現は violin と同じ encY=値。
ridge :: ColRef -> Layer
ridge v = mempty { lyKind = First (Just MRidge), lyEncY = Last (Just v) }

-- | P14: scatter jitter (= plotArea 比率 0..1)。
jitterX, jitterY :: Double -> Layer
jitterX a = mempty { lyJitterX = Last (Just a) }
jitterY a = mempty { lyJitterY = Last (Just a) }

-- | frontend-settings v0.1 §2.4: histogram の bin 数 (= default 10)。
binCount :: Int -> Layer
binCount n = mempty { lyBinCount = Last (Just n) }

-- | Phase 28: histogram の bin 幅 (= ggplot @geom_histogram(binwidth = w)@)。
--   'binWidth' を指定すると 'binCount' より優先され、 'histBinning' が ggplot 流
--   (boundary = w/2 で bin 原点を定める) の bin 化を行う。
binWidth :: Double -> Layer
binWidth w = mempty { lyBinWidth = Last (Just w) }

-- | histogram の bin 化パラメタ (origin, binW, nBin) を決める単一情報源。
--   render (Render.Basic) と y/x range (Layout.RangeOf) の双方がこれを使い、
--   bin 境界・棒高・軸範囲を一致させる。
--
--   * 'lyBinWidth' 指定時: ggplot @bin_breaks_width@ と同式。 boundary = w/2 とし、
--     origin = boundary + floor((lo - boundary)/w) * w、 nBin = ceil((hi - origin)/w)。
--     これで R4DS の @binwidth=@ と同じ bin 境界・棒高になる。
--   * 未指定時: 従来どおり 'lyBinCount' (既定 30) で [lo,hi] を等分。
--
--   bin i は @[origin + i*binW, origin + (i+1)*binW)@、 値 v の所属は
--   @clamp 0 (nBin-1) (floor ((v - origin)/binW))@。
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

-- | Phase 40: hexbin の六角セル (中心 + 件数 + 6 頂点、 すべてデータ座標)。
data HexCell = HexCell
  { hexCx    :: !Double             -- ^ セル中心 x (データ座標)
  , hexCy    :: !Double             -- ^ セル中心 y
  , hexCount :: !Int                -- ^ セルに入った点数
  , hexVerts :: ![(Double, Double)] -- ^ 6 頂点 (pointy-top、 データ座標)
  } deriving (Show, Eq)

-- | Phase 40: 六角ビニング (d3-hexbin = Carr 1987)。 @bins@ = x 方向セル分割数。
--   (xmin,xmax)/(ymin,ymax) = データ範囲、 @pts@ = (x,y) 点列。 binwidth で正規化した
--   (u,v) 空間で点を六角セルに割当て件数を数え、 中心・6 頂点をデータ座標で返す
--   (= scale パイプラインでそのまま screen へ。 pointy-top)。
--   ★HS/PS で同式・JS Math.round (= @floor (z+0.5)@) を使い byte 一致させる。
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

-- | Phase 40: hexbin layer を解決して六角セルを返す (renderHexbin と count colorbar が共有)。
--   x/y を 'resolveNum' で取り NaN を除いて zip、 bins (既定 30) で 'hexbinCells'。
--   render と凡例で**同じ count 域**を得るために 1 本に集約する。
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

-- | TODO-3a (2026-05-29): histogram の y 軸を密度 (= count / (total * binW))
-- に正規化。 PS Spec.histogramDensity と同等。 SVG export でも動くように HS
-- 側にも実装 (= 旧来 HS は count のみで density mode が機能しなかった)。
histogramDensity :: Bool -> Layer
histogramDensity b = mempty { lyHistDensity = Last (Just b) }

-- | Phase 8 B7: histogram / bar の bin 境界線 (= 各バーの白枠) を表示するか。
-- デフォルトは False (= ggplot 流フラットバー、 枠なし)。 True で bin 区切りが見える。
histBorder :: Bool -> Layer
histBorder b = mempty { lyHistBorder = Last (Just b) }

-- | Phase 28: density 曲線の下を塗りつぶす (= ggplot @geom_density(aes(fill = …))@)。
--   群別 ('color') と 'alpha' を併用すると、 各群を群色 × alpha で塗る (R4DS Ch1 §1.5)。
--   既定 (未指定/False) は ggplot 同様 fill=NA = 線のみ。
densityFill :: Bool -> Layer
densityFill b = mempty { lyDensityFill = Last (Just b) }

-- | Phase 34: マーカーを中抜き (= ggplot @shape="circle open"@ / @geom_point(fill = NA)@)。
--   塗りを透明にし、 点色で輪郭 (stroke) のみ描く。 'size' で輪郭円の直径、
--   'stroke' で線幅 (既定 1pt)。 重畳して「点を輪で囲む」 強調に使う (R4DS Ch9 §9.6)。
hollow :: Layer
hollow = mempty { lyHollow = Last (Just True) }

-- | Phase 36 D1: 分布 mark (box/violin/strip/swarm) の slot 内横 offset。 値は **slot 幅比**
--   (= ggplot @position_nudge@)。 正で右、 負で左。 raincloud の「box を中央・strip を左・雲を右」
--   のような重畳配置を組むのに使う (= 旧 raincloud のハードコード offset を置換)。
nudge :: Double -> Layer
nudge x = mempty { lyNudge = Last (Just x) }

-- | Phase 36 D1: 分布 mark の幅 (= **slot 幅比・占有率**)。 各 mark の既定占有率
--   (box 0.5 / violin 0.7 / strip 0.4 / swarm 0.8) を上書きする。 raincloud では box を細く
--   (= 0.1 等) するのに使う。
markWidth :: Double -> Layer
markWidth w = mempty { lyMarkWidth = Last (Just w) }

-- | Phase 36 D1: violin の片側化 (= 半 violin)。 @violin "v" <> side SideRight@ で右半分のみ。
--   raincloud の「雲」 (= 片側 violin) に使う。 box/strip 等には影響しない。
side :: Side -> Layer
side s = mempty { lySide = Last (Just s) }

-- | Phase 9 B: bar の position adjustment (= ggplot `position`)。
--   群分け (= color/group aesthetic) があるとき 'PosDodge' / 'PosStack' / 'PosFill' で
--   並べ方を選ぶ。 既定 ('PosIdentity') は従来通り単色棒 (color を見ない)。
--
--   > bar "cat" "y" <> colorBy "grp" <> position PosDodge
position :: Position -> Layer
position p = mempty { lyPosition = Last (Just p) }

-- | Phase 30 A3: 固定 shape (= layer 全体に適用・ggplot @shape=@)。 bare=固定。
--   'shapeBy' (列で map) より優先される ('pointShapeAt' 参照)。
shape :: MarkShape -> Layer
shape s = mempty { lyShape = Last (Just s) }

-- | C-6: shape categorical encoding 列。
shapeBy :: ColRef -> Layer
shapeBy c = mempty { lyShapeBy = Last (Just c) }

-- | C-6: cat 名 → MarkShape 1 件追加 (= 複数 entry は <> で合成)。
shapeMapEntry :: Text -> MarkShape -> Layer
shapeMapEntry v s = mempty { lyShapeMap = [ ShapeMapEntry { smeValue = v, smeShape = s } ] }

-- | C-6: size continuous encoding 列。
sizeBy :: ColRef -> Layer
sizeBy c = mempty { lySizeBy = Last (Just c) }

-- | Phase 30 A8: alpha (= 不透明度) を連続値の列で encode する (= ggplot @scale_alpha@・
--   @aes(alpha = col)@)。 列値 min..max を alpha @[0.1, 1.0]@ に線形 map (ggplot 既定 range)。
--   固定 alpha は bare 'alpha' (案2 = bare 固定 / `*By` = map)。
--
-- > scatter "x" "y" <> alphaBy "weight"
alphaBy :: ColRef -> Layer
alphaBy c = mempty { lyAlphaBy = Last (Just c) }

-- | Phase 11 A4-b: 固定 linetype (= ggplot linetype="dashed")。 line 系 mark に適用。
--   例: @line "x" "y" <> linetype LtDashed@
linetype :: LineType -> Layer
linetype lt = mempty { lyLinetype = Last (Just lt) }

-- | Phase 11 A4-b: categorical linetype encoding 列 (= ggplot linetype=factor(g))。
--   line を群ごとに分割し各群へ巡回 LineType ('lineTypeForIndex') を割当。
--   例: @line "x" "y" <> linetypeBy (ColByName "grp")@
linetypeBy :: ColRef -> Layer
linetypeBy c = mempty { lyLinetypeBy = Last (Just c) }

-- | C-step trellis 色一貫性: 全データ cat 出現順を Layer に注入。
colorCats :: [Text] -> Layer
colorCats cs = mempty { lyColorCats = cs }

-- | Phase 28: categorical 水準の既定順 (= ggplot2 の factor 既定 = アルファベット順)。
--   色 / x 軸 / shape の distinct を取るときに使い、 R4DS と凡例・色・並びを一致させる。
--   明示順が要るとき (fct_infreq 等) は 'colorCats' / 'xCatOrder' で上書きする。
orderedCats :: [Text] -> [Text]
orderedCats = Data.List.sort . Data.List.nub

-- | Phase 26 §C-2 #8: 列の平均値を水平線として描画 (= PlotConfig.showMean)。
statMean :: ColRef -> Layer
statMean c = mempty
  { lyKind = First (Just MStatMean), lyEncY = Last (Just c) }

-- | Phase 26 §C-2 #8: 列の中央値を水平線として描画 (= PlotConfig.showMedian)。
statMedian :: ColRef -> Layer
statMedian c = mempty
  { lyKind = First (Just MStatMedian), lyEncY = Last (Just c) }

-- | Phase 26 §C-2 #13: parallel coordinates plot。 各 col が縦軸となり、
-- 各 row を全軸 cross する折線で表現。 hover で row 強調 (= 後追い)。
parallelCoords :: [ColRef] -> Layer
parallelCoords cols = mempty
  { lyKind = First (Just MParallel), lyHover = cols }

-- | Phase 26 §E-6: HBM ModelGraph DAG を描画する layer。
-- 内部 builder で使う直接 constructor。 ユーザは 'Graphics.Hgg.DAG.dagPlot'
-- (= Graph a + ~> 経由) を使う方が良い。
dagFromLists :: [DAGNode] -> [DAGEdge] -> DAGLayoutAlgorithm -> Layer
dagFromLists nodes edges algo = mempty
  { lyKind = First (Just MDAG)
  , lyDAG  = Last (Just (DAGSpec nodes edges algo [])) }

-- | dsPlates も指定する版。
dagFromListsWithPlates
  :: [DAGNode] -> [DAGEdge] -> DAGLayoutAlgorithm -> [DAGPlate] -> Layer
dagFromListsWithPlates nodes edges algo plates = mempty
  { lyKind = First (Just MDAG)
  , lyDAG  = Last (Just (DAGSpec nodes edges algo plates)) }

-- | DAGNode constructor (= kind + 分布名なし)。
dagNode :: Text -> Text -> DAGNodeKind -> Double -> Double -> DAGNode
dagNode i l k x y = DAGNode i l k Nothing x y

-- | 分布名付き DAGNode constructor (= PyMC 風 "name ~ dist" 表示用)。
dagNodeDist :: Text -> Text -> DAGNodeKind -> Text -> Double -> Double -> DAGNode
dagNodeDist i l k dist x y = DAGNode i l k (Just dist) x y

-- | DAGEdge constructor。
dagEdge :: Text -> Text -> DAGEdge
dagEdge f t = DAGEdge f t Nothing Nothing

-- | 互換用 shortcut: 既存 demo / test 用 (= NodeLatent + LayoutManual)。
-- 新規 API は Graphics.Hgg.DAG.dagPlot を使う。
dag :: [DAGNode] -> [DAGEdge] -> Layer
dag nodes edges = dagFromLists nodes edges LayoutManual

-- | Phase 26 §E-1: MCMC trace plot (single chain)。 iteration vs parameter
-- 値の line。 mark kind は MTrace (= alias for MLine、 frontend で区別可能)。
trace :: ColRef -> ColRef -> Layer
trace iterCol valCol = mempty
  { lyKind = First (Just MTrace)
  , lyEncX = Last (Just iterCol)
  , lyEncY = Last (Just valCol)
  }

-- | Phase 26 §E-1: multi-chain trace。 chain 列で色分け、 connect group も
-- chain 列 (= chain 内で連結、 chain 跨ぎ無し)。 PlotConfig.StreamingTracePlot 等価。
traceLines :: ColRef -> ColRef -> ColRef -> Layer
traceLines iterCol valCol chainCol =
  trace iterCol valCol
    <> colorBy chainCol
    <> connectGroup chainCol
    <> stroke 1.0

