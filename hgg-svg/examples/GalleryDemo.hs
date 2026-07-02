-- | Gallery demo: hgg の機能カタログ用 SVG 出力 (= design/gallery/ へ吐く)。
--
-- @
-- cabal run gallery-demo
-- @
--
-- → design/gallery/{basic,distribution,statistical,decoration,axes,theme,doe}/ 配下に
-- 各 chart 種別ごとに SVG を生成。 'design/gallery.md' でこれらを参照して表示。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hgg.Plot.Backend.SVG (saveSVG, saveSVGWith)
import           Hgg.Plot.DAG         (layoutHierarchicalFull)
import           Hgg.Plot.Render.Special (bakeDAGRoutesInSpec)
import           Hgg.Plot.Unit         (px, (*~))
import           Hgg.Plot.Easy
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Vector              as V
import qualified System.Directory
import           System.Directory         (createDirectoryIfMissing)

main :: IO ()
main = do
  let dirs = ["basic", "distribution", "statistical", "decoration",
              "axes", "theme", "palette", "doe", "coord", "scale"]
  mapM_ (\d -> createDirectoryIfMissing True ("design/gallery/" ++ d)) dirs

  putStrLn "Generating gallery SVGs..."

  -- ===========================================================================
  -- BASIC (= 1 列 chart)
  -- ===========================================================================
  let xs   = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
      ys   = map (\x -> x * x) xs
      vals = [3.0, 4.0, 4.5, 5.0, 5.0, 5.2, 5.5, 6.0, 6.5, 7.0, 8.0, 12.0] :: [Double]

  -- scatter
  emit "basic/scatter.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> alpha 0.85 <> size 5)
    <> title  "scatter: y = x²"
    <> xLabel "x" <> yLabel "y"

  -- line
  let xs2 = [0, 0.5 .. 6.28] :: [Double]
      ys2 = map sin xs2
  emit "basic/line.svg" $
       purePlot
    <> layer (line (inline xs2) (inline ys2) <> color (fromHex "#2563eb") <> stroke 2)
    <> title "line: y = sin(x)"

  -- bar
  emit "basic/bar.svg" $
       purePlot
    <> layer (bar (inlineCat (["A", "B", "C", "D", "E"] :: [Text]))
                   (inline ([8.0, 5.0, 12.0, 6.5, 3.0] :: [Double])))
    <> title "bar (categorical)"

  -- Phase 9 B: position adjustment (dodge / stack / fill)。 long-form データ
  --   (= 各 row が (cat, group, value)) を color で群分けし、 position で並べ方を選ぶ。
  let posCats  = inlineCat (concatMap (replicate 3) (["A","B","C"] :: [Text]))
      posGrp   = inlineCat (concat (replicate 3 (["x","y","z"] :: [Text])))
      posVals  = inline ([3.0,5.0,2.0, 4.0,1.0,6.0, 2.0,3.0,4.0] :: [Double])
  emit "basic/bar-dodge.svg" $
       purePlot
    <> layer (bar posCats posVals <> colorBy posGrp <> position PosDodge)
    <> title "bar (dodge = 横並び)"
  emit "basic/bar-stack.svg" $
       purePlot
    <> layer (bar posCats posVals <> colorBy posGrp <> position PosStack)
    <> title "bar (stack = 積み上げ)"
  emit "basic/bar-fill.svg" $
       purePlot
    <> layer (bar posCats posVals <> colorBy posGrp <> position PosFill)
    <> title "bar (fill = 100% 積み上げ)"

  -- Phase 10: coord_flip (= 横棒)。 軸ラベルは水平のまま・bar は横に伸びる。
  emit "coord/bar-flip.svg" $
       purePlot
    <> layer (bar (inlineCat (["A", "B", "C", "D", "E"] :: [Text]))
                   (inline ([8.0, 5.0, 12.0, 6.5, 3.0] :: [Double])))
    <> coordFlip
    <> title "bar + coord_flip (横棒)"

  -- Phase 11 A7-a: coord_cartesian(xlim,ylim) = データを落とさない zoom。
  --   全 11 点 (x=0..10, y=x^2) のうち x∈[2,6] の窓に絞り、 窓外の点は panel に clip
  --   される (= データは残るが描画は枠内のみ)。 axisRange と違いデータを切らない。
  emit "coord/cartesian-zoom.svg" $
       purePlot
    <> layer (scatter (inline ([0,1,2,3,4,5,6,7,8,9,10] :: [Double]))
                      (inline ([0,1,4,9,16,25,36,49,64,81,100] :: [Double])))
    <> coordCartesian 2 6 0 40
    <> title "coord_cartesian (zoom x∈[2,6], y∈[0,40])"

  -- Phase 11 A7-c: coord_polar (theta="x")。 x を角度・y を半径に写す (= radar / rose)。
  --   12 方位の値を閉じた折線で結ぶ (先頭を末尾に追加して 1 周)。
  emit "coord/polar-line.svg" $
       purePlot
    <> layer (line (inline ([0,1,2,3,4,5,6,7,8,9,10,11,12] :: [Double]))
                   (inline ([6,8,5,9,7,10,6,8,5,9,7,10,6] :: [Double]))
              <> stroke 2)
    <> layer (scatter (inline ([0,1,2,3,4,5,6,7,8,9,10,11] :: [Double]))
                      (inline ([6,8,5,9,7,10,6,8,5,9,7,10] :: [Double])) <> size 4)
    <> coordPolar
    <> title "coord_polar (theta=x、 radar)"

  -- Phase 11 A7-c: coord_polar + bar = rose / 円形棒グラフ (扇形 wedge)。
  emit "coord/polar-bar.svg" $
       purePlot
    <> layer (bar (inlineCat (["Mon","Tue","Wed","Thu","Fri","Sat","Sun"] :: [Text]))
                  (inline ([4,7,5,9,6,11,8] :: [Double])))
    <> coordPolar
    <> title "coord_polar + bar (rose / 円形棒)"

  -- Phase 11 A4-a: scale_x_reverse / scale_y_reverse (軸反転 = range 入替)。
  --   tick/grid/glyph は scaleApply 経由で自動追従 (renderer 無変更)。
  emit "scale/reverse-x.svg" $
       purePlot
    <> layer (scatter (inline ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]))
                       (inline ([1.0, 4.0, 9.0, 16.0, 25.0] :: [Double])))
    <> reverseX
    <> title "scale_x_reverse (X 軸反転)"
  emit "scale/reverse-y.svg" $
       purePlot
    <> layer (scatter (inline ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]))
                       (inline ([1.0, 4.0, 9.0, 16.0, 25.0] :: [Double])))
    <> reverseY
    <> title "scale_y_reverse (Y 軸反転)"

  -- Phase 11 A4-b: linetype (固定) と linetypeBy (categorical 群分け = 巡回 dash)。
  --   line 系 mark の dash は LineStyle.lsDash 経由 (SVG stroke-dasharray / Canvas setLineDash)。
  emit "scale/linetype.svg" $
       purePlot
    <> layer (line (inline ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double]))
                   (inline ([1.0, 3.0, 2.0, 5.0, 4.0] :: [Double]))
              <> stroke 2 <> linetype LtDashed)
    <> title "linetype = dashed (固定)"
  let ltX = inline (concat (replicate 2 ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double])))
      ltY = inline ([1.0, 3.0, 2.0, 5.0, 4.0, 2.0, 1.0, 4.0, 3.0, 6.0] :: [Double])
      ltG = inlineCat (concatMap (replicate 5) (["A", "B"] :: [Text]))
  emit "scale/linetype-by.svg" $
       purePlot
    <> layer (line ltX ltY <> stroke 2 <> linetypeBy ltG)
    <> title "linetype = factor(g) (A=solid, B=dashed)"

  -- Phase 11 A4-d: 明示 breaks / labels (= ggplot scale_*_continuous(breaks=,labels=))。
  --   X は break+label 対 (0→low, 50→mid, 100→high)、 Y は break のみ (自動 format)。
  emit "scale/breaks-labels.svg" $
       purePlot
    <> layer (scatter (inline ([0.0, 25.0, 50.0, 75.0, 100.0] :: [Double]))
                      (inline ([10.0, 40.0, 55.0, 70.0, 90.0] :: [Double])))
    <> xAxis (axisBreaksLabeled [(0, "low"), (50, "mid"), (100, "high")])
    <> yAxis (axisBreaksAt [0, 25, 50, 75, 100])
    <> title "明示 breaks/labels (X=ラベル, Y=値刻み)"

  -- Phase 11 A4-e: 色/サイズ scale 拡充 (manual 辞書 / 発散 gradient2 / size range)。
  let aeX = inline (concat (replicate 2 ([1.0, 2.0, 3.0, 4.0] :: [Double])))
      aeY = inline ([2.0, 3.0, 1.0, 4.0, 3.0, 1.0, 4.0, 2.0] :: [Double])
      aeG = inlineCat (concatMap (replicate 4) (["alpha", "beta"] :: [Text]))
  emit "scale/color-manual.svg" $
       purePlot
    <> layer (scatter aeX aeY <> colorBy aeG <> size 6)
    <> scaleColorManual [("alpha", "#1B9E77"), ("beta", "#D95F02")]
    <> legend
    <> title "scale_color_manual (alpha→緑, beta→橙)"
  -- gradient2: z = -3..3、 midpoint 0 を白に (発散)。
  let gz = inline ([-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0] :: [Double])
      gx = inline ([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0] :: [Double])
  emit "scale/color-gradient2.svg" $
       purePlot
    <> layer (scatter gx gx <> colorContinuousBy gz <> size 8)
    <> scaleColorGradient2 "#2166AC" "#F7F7F7" "#B2182B" 0.0
    <> legend
    <> title "scale_color_gradient2 (midpoint 0 = 白)"
  -- size: 値に応じた半径 (range 4..20)。
  let sx = inline ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double])
      ssz = inline ([1.0, 5.0, 10.0, 20.0, 40.0] :: [Double])
  emit "scale/size.svg" $
       purePlot
    <> layer (scatter sx sx <> sizeBy ssz)
    <> scaleSize 4 20
    <> title "scale_size (range 4..20)"
  -- Phase 30 A8: alphaBy = 連続 alpha encoding (= ggplot scale_alpha)。
  -- 列値 min..max を alpha [0.1, 1.0] に map (薄→濃)。size 固定で alpha のみ変化。
  emit "scale/alpha.svg" $
       purePlot
    <> layer (scatter sx sx <> alphaBy ssz <> size 12)
    <> title "scale_alpha (alphaBy・薄→濃)"

  -- Phase 11 A6: geom_text / geom_label (データ駆動ラベル)。
  let gtX = inline ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double])
      gtY = inline ([2.0, 4.0, 3.0, 5.0, 4.5] :: [Double])
      gtL = inlineCat (["alpha", "beta", "gamma", "delta", "epsilon"] :: [Text])
  emit "basic/geom-text.svg" $
       purePlot
    <> layer (scatter gtX gtY <> size 4)
    <> layer (text gtX gtY gtL <> size 12)
    <> title "geom_text (各点にラベル)"
    <> xLabel "x" <> yLabel "y"
  emit "basic/geom-label.svg" $
       purePlot
    <> layer (label gtX gtY gtL <> size 12)
    <> title "geom_label (背景付きラベル)"
    <> xLabel "x" <> yLabel "y"

  -- Phase 11 A6-2: Q-Q plot (= ggplot geom_qq)。 正規サンプル N=120 を
  -- 理論正規分位点に対してプロット (= 正規性の視覚診断、 直線なら正規)。
  let qqVals = gaussian 7 0.0 1.0 120 :: [Double]
  emit "basic/qq.svg" $
       purePlot
    <> layer (qq (inline qqVals) <> size 5)
    <> title "Q-Q plot (正規分位点 vs サンプル)"
    <> xLabel "理論分位点" <> yLabel "標本分位点"

  -- Phase 11 A6-3: heatmap (= ggplot geom_tile)。 x/y カテゴリ grid を value の
  -- 連続色 (Viridis) で塗る。 long-form (各 row = (x, y, value))。
  let hmX = inlineCat (concatMap (replicate 3) (["A", "B", "C"] :: [Text]))
      hmY = inlineCat (concat (replicate 3 (["P", "Q", "R"] :: [Text])))
      hmV = inline ([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0] :: [Double])
  emit "basic/heatmap.svg" $
       purePlot
    <> layer (heatmap hmX hmY hmV)
    <> title "heatmap (3×3 grid、 Viridis)"
    <> xLabel "x" <> yLabel "y"

  -- contour (= 等高線図、 marching squares)。 連続 x/y/z を正則格子に再標本化し
  -- z を等分した各レベルの等値線を描く。 z = exp(-((x-3)²+(y-3)²)/4) 的な釣鐘 (中心が高い)。
  let ctGrid = [ (xi, yi) | xi <- [0.0, 0.5 .. 6.0], yi <- [0.0, 0.5 .. 6.0] ]
      ctX = inline (map fst ctGrid)
      ctY = inline (map snd ctGrid)
      ctZ = inline [ exp (-(((x - 3) ** 2) + ((y - 3) ** 2)) / 4) | (x, y) <- ctGrid ]
  emit "basic/contour.svg" $
       purePlot
    <> layer (contour ctX ctY ctZ)
    <> title "contour (等高線、 中心が高い釣鐘)"
    <> xLabel "x" <> yLabel "y"

  -- bin2d (= ggplot geom_bin2d、 binned heatmap)。 contour と同データを塗りで。
  emit "basic/bin2d.svg" $
       purePlot
    <> layer (bin2d ctX ctY ctZ)
    <> title "bin2d (binned heatmap、 中心が高い釣鐘)"
    <> xLabel "x" <> yLabel "y"

  -- Phase 11 A6-4: ECDF (= ggplot stat_ecdf)。 正規サンプルの経験累積分布 (階段)。
  let ecdfVals = gaussian 13 0.0 1.0 80 :: [Double]
  emit "basic/ecdf.svg" $
       purePlot
    <> layer (ecdf (inline ecdfVals))
    <> title "ECDF (経験累積分布、 N=80)"
    <> xLabel "x" <> yLabel "F(x)"

  -- Phase 11 A6-4b: 区間 geom (linerange / pointrange / crossbar)。 x/y/err。
  let rbX = inline ([1.0, 2.0, 3.0, 4.0, 5.0] :: [Double])
      rbY = inline ([3.0, 4.5, 4.0, 5.5, 5.0] :: [Double])
      rbE = inline ([0.5, 0.8, 0.4, 0.6, 0.7] :: [Double])
  emit "basic/linerange.svg" $
       purePlot
    <> layer (lineRange rbX rbY rbE)
    <> title "linerange (y±err の縦線)" <> xLabel "x" <> yLabel "y"
  emit "basic/pointrange.svg" $
       purePlot
    <> layer (pointRange rbX rbY rbE)
    <> title "pointrange (縦線 + 中心点)" <> xLabel "x" <> yLabel "y"
  emit "basic/crossbar.svg" $
       purePlot
    <> layer (crossbar rbX rbY rbE)
    <> title "crossbar (幅付き箱 + 中央線)" <> xLabel "x" <> yLabel "y"

  -- Phase 11 A6-4c: stat_function (= ggplot stat_function)。 関数を構成時にサンプルして
  -- inline line に焼き込む。 ここでは sin を [0, 2π] で 100 点。
  emit "basic/stat-function.svg" $
       purePlot
    <> layer (statFunction sin 0.0 (2 * pi) 100 <> color (fromHex "#2563eb") <> stroke 2)
    <> title "stat_function (sin、 [0,2π] を 100 点)"
    <> xLabel "x" <> yLabel "sin x"

  -- histogram
  -- Phase 8 B8: N=200 正規分布、 bin 数は既定 (= ggplot と同じ 30)
  let histVals = gaussian 41 5.0 1.5 200 :: [Double]
  emit "basic/histogram.svg" $
       purePlot
    <> layer (histogram (inline histVals) <> alpha 0.8)
    <> title "histogram (N=200、 既定 30 bins)"

  -- box
  -- 単一群 box
  emit "basic/box.svg" $
       purePlot
    <> layer (boxplot (inline vals))
    <> title "box plot (単一群、 Q1/median/Q3 + whisker)"

  -- density
  let denseVals = take 200 $ cycle
        [3.0, 4.0, 4.5, 5.0, 5.0, 5.2, 5.5, 6.0, 6.5, 7.0, 8.0]
  emit "basic/density.svg" $
       purePlot
    <> layer (density (inline denseVals) <> color (fromHex "#16a34a"))
    <> title "density (KDE、 Silverman bw)"

  -- pie
  emit "basic/pie.svg" $
       purePlot
    <> layer (pie (inlineCat (["Eng", "Bio", "Math", "CS"] :: [Text]))
                   (inline ([35.0, 22.0, 18.0, 25.0] :: [Double])))
    <> title "pie (proportion)"

  -- waterfall
  emit "basic/waterfall.svg" $
       purePlot
    <> layer (waterfall (inlineCat (["Start", "+A", "+B", "-C", "End"] :: [Text]))
                         (inline ([100.0, 30.0, 20.0, -15.0, 0.0] :: [Double])))
    <> title "waterfall"

  -- step
  emit "basic/step.svg" $
       purePlot
    <> layer (step (inline xs) (inline ys) <> color (fromHex "#dc2626"))
    <> title "step plot"

  -- histogram wide-form (Phase 6 A10、 P1 解消) — 3 列を半透明で重ね
  -- Phase 8 B7: 各列 N=70 の正規分布 (平均をずらした 3 群)、 bin 境界は全列共通
  let h1 = gaussian 71 5.0 1.2 70 :: [Double]
      h2 = gaussian 83 6.5 1.0 70
      h3 = gaussian 97 4.0 1.4 70
  emit "basic/histogram-wide.svg" $
       histogramWide [inline h1, inline h2, inline h3]
    <> title "histogramWide [c1, c2, c3] (= 全列共通 bin、 N=70 ×3)"

  -- stem
  let stemX = [1, 2, 3, 4, 5, 6, 7, 8] :: [Double]
      stemY = [2.0, 4.5, 1.0, 5.5, 3.0, 6.0, 2.5, 4.0]
  emit "basic/stem.svg" $
       purePlot
    <> layer (stem (inline stemX) (inline stemY) <> color (fromHex "#7c3aed"))
    <> title "stem / lollipop"

  -- ===========================================================================
  -- DISTRIBUTION
  -- ===========================================================================

  let groups   = inlineCat (["A", "A", "A", "B", "B", "B", "C", "C", "C"] :: [Text])
      gvalues  = inline ([1.0, 2.5, 3.0, 4.0, 4.5, 5.5, 6.0, 7.5, 8.0] :: [Double])
      -- Phase 8 B2: 分布系の共用群データ (4 群 × N=90、 正規分布)
      (grpLabels, grpVals) = groupedDemo
      grpCat = inlineCat grpLabels
      grpNum = inline grpVals

  -- Phase 8 B3: violin も共用群データ (4 群 × N=90) で
  emit "distribution/violin.svg" $
       purePlot
    <> layer (violin grpNum <> groupBy grpCat)
    <> title "violin (4 群 × N=90)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 8 B4: strip も共用群データ (4 群 × N=90)、 横 jitter で散らす
  emit "distribution/strip.svg" $
       purePlot
    <> layer (strip grpNum <> groupBy grpCat <> alpha 0.6 <> size 4)
    <> title "strip plot (4 群 × N=90、 jitter)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 8 B5: swarm も共用群データ (4 群 × N=90)、 beeswarm 衝突回避
  emit "distribution/swarm.svg" $
       purePlot
    <> layer (swarm grpNum <> groupBy grpCat <> alpha 0.85 <> size 5)
    <> title "swarm (4 群 × N=90、 beeswarm)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 8 B2: raincloud は群ごと N=90 の正規分布データで (= 群内分布を見せる図)
  emit "distribution/raincloud.svg" $
       purePlot
    <> layer (raincloud grpNum <> groupBy grpCat)
    <> title "raincloud (4 群 × N=90、 jitter + box + half-violin)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 8 B6: ridge は (値, 群) の順。 群ごとに分布形が違うデータ (= joyplot は
  -- 群間の分布形状比較が主眼) で 4 群 × N=90。 引数順は ridge valCol groupCol。
  emit "distribution/ridge.svg" $
       purePlot
    <> layer (ridge grpNum <> groupBy grpCat)
    <> title "ridge / joyplot (4 群 × N=90)"
    <> xLabel "weight gain (g/d)"

  -- Phase 8 B9: 複数群 box (= boxplot valsCol <> groupBy groupCol)、 共用群データ 4 群 × N=90
  emit "distribution/box-grouped.svg" $
       purePlot
    <> layer (boxplot grpNum <> groupBy grpCat)
    <> title "box plot (4 群、 群別 Tukey box)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 10: box + coord_flip (= 横向き box)。 群が縦に並び value が横に伸びる。
  emit "coord/box-flip.svg" $
       purePlot
    <> layer (boxplot grpNum <> groupBy grpCat)
    <> coordFlip
    <> title "box plot + coord_flip (横向き)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 10 A5 ②: distribution 族の flip (violin/strip/swarm)。 自前軸 (cat ラベル/値 tick) が
  -- flip 配置に追従するか確認 (box-flip と同パターン)。
  emit "coord/violin-flip.svg" $
       purePlot
    <> layer (violin grpNum <> groupBy grpCat)
    <> coordFlip
    <> title "violin + coord_flip (横向き)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  emit "coord/strip-flip.svg" $
       purePlot
    <> layer (strip grpNum <> groupBy grpCat <> alpha 0.6 <> size 4)
    <> coordFlip
    <> title "strip + coord_flip (横向き)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  emit "coord/swarm-flip.svg" $
       purePlot
    <> layer (swarm grpNum <> groupBy grpCat <> alpha 0.85 <> size 5)
    <> coordFlip
    <> title "swarm + coord_flip (横向き)"
    <> xLabel "genotype" <> yLabel "weight gain (g/d)"

  -- Phase 10 A5 ②: waterfall の flip (standalone・自前 baseline/y tick/cat ラベル)。
  emit "coord/waterfall-flip.svg" $
       purePlot
    <> layer (waterfall (inlineCat (["Start", "+A", "+B", "-C", "End"] :: [Text]))
                         (inline ([100.0, 30.0, 20.0, -15.0, 0.0] :: [Double])))
    <> coordFlip
    <> title "waterfall + coord_flip (横向き)"

  -- ===========================================================================
  -- STATISTICAL
  -- ===========================================================================

  -- regression line + CI: plot 側では fit しないので OLS 当てはめ値 yhat を作り
  -- band(信頼帯) + line で重ねる (自動 fit は analyze の statLm)。
  let rxs = [0, 1 .. 9] :: [Double]
      rys = map (\x -> 2 * x + 1 + sin x) rxs
      rN   = fromIntegral (length rxs)
      rA   = (rN * sum (zipWith (*) rxs rys) - sum rxs * sum rys)
               / (rN * sum (map (^ (2 :: Int)) rxs) - sum rxs ^ (2 :: Int))
      rB   = (sum rys - rA * sum rxs) / rN
      yhat = [ rA * x + rB | x <- rxs ]
      rlo  = map (subtract 0.6) yhat
      rhi  = map (+ 0.6) yhat
  emit "statistical/regression-ci.svg" $
       purePlot
    <> layer (scatter (inline rxs) (inline rys) <> alpha 0.85 <> size 5)
    <> layer (band (inline rxs) (inline rlo) (inline rhi) <> color (fromHex "#dc2626") <> alpha 0.2)
    <> layer (line (inline rxs) (inline yhat) <> color (fromHex "#dc2626"))
    <> title "regression line + CI band"
    <> xLabel "x" <> yLabel "y"

  -- stat line (= 値系列に対する平均/中央値の水平基準線。 ggplot geom_hline 相当、
  -- DoE の grand-mean 線と同じ用法)。 旧版は histogram (count 軸) の上に値の水平線を
  -- 重ねており、 y 軸が count と値の混在で意味的に破綻 (= findings「y-range 微妙」) して
  -- いた。 statMean/statMedian は y=値 の水平線なので、 値そのものを y に取る系列
  -- (scatter + connect 折れ線) に重ねるのが正しい (Phase 8 B17)。
  let statIdx = map fromIntegral [1 .. length vals] :: [Double]
  emit "statistical/stat-line.svg" $
       purePlot
    <> layer (scatter (inline statIdx) (inline vals) <> connect <> alpha 0.85 <> size 5)
    <> layer (statMean (inline vals)   <> color (fromHex "#dc2626") <> stroke 2)
    <> layer (statMedian (inline vals) <> color (fromHex "#2563eb") <> stroke 2)
    <> title "series + statMean (red) + statMedian (blue)"
    <> xLabel "index" <> yLabel "value"

  -- Forest plot (Phase 6 A2)
  let studies :: [Text]
      studies = ["Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Pooled"]
      ests :: [Double]
      ests    = [0.3, -0.2, 0.5, 0.1, 0.4, 0.22]
      errs :: [Double]
      errs    = [0.2, 0.15, 0.18, 0.25, 0.13, 0.08]
  emit "statistical/forest.svg" $
       purePlot
    <> layer (forest (inlineCat studies) (inline ests) (inline errs)
               <> color (fromHex "#377EB8") <> size 7)
    <> title "Forest plot (= meta-analysis、 中央 null line + horizontal CI)"
    <> xLabel "effect size" <> yLabel "study"

  -- Phase 10 A5 ②: forest の flip (standalone・自前 x tick / 群ラベル左 の flip 辺入替確認)。
  emit "coord/forest-flip.svg" $
       purePlot
    <> layer (forest (inlineCat studies) (inline ests) (inline errs)
               <> color (fromHex "#377EB8") <> size 7)
    <> coordFlip
    <> title "Forest plot + coord_flip"
    <> xLabel "effect size" <> yLabel "study"

  -- Streamgraph (Phase 52.D2): 中心化積層 area。 color aes で 3 系列に分割し、
  -- 各 x 点で系列を積層、 baseline を -(Σy)/2 から (silhouette 中心化) で描く。
  let streamT :: [Double]
      streamT = concat (replicate 3 [0, 1, 2, 3, 4, 5])
      streamV :: [Double]
      streamV = [ 1, 2, 4, 3, 2, 1     -- 系列 A
                , 2, 3, 3, 4, 5, 4     -- 系列 B
                , 1, 1, 2, 2, 3, 5 ]   -- 系列 C
      streamG :: [Text]
      streamG = concat [ replicate 6 "A", replicate 6 "B", replicate 6 "C" ]
  emit "statistical/streamgraph.svg" $
       purePlot
    <> layer (stream (inline streamT) (inline streamV)
               <> colorBy (inlineCat streamG) <> alpha 0.85)
    <> title "Streamgraph (= 中心化積層 area、 ThemeRiver 風、 color で系列分割)"
    <> xLabel "time" <> yLabel "value (centered)"

  -- Funnel plot (Phase 6 A3)
  let funnelEff :: [Double]
      funnelEff = [0.3, 0.25, 0.4, 0.1, -0.2, 0.5, 0.35, 0.15, 0.45, 0.05, 0.6, -0.1]
      funnelSE :: [Double]
      funnelSE  = [0.1, 0.15, 0.2, 0.18, 0.22, 0.25, 0.12, 0.28, 0.16, 0.3, 0.08, 0.35]
  emit "statistical/funnel.svg" $
       purePlot
    <> layer (funnel (inline funnelEff) (inline funnelSE)
               <> color (fromHex "#377EB8") <> size 6)
    <> title "Funnel plot (= publication bias、 ±1.96 SE envelope)"
    <> xLabel "effect size" <> yLabel "SE"

  -- autocorrelation (Phase 6 A4、 P19 解消)
  -- AR(1) process: x_{t+1} = 0.7 * x_t + ε_t
  let ar1Vals :: [Double]
      ar1Vals = take 500 $ iterate (\v -> 0.7 * v + 0.3 * sin (v * 13.0)) 0.5
  emit "statistical/autocorr.svg" $
       purePlot
    <> layer (autocorr (inline ar1Vals) <> autocorrMaxLag 30
               <> color (fromHex "#377EB8"))
    <> title "autocorr (= MCMC AR(1)、 max lag 30、 ±1.96/√N band)"
    <> xLabel "lag" <> yLabel "r(τ)"

  -- ESS (Phase 8 B13): パラメータごとの計算済み ESS 値を棒に (= ggplot/bayesplot 流、
  -- 計算は統計ライブラリの責務、 plot は値を描くだけ)。 閾値 100/400 で色分け。
  let essParams :: [Text]
      essParams = ["alpha", "beta", "sigma", "mu", "tau", "theta"]
      essValues :: [Double]
      essValues = [820.0, 640.0, 95.0, 410.0, 280.0, 55.0]  -- sigma/theta が低い (= 要注意)
  emit "statistical/ess.svg" $
       purePlot
    <> layer (ess (inlineCat essParams) (inline essValues))
    <> title "ESS (= parameter ごとの有効サンプルサイズ、 閾値 100/400)"
    <> xLabel "parameter" <> yLabel "ESS"
    <> title "ESS (= 4 chain、 Geyer initial positive sequence)"
    <> xLabel "chain" <> yLabel "ESS"

  -- MCMC trace
  let mcmcSteps = [0, 1 .. 199] :: [Double]
      mcmcVals = map (\i -> sin (i * 0.05) + i * 0.001) mcmcSteps
  emit "statistical/trace.svg" $
       purePlot
    <> layer (trace (inline mcmcSteps) (inline mcmcVals) <> stroke 1.5)
    <> title "MCMC trace plot"
    <> xLabel "step" <> yLabel "θ"

  -- pairs plot (Phase 8 B16): N=50、 列間に相関 (height=正、 weight=負) + 名前付き列
  let p1 = gaussian 17 5.0 1.5 80 :: [Double]   -- N=80 で単峰が安定
      p2 = zipWith (\a e -> 0.8 * a + e) p1 (gaussian 29 1.0 0.6 80)  -- p1 と正相関
      p3 = zipWith (\a e -> negate 0.6 * a + e) p1 (gaussian 43 8.0 0.8 80)  -- p1 と負相関
      p4 = gaussian 53 5.0 1.5 80    -- 独立
      p5 = zipWith (\a e -> 0.5 * a + e) p2 (gaussian 59 2.0 0.7 80)
      pairsResolver k = case k of
        "age"    -> Just (NumData (V.fromList p1))
        "height" -> Just (NumData (V.fromList p2))
        "weight" -> Just (NumData (V.fromList p3))
        "bmi"    -> Just (NumData (V.fromList p4))
        "score"  -> Just (NumData (V.fromList p5))
        _        -> Nothing
  emitR "statistical/pairs.svg" pairsResolver
    (purePlot
       <> pairs ["age", "height", "weight", "bmi", "score"]
       <> title "pairs plot (5 変数 × N=80、 相関あり)")

  -- ===========================================================================
  -- DECORATION
  -- ===========================================================================

  -- facet (= Resolver 経由で column 解決が必要、 inline では不可なので別 demo)
  let facetResolver facetN = case facetN of
        "x" -> Just (NumData (V.fromList [1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]))
        "y" -> Just (NumData (V.fromList [1, 4, 9, 16, 2, 5, 8, 12, 3, 6, 9, 15]))
        "g" -> Just (TxtData (V.fromList ["A","A","A","A","B","B","B","B","C","C","C","C"]))
        _   -> Nothing
  emitR "decoration/facet.svg" facetResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
    <> facet "g"
    <> title "facet (= Trellis、 3 panel)"

  -- Phase 10 A5: facet + coord_flip。 各 panel が vsCoord を継承し、 flip scale も
  -- panelArea に retarget されて panel が重ならないことを確認 (罠5 = flip scale 伝播)。
  emitR "coord/facet-flip.svg" facetResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
    <> facet "g"
    <> coordFlip
    <> title "facet + coord_flip (3 panel・各 panel flip)"

  -- Phase 8 C G7: facet_wrap 複数行 (ncol=3 → 5 群を 3 列 2 行に折り返し)。
  -- 最下行のみ x 軸・左端列のみ y 軸 (ggplot facet_wrap の内側軸 drop)。
  let facetWrapResolver facetN = case facetN of
        "x" -> Just (NumData (V.fromList (concat (replicate 5 [1,2,3,4]))))
        "y" -> Just (NumData (V.fromList
                 [1,4,9,16, 2,5,8,12, 3,6,9,15, 2,3,7,10, 4,8,11,14]))
        "g" -> Just (TxtData (V.fromList (concatMap (replicate 4)
                 ["A","B","C","D","E"])))
        _   -> Nothing
  emitR "decoration/facet-wrap.svg" facetWrapResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
    <> facetWrap "g" 3
    <> title "facet_wrap (ncol=3、 5 panel → 2 行)"

  -- Phase 11 A7-b: facet free scales (= ggplot facet_wrap(scales="free"))。
  --   群ごとに y のスケールが大きく違う (A: 0..16, B: 0..1200, C: 0..0.3)。 free だと
  --   各 panel が自分のデータ範囲で軸を持ち、 全 panel に x/y 軸が出る。
  let facetFreeResolver facetN = case facetN of
        "x" -> Just (NumData (V.fromList [1,2,3,4, 1,2,3,4, 1,2,3,4]))
        "y" -> Just (NumData (V.fromList
                 [1,4,9,16,  300,600,900,1200,  0.05,0.1,0.2,0.3]))
        "g" -> Just (TxtData (V.fromList (concatMap (replicate 4) ["A","B","C"])))
        _   -> Nothing
  emitR "decoration/facet-free.svg" facetFreeResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
    <> facet "g"
    <> facetScales FacetFree
    <> title "facet free scales (各 panel 独立 y 軸)"

  -- Phase 8 C G7 part-b: facet_grid(row ~ col) 2 変数 cross (2 行 × 3 列)。
  -- 上 strip = col 名 (各列頭)、 右 strip = row 名 (各行端・縦書き)、 軸は最下行 x・左端列 y。
  let fgRows = ["top","bot"]
      fgCols = ["L","M","R"]
      fgX = concat (replicate 6 [1,2,3,4])
      fgY = [1,2,3,4, 1,3,5,7, 1,4,7,10,  2,2,2,2, 4,3,2,1, 1,5,3,8]
      fgR = concatMap (replicate 12) fgRows
      fgC = concat (replicate 2 (concatMap (replicate 4) fgCols))
      facetGridResolver fn = case fn of
        "x" -> Just (NumData (V.fromList fgX))
        "y" -> Just (NumData (V.fromList fgY))
        "r" -> Just (TxtData (V.fromList fgR))
        "c" -> Just (TxtData (V.fromList fgC))
        _   -> Nothing
  emitR "decoration/facet-grid.svg" facetGridResolver $
       purePlot
    <> layer (scatter "x" "y" <> size 6)
    <> facetGrid "r" "c"
    <> title "facet_grid (r ~ c、 2 行 × 3 列)"

  -- Phase 11 A7-b: facet_grid free scales + space="free"。 列ごとに x 範囲が違い
  --   (L:0..3, M:0..6, R:0..12 = 1:2:4)、 行ごとに y 範囲が違う (top:0..4, bot:0..8 = 1:2)。
  --   free_x = 列共有 x domain、 free_y = 行共有 y domain。 space free で列幅/行高を
  --   data 範囲に比例 (= 各 panel の単位長が揃う)。 A6: 旧データは 1:10:100 / 1:100 と
  --   範囲差が極端で L列・top行が比例で極小化し潰れたため、 穏当な比に緩和 (計算ロジックは不変)。
  let fgfX = [ 0,1,2,3,  0,2,4,6,  0,4,8,12              -- top 行: L/M/R (範囲 3:6:12 = 1:2:4)
             , 0,1,2,3,  0,2,4,6,  0,4,8,12 ]            -- bot 行
      fgfY = [ 1,2,3,4,  1,2,3,4,  1,2,3,4               -- top 行: y 範囲 3
             , 2,4,6,8,  2,4,6,8,  2,4,6,8 ]             -- bot 行: y 範囲 6 (top:bot = 1:2)
      facetGridFreeResolver fn = case fn of
        "x" -> Just (NumData (V.fromList fgfX))
        "y" -> Just (NumData (V.fromList fgfY))
        "r" -> Just (TxtData (V.fromList (concatMap (replicate 12) ["top","bot"])))
        "c" -> Just (TxtData (V.fromList (concat (replicate 2 (concatMap (replicate 4) ["L","M","R"])))))
        _   -> Nothing
  emitR "decoration/facet-grid-free.svg" facetGridFreeResolver $
       purePlot
    <> layer (scatter "x" "y" <> size 6)
    <> facetGrid "r" "c"
    <> facetScales FacetFree
    <> facetSpace SpaceFree
    <> title "facet_grid free + space (列x/行y 独立・幅比例)"

  -- Phase 9 A-4: facet strip.background (灰矩形) の theme 連動デモ。 HggCanvas (羊皮紙 strip) で
  -- strip 帯が panel/grid と調和して描かれることを確認。 facet_grid で 上 strip + 右 strip 両方。
  emitR "decoration/facet-strip-themed.svg" facetGridResolver $
       purePlot
    <> layer (scatter "x" "y" <> size 6)
    <> facetGrid "r" "c"
    <> theme ThemeCanvas
    <> title "facet strip + theme (HggCanvas 羊皮紙 strip)"

  -- subplots
  let sp1 = purePlot
              <> layer (scatter (inline [1.0,2,3,4,5 :: Double]) (inline [1.0,4,9,16,25]))
              <> title "x²"
      sp2 = purePlot
              <> layer (line    (inline [1.0,2,3,4,5 :: Double]) (inline [2.0,4,8,16,32]))
              <> title "2^x"
      sp3 = purePlot
              <> layer (bar     (inlineCat (["a","b","c","d"] :: [Text]))
                                 (inline [3.0,7,5,9]))
              <> title "bar"
  emit "decoration/subplots.svg" $
       purePlot
    <> subplots [sp1, sp2, sp3]
    <> subplotCols 3
    <> title "subplots (任意 spec 並列)"
    <> widthUnit (900 *~ px) <> heightUnit (350 *~ px)

  -- Phase 52.D concat 合成 (Vega-Lite hconcat/vconcat 相当・patchwork 風演算子)。
  -- (a <-> b <-> c) <:> d = 1 行目 3 列 + 2 行目を全幅 (1 行目セルの 3 倍幅)。
  let sp4 = purePlot
              <> layer (scatter (inline [0.0,1,2,3,4 :: Double]) (inline [0.0,3,1,4,2]))
              <> title "full-width row"
  emit "decoration/concat.svg" $
       ((sp1 <-> sp2 <-> sp3) <:> sp4)
    <> title "(a <-> b <-> c) <:> d  (横3列 + 全幅)"
    <> widthUnit (900 *~ px) <> heightUnit (500 *~ px)

  -- annotation (Phase 8 B20: 機能を並べるだけでなく「外れ値を指す」実用的な配置に)。
  -- データはほぼ y=8x の直線だが x=3 だけ外れ値 (70、 本来 24)。 トレンド線 (Line)、
  -- 外れ値を囲む focus 枠 (Rect)、 ラベル (Text) と そこから点への矢印 (Arrow) を連携させる。
  let axs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
      ays = [0, 8, 16, 70, 32, 40, 48, 56, 64, 72] :: [Double]
  emit "decoration/annotation.svg" $
       purePlot
    <> layer (scatter (inline axs) (inline ays) <> size 5)
    <> annotLine  0.0 0.0 9.0 72.0                -- トレンド線 (y=8x)
    <> annotRect  2.5 62.0 1.0 16.0 "#f59e0b"     -- 外れ値 (3,70) を囲む focus 枠 (amber)
    <> annotText  4.7 74.0 "outlier?"             -- ラベル (点の右上)
    <> annotArrow 4.0 74.0 3.18 70.5              -- ラベル → 外れ値点 への矢印
    <> title "annotation (text / arrow / line / rect)"
    <> xLabel "x" <> yLabel "y"

  -- Phase 11 A5-a: labs (title / subtitle / caption / tag)。
  emit "decoration/labs.svg" $
       purePlot
    <> layer (scatter (inline axs) (inline ays) <> size 5)
    <> labs emptyLabs { labsTitle    = Just "Fuel efficiency vs displacement"
                      , labsSubtitle = Just "各点 = 1 車種 (sample data)"
                      , labsCaption  = Just "Source: Hgg gallery demo"
                      , labsTag      = Just "A"
                      , labsX        = Just "displacement"
                      , labsY        = Just "mpg" }

  -- inset axes
  emit "decoration/inset.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> size 5)
    <> title  "scatter + inset (zoom)"
    <> insetAt 0.6 0.05 0.35 0.35
         (purePlot
            <> layer (scatter (inline (take 4 xs)) (inline (take 4 ys)) <> size 7))

  -- marginal (= 軸外に x/y の周辺 histogram)。
  -- Phase 8 B18: 旧 demo は rxs=[0..9] (N=10) で各 bin がほぼ均一 → 周辺分布が見えなかった。
  -- gaussian で相関のある 2 変数 (N=200) にし、 x/y それぞれ正規分布の山が周辺 hist に
  -- 出るようにする (= seaborn jointplot 風)。
  let mgx = gaussian 211 5.0 1.5 200 :: [Double]
      mgy = zipWith (\a e -> a + 1.0 + e) mgx (gaussian 223 0.0 1.0 200)  -- mgx と正相関
  emit "decoration/marginal.svg" $
       purePlot
    <> layer (scatter (inline mgx) (inline mgy) <> size 4 <> alpha 0.5)
    <> marginal
    <> title "scatter + marginal histograms"
    <> xLabel "x" <> yLabel "y"
    <> widthUnit (700 *~ px) <> heightUnit (700 *~ px)

  -- dual Y
  let dyL = map (* 1) ys
      dyR = map (\v -> v / 10) ys
  emit "decoration/dual-y.svg" $
       purePlot
    <> layer (line (inline xs) (inline dyL) <> color (fromHex "#2563eb") <> stroke 2)
    <> layer (line (inline xs) (inline dyR)
               <> color (fromHex "#dc2626") <> stroke 2 <> toRightY)
    <> yAxisRight (mempty <> axisFormat (AxisDecimalFmt 2))
    <> title "dual Y axis"

  -- legend chip: Resolver 経由で color encoding (= 凡例に表示する category)
  let legendResolver k = case k of
        "x" -> Just (NumData (V.fromList ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                            0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double])))
        "y" -> Just (NumData (V.fromList ([0, 1, 4, 9, 16, 25, 36, 49, 64, 81,
                                            0, 0.7, 2.8, 6.3, 11.2, 17.5, 25.2, 34.3, 44.8, 56.7] :: [Double])))
        "group" -> Just (TxtData (V.fromList (replicate 10 "Series A" ++ replicate 10 "Series B")))
        _   -> Nothing
  emitR "decoration/legend.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
    <> legend
    <> legendPos LegendInsideTopLeft
    <> title "legend chip (top-left)"

  -- Phase 9 A-5: legend 配置を ggplot に揃える (panel を縮め図内に収める)。 Right (既定) は
  -- panel 右の予約域、 Bottom は panel 下の予約域、 continuous は gradient bar。 HS/PS 同一。
  emitR "decoration/legend-bottom.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
    <> legendPos LegendBottom
    <> title "legend (bottom)"
  emitR "decoration/legend-continuous.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorContinuousBy "y" <> size 5)
    <> legend
    <> title "legend (continuous gradient)"

  -- Phase 11 A4-c: 凡例タイトル (= scale name / labs(color=))。 Right/Bottom 両方に出る。
  emitR "decoration/legend-title.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
    <> legendTitle "Series"
    <> title "legend title (= scale name)"
  emitR "decoration/legend-title-bottom.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
    <> legendPos LegendBottom
    <> legendTitle "Series"
    <> title "legend title (bottom)"

  -- Phase 11 A5-c: guides サブシステム (reverse / ncol / guide hide)。
  emitR "decoration/legend-reverse.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
    <> legend
    <> legendReverse
    <> title "legend (reverse = キー逆順, 色は固定)"
  -- ncol: 6 カテゴリを 2 列に。
  let ncolResolver k = case k of
        "x" -> Just (NumData (V.fromList (map fromIntegral [0 .. 11 :: Int])))
        "y" -> Just (NumData (V.fromList (map (\i -> fromIntegral (i `mod` 6 :: Int)) [0 .. 11 :: Int])))
        "g" -> Just (TxtData (V.fromList (concatMap (replicate 2)
                 (["cat-1", "cat-2", "cat-3", "cat-4", "cat-5", "cat-6"] :: [Text]))))
        _   -> Nothing
  emitR "decoration/legend-ncol.svg" ncolResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "g" <> size 5)
    <> legend
    <> legendNcol 2
    <> title "legend (ncol=2, 6 カテゴリ → 2 列)"
  -- guide hide: 色分けはするが凡例を出さない (= guides(color="none"))。
  emitR "decoration/guide-none.svg" legendResolver $
       purePlot
    <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
    <> guideColorNone
    <> title "guides(color=none) = 凡例非表示"

  -- refLine
  emit "decoration/refline.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> size 5)
    <> refHorizontal 40
    <> title "scatter + reference horizontal at y=40"

  -- jitter (Phase 6 A8、 P14 解消)
  let jxs = concat $ replicate 4 [1.0, 2, 3, 4, 5] :: [Double]
      jys = concat $ replicate 4 [3.0, 3, 3, 3, 3]
  emit "decoration/jitter.svg" $
       purePlot
    <> layer (scatter (inline jxs) (inline jys)
               <> size 5
               <> jitterX 0.02 <> jitterY 0.02
               <> alpha 0.6)
    <> title "scatter with jitter (deterministic、 hash-rand offset)"

  -- ===========================================================================
  -- AXES
  -- ===========================================================================

  -- log axis
  let lxs = [1, 10, 100, 1000, 10000] :: [Double]
      lys = [2.0, 8.0, 16.0, 64.0, 256.0]
  emit "axes/log.svg" $
       purePlot
    <> layer (scatter (inline lxs) (inline lys) <> size 6)
    <> xAxis logAxis
    <> yAxis logAxis
    <> title "log-log axes"

  -- sqrt axis (Phase 6 A6 で Layout 対応済)
  emit "axes/sqrt.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> size 5)
    <> yAxis sqrtAxis
    <> title "sqrt y axis (= Phase 6 A6)"

  -- time axis (Phase 6 A7 で Layout + niceTimeTicks 対応)
  -- 入力: unix epoch (= seconds since 1970)
  let tdays = [1735689600.0, 1735776000, 1735862400, 1735948800, 1736035200, 1736121600] :: [Double]
      tvals = [10.0, 14.0, 11.0, 16.0, 18.0, 13.0]
  emit "axes/time.svg" $
       purePlot
    <> layer (line (inline tdays) (inline tvals) <> stroke 2)
    <> xAxis (timeAxis "%Y-%m-%d")
    <> title "time axis (= Phase 6 A7、 unix epoch + AxisTimeFmt)"

  -- format
  emit "axes/format.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline (map (* 100000) ys)) <> size 5)
    <> yAxis (axisFormat (AxisExponentFmt 1))
    <> title "exponent y format"

  -- coord_fixed (Phase 8 A2 Step2): aspect = panel 高/幅比。 1.0 で正方 panel を
  -- 可用域内に取り中央寄せ (ggplot coord_fixed)。 600x400 なので左右に余白が出る。
  emit "axes/coord-fixed.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> size 5)
    <> aspectRatio 1.0
    <> title "coord_fixed (aspect = 1)"
    <> xLabel "x" <> yLabel "y"

  -- ===========================================================================
  -- THEME
  -- ===========================================================================

  emit "theme/dark.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> size 5 <> color (fromHex "#56B4E9"))
    <> theme ThemeDark
    <> title "dark theme"

  emit "theme/light.svg" $
       purePlot
    <> layer (scatter (inline xs) (inline ys) <> size 5)
    <> theme ThemeLight
    <> title "light theme (= default)"

  -- Phase 9 A-1: theme preset (ggplot theme_grey) + HggCanvas ブランド 3 種。
  -- color by group で各 theme の panel 背景・grid・既定 series palette を一度に確認。
  -- series palette は 7 色あるので 7 群 (A-G) を扇状にずらした直線で配置し、 全色を一度に確認。
  let gxs       = [0,1,2,3,4,5,6,7] :: [Double]
      grpY i    = [ fromIntegral (i * 3) + (1.0 + fromIntegral i * 0.18) * x | x <- gxs ]
      grpLabels = ["A","B","C","D","E","F","G"] :: [Text]
      themeResolver k = case k of
        "x"     -> Just (NumData (V.fromList (concat (replicate 7 gxs))))
        "y"     -> Just (NumData (V.fromList (concat [ grpY i | i <- [0..6] :: [Int] ])))
        "group" -> Just (TxtData (V.fromList (concat [ replicate 8 g | g <- grpLabels ])))
        _       -> Nothing
      themeDemo file thm ttl =
        emitR file themeResolver $
             purePlot
          <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
          <> theme thm
          <> legend
          <> title ttl
  themeDemo "theme/grey.svg"  ThemeGrey         "theme_grey (ggplot 既定: 灰 panel + 白 grid)"
  themeDemo "theme/noir.svg"  ThemeNoir  "HggCanvas Noir (暗・寒色アクセント)"
  themeDemo "theme/lumen.svg" ThemeLumen "HggCanvas Lumen (白基調・深い差し色)"
  themeDemo "theme/canvas.svg"      ThemeCanvas     "HggCanvas (明: 外周白 + 羊皮紙 panel)"
  themeDemo "theme/canvas-dark.svg" ThemeCanvasDark "HggCanvas Dark (Charcoal)"
  themeDemo "theme/bw.svg"       ThemeBW       "theme_bw (白背景 + 4 辺枠)"
  themeDemo "theme/classic.svg"  ThemeClassic  "theme_classic (grid なし + 下/左 軸線)"
  themeDemo "theme/void.svg"     ThemeVoid     "theme_void (背景・grid・枠 すべてなし)"
  themeDemo "theme/linedraw.svg" ThemeLinedraw "theme_linedraw (細い黒 grid + 黒枠)"

  -- 学術向け named series palette を theme と独立に適用するデモ (theme=Minimal 白背景固定で色だけ差替)。
  let paletteDemo file pal ttl =
        emitR file themeResolver $
             purePlot
          <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
          <> theme ThemeMinimal
          <> palette pal
          <> legend
          <> title ttl
  paletteDemo "palette/okabe-ito.svg"    okabeIto    "Okabe-Ito (colorblind-safe)"
  paletteDemo "palette/tol-bright.svg"   tolBright   "Paul Tol bright (colorblind-safe)"
  paletteDemo "palette/brewer-set2.svg"  brewerSet2  "ColorBrewer Set2"
  paletteDemo "palette/brewer-dark2.svg" brewerDark2 "ColorBrewer Dark2"

  -- A-2: element 単位 theme override の例 (preset に themeGrid/panelBorder 等を `<>` で重ねる)。
  let overrideDemo file extra ttl =
        emitR file themeResolver $
             purePlot
          <> layer (scatter "x" "y" <> colorBy "group" <> size 5)
          <> extra
          <> legend
          <> title ttl
  overrideDemo "theme/ov-grey-nogrid.svg"
    (theme ThemeGrey <> themeGrid False)
    "theme_grey + grid off (override)"
  overrideDemo "theme/ov-canvas-custom.svg"
    (theme ThemeCanvas <> panelBorder True <> gridColor "#d8c9a8")
    "canvas + border on + custom grid (override)"

  -- A-3: 文字 theme 統合の例。 theme 経由で title / tick の font を上書き、 axis.text を回転。
  overrideDemo "theme/ov-font.svg"
    (theme ThemeMinimal
      <> themeTitleFont (fontSize 20 <> fontWeight "bold" <> fontColor "#7A1F23")
      <> themeTickFont  (fontSize 13 <> fontColor "#3E6A6F"))
    "theme font override (title 20pt bold + tick 13pt teal)"
  overrideDemo "theme/ov-axis-angle.svg"
    (theme ThemeMinimal <> themeAxisTextAngle 45)
    "theme axis.text angle = 45° (x/y 両軸)"

  -- ===========================================================================
  -- STATISTICAL: DAG / ModelGraph (= Phase 1 完了の hbm 風)
  -- ===========================================================================

  let alphaN  = ("alpha"  :: Text, "α"      :: Text, NodeLatent)
      betaN   = ("beta"   :: Text, "β"      :: Text, NodeLatent)
      sigmaN  = ("sigma"  :: Text, "σ"      :: Text, NodeLatent)
      muN     = ("mu"     :: Text, "μ"      :: Text, NodeLatent)
      yN      = ("y"      :: Text, "y"      :: Text, NodeObserved)
      nodes   = [ dagNode (one t)   (two t)   (three t) 0 0
                | t <- [alphaN, betaN, sigmaN, muN, yN] ]
      edges   = [ dagEdge "alpha" "mu"
                , dagEdge "beta"  "mu"
                , dagEdge "mu"    "y"
                , dagEdge "sigma" "y" ]
      one (a,_,_) = a; two (_,b,_) = b; three (_,_,c) = c
      -- ★ 階層 layout (Sugiyama) を適用して dnX/dnY を埋める。 これを通さないと
      -- 全 node が dnX=dnY=0 のまま 1 点に潰れる (dagPlotWith の内部処理と同一)。
      (positioned, routedEdges) = layoutHierarchicalFull nodes edges
  emit "statistical/dag-hbm.svg" $
       purePlot
    <> layer (dagFromLists positioned routedEdges LayoutHierarchical)
    <> title "DAG (HBM-like): Phase 1 Sugiyama framework"
    <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)

  putStrLn "Done. Outputs in design/gallery/"

-- ===========================================================================
-- helper: 共通 width/height + emptyResolver で saveSVG
-- ===========================================================================

-- ===========================================================================
-- helper: 決定論的 Gaussian (= 再現可能な demo データ生成、 Phase 8)
-- ===========================================================================

-- | 線形合同法 (LCG) による決定論的 [0,1) 一様乱数列。 seed 固定で再現可能。
-- glibc 系パラメータ (a=1103515245, c=12345, m=2^31)。
lcg01 :: Int -> [Double]
lcg01 seed = go (fromIntegral seed `mod` m)
  where
    a = 1103515245 :: Integer
    c = 12345      :: Integer
    m = 2147483648 :: Integer  -- 2^31
    go s = let s' = (a * s + c) `mod` m
           in fromIntegral s' / fromIntegral m : go s'

-- | 決定論的 Gaussian N(mu, sigma) を n 個。 Box-Muller (LCG 一様列のペアから生成)。
gaussian :: Int -> Double -> Double -> Int -> [Double]
gaussian seed mu sigma n = take n (boxMuller (lcg01 seed))
  where
    boxMuller (u1 : u2 : rest) =
      let r = sqrt (negate 2 * log (max 1e-12 u1))
          z = r * cos (2 * pi * u2)
      in (mu + sigma * z) : boxMuller rest
    boxMuller _ = []

-- | Phase 8 B2: 分布系プロット (raincloud/violin/strip/swarm/ridge/box) で共用する
-- 群データ。 4 群 × 各 N=90、 群ごとに平均・分散を変えた正規分布 (= 参照画像の
-- DD/DR/RD/RR 風)。 決定論的なので compare 回帰に使える。
groupLabels :: [Text]
groupLabels = ["DD", "DR", "RD", "RR"]

groupSpecs :: [(Text, Int, Double, Double)]  -- (label, seed, mu, sigma)
groupSpecs =
  [ ("DD", 11, 95.0, 22.0)
  , ("DR", 23, 82.0, 16.0)
  , ("RD", 37, 101.0, 20.0)
  , ("RR", 53, 78.0, 14.0)
  ]

-- | 群データを (繰り返しラベル, 値) の 2 列に flatten。 各群 N=90。
groupedDemo :: ([Text], [Double])
groupedDemo =
  let n = 90
      perGroup = [ (replicate n lbl, gaussian seed mu sigma n)
                 | (lbl, seed, mu, sigma) <- groupSpecs ]
  in (concatMap fst perGroup, concatMap snd perGroup)

emit :: FilePath -> VisualSpec -> IO ()
emit relPath spec = do
  let sized = spec <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  saveSVG ("design/gallery/" ++ relPath) sized
  -- DAG layer を含む spec は edge routing (deRoute) を JSON へ焼き込む。
  -- PS canvas は routing を持たないため、 これで HS と同じ spline を再現できる
  -- (DAG を含まない spec には no-op)。
  writeSpecJSON relPath (bakeDAGRoutesInSpec sized)

emitR :: FilePath -> Resolver -> VisualSpec -> IO ()
emitR relPath r spec = do
  let sized = spec <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  saveSVGWith ("design/gallery/" ++ relPath) r sized
  -- Phase 8 B16: Resolver を JSON に焼き込む (ColByName → inline)。 PS は Resolver を
  -- 持たないため、 これをしないと pairs/facet/legend 等が PS で空になる。
  writeSpecJSON relPath (bakeSpec r sized)

-- | spec を JSON 化して design/gallery/specs/ に同名 .json で保存。
-- 比較 demo (= compare.html) で PS canvas backend に渡す入力になる。
-- Resolver は inline data 前提なので保存しない (= ColNum / ColTxt は inline で完結)。
writeSpecJSON :: FilePath -> VisualSpec -> IO ()
writeSpecJSON relPath spec = do
  let jsonPath = "design/gallery/specs/" ++ replaceExt relPath ".json"
      dir      = takeDir jsonPath
  System.Directory.createDirectoryIfMissing True dir
  Data.ByteString.Lazy.writeFile jsonPath (Data.Aeson.encode spec)
  where
    replaceExt p _ = takeWhile (/= '.') p ++ ".json"
    takeDir p =
      let parts = reverse (splitOn '/' p)
      in case parts of
        []     -> "."
        [_]    -> "."
        _:rest -> joinWith '/' (reverse rest)
    splitOn c s = case break (== c) s of
      (h, [])   -> [h]
      (h, _:t) -> h : splitOn c t
    joinWith _ []     = ""
    joinWith _ [x]    = x
    joinWith c (x:xs) = x ++ [c] ++ joinWith c xs
