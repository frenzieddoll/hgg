-- | チュートリアル 01: データ可視化 (R4DS 2e Ch1 "Data visualization")
--   https://r4ds.hadley.nz/data-visualize
--
--   R4DS 第 1 章が **表示する図を、 順番どおり・全数 (24 枚)** 再現する。
--   penguins 全量 (344 個体・欠損 2 行は R4DS と同様に除外) を使い、 R4DS の
--   geom / aes / 設定 (binwidth・position・facet 等) をそのまま写す。
--
--   ・忠実性メモ:
--     - 色分け図 (05-08, 17-23) は ggplot 既定 hue 配色 (= palette 未指定)。
--       colorblind パレット (scale_color_colorblind = okabeIto) は R4DS が使う
--       teaser / final の 2 枚だけに適用する。
--     - 軸ラベルは R4DS 既定どおり変数名そのまま。 タイトル / 整形ラベルは
--       R4DS が labs() を付ける teaser / final のみ。
--     - histogram は binWidth (= R4DS binwidth) で R4DS と同じ bin 境界・棒高。
--
--   DataFrame 変換は dataframe の `|>` 前方パイプ (R4DS の `|>` と同型)。
--   flipper_length_mm / body_mass_g は 2 行が欠損 (Maybe Int) なので
--   `DF.filterJust` で欠損行を除く (= R4DS が "removing 2 rows containing
--   missing values" と警告する箇所)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Data.Text                (Text)
import qualified DataFrame                as DF
import qualified DataFrame.Functions      as F
import           DataFrame.Operators      ((|>))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)
import           Hgg.Plot.DataFrame   ()

-- ggplot geom_smooth(method="lm") の既定線色 (= 単一回帰線のとき)。
smoothBlue :: Text
smoothBlue = "#3366FF"

main :: IO ()
main = do
  raw <- DF.readCsv "penguins.csv"

  -- 散布図系で使う 2 列の欠損行を除く (Maybe Int → Int)。 R4DS の警告と同じ 2 行。
  let p = raw |> DF.filterJust "flipper_length_mm"
              |> DF.filterJust "body_mass_g"

  -- =========================================================================
  -- §1.1 章扉の motivating plot (R4DS L137): final と同一の完成図
  -- =========================================================================
  saveSVGBoundStats "01-teaser.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> shapeBy "species" <> size 5 <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorStatic smoothBlue <> stroke 2)
        <> palette okabeIto
        <> title "Body mass and flipper length"
        <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
        <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
        <> legendTitle "Species"

  -- =========================================================================
  -- §1.2 散布図を一歩ずつ組み立てる
  -- =========================================================================

  -- R4DS L162: ggplot(penguins) = 空のグレーパネル (aes なし → 軸目盛なし)。
  saveSVGBound "02-empty.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.0)
        <> xAxis hideTicks <> yAxis hideTicks
        <> xLabel "" <> yLabel ""

  -- R4DS L178: + aes(x,y) = 軸 (flipper 170-230 / body_mass 3000-6000) のみ、 点なし。
  saveSVGBound "03-empty-axes.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.0)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"

  -- R4DS L202: + geom_point() = 最初の散布図。
  saveSVGBound "04-scatter.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g" <> size 5 <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"

  -- R4DS L241: aes(color=species) + geom_point() = 種で色分け (ggplot 既定 hue)。
  saveSVGBound "05-color.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> size 5 <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"

  -- R4DS L265: global color=species を point/smooth 両方が継承 → 種ごと 3 本の lm 線。
  saveSVGBoundStats "06-smooth-species.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> size 5 <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> color "species" <> stroke 2)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"

  -- R4DS L287: color は geom_point のみ → 回帰線は全体 1 本 (ggplot 既定の青)。
  saveSVGBoundStats "07-smooth-global.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> size 5 <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorStatic smoothBlue <> stroke 2)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"

  -- R4DS L310: geom_point(aes(color,shape=species)) + smooth(lm) 全体 1 本。
  saveSVGBoundStats "08-shape.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> shapeBy "species" <> size 5 <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorStatic smoothBlue <> stroke 2)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"

  -- R4DS L336: + labs(...) + scale_color_colorblind() = 完成図。
  saveSVGBoundStats "09-final.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> shapeBy "species" <> size 5 <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorStatic smoothBlue <> stroke 2)
        <> palette okabeIto
        <> title "Body mass and flipper length"
        <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
        <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
        <> legendTitle "Species"

  -- =========================================================================
  -- §1.4 1 変数の分布
  -- =========================================================================

  -- geom_bar は stat_count を内部で行う。 ここでは件数を |> で先に集計 (値は不変)。
  let bySpecies = raw |> DF.groupBy ["species"]
                      |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

  -- R4DS L489: geom_bar(aes(x=species)) = 種ごとの件数 (Adelie 152 / Chinstrap 68 / Gentoo 124)。
  --   x はアルファベット順 (= ggplot factor 既定 / 本ライブラリの既定)。
  saveSVGBound "10-bar-species.svg" $
    bySpecies |>> layer (bar "species" "n")
                <> xLabel "species" <> yLabel "count"

  -- R4DS L502: geom_bar(aes(x=fct_infreq(species))) = 件数降順。
  --   fct_infreq は factor 水準を件数降順に並べ替える。 ここでは scale_x_discrete(limits=)
  --   相当の scaleXDiscreteLimits で水準順を Adelie(152) > Gentoo(124) > Chinstrap(68) に固定。
  saveSVGBound "11-bar-infreq.svg" $
    bySpecies |>> layer (bar "species" "n")
                <> scaleXDiscreteLimits ["Adelie", "Gentoo", "Chinstrap"]
                <> xLabel "fct_infreq(species)" <> yLabel "count"

  -- 体重 (欠損除外)。 histogram / density で使う。
  let pm = raw |> DF.filterJust "body_mass_g"

  -- R4DS L520: geom_histogram(body_mass_g, binwidth = 200)。
  saveSVGBound "12-histogram-bw200.svg" $
    pm |>> layer (histogram "body_mass_g" <> binWidth 200)
         <> xLabel "body_mass_g" <> yLabel "count"

  -- R4DS L542: binwidth = 20 (細かすぎてギザギザ)。
  saveSVGBound "13-histogram-bw20.svg" $
    pm |>> layer (histogram "body_mass_g" <> binWidth 20)
         <> xLabel "body_mass_g" <> yLabel "count"

  -- R4DS L544: binwidth = 2000 (粗すぎて 3 bin)。
  saveSVGBound "14-histogram-bw2000.svg" $
    pm |>> layer (histogram "body_mass_g" <> binWidth 2000)
         <> xLabel "body_mass_g" <> yLabel "count"

  -- R4DS L560: geom_density(body_mass_g)。
  saveSVGBound "15-density.svg" $
    pm |>> layer (density "body_mass_g")
         <> xLabel "body_mass_g" <> yLabel "density"

  -- =========================================================================
  -- §1.5 2 変数の関係
  -- =========================================================================

  -- R4DS L630: geom_boxplot(aes(x=species, y=body_mass_g))。
  saveSVGBound "16-boxplot.svg" $
    pm |>> layer (boxplotBy "species" "body_mass_g")
         <> xLabel "species" <> yLabel "body_mass_g"

  -- R4DS L642: geom_density(aes(x=body_mass_g, color=species), linewidth=0.75)。
  saveSVGBound "17-density-color.svg" $
    pm |>> layer (density "body_mass_g" <> color "species" <> stroke 1.5)
         <> xLabel "body_mass_g" <> yLabel "density"
         <> legendTitle "species"

  -- R4DS L659: geom_density(aes(color=species, fill=species), alpha=0.5) = 塗りつぶし。
  saveSVGBound "18-density-fill.svg" $
    pm |>> layer (density "body_mass_g" <> color "species"
                   <> densityFill True <> alpha 0.5 <> stroke 1.5)
         <> xLabel "body_mass_g" <> yLabel "density"
         <> legendTitle "species"

  -- 島 × 種の件数 (stack / fill 用)。
  let byIslandSpecies = raw |> DF.groupBy ["island", "species"]
                            |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

  -- R4DS L680: geom_bar(aes(x=island, fill=species)) = 既定 (stack) で積み上げ。
  saveSVGBound "19-bar-stack.svg" $
    byIslandSpecies |>> layer (bar "island" "n" <> color "species" <> position PosStack)
                      <> xLabel "island" <> yLabel "count"
                      <> legendTitle "species"

  -- R4DS L692: position="fill" = 各島の合計を 1 に揃える (y 軸は既定で "count" 表記のまま)。
  saveSVGBound "20-bar-fill.svg" $
    byIslandSpecies |>> layer (bar "island" "n" <> color "species" <> position PosFill)
                      <> xLabel "island" <> yLabel "count"
                      <> legendTitle "species"

  -- R4DS L704: 同上 + labs(y="proportion") で y 軸ラベルを直す。
  saveSVGBound "21-bar-fill-proportion.svg" $
    byIslandSpecies |>> layer (bar "island" "n" <> color "species" <> position PosFill)
                      <> xLabel "island" <> yLabel "proportion"
                      <> legendTitle "species"

  -- R4DS L720: §1.5.3 冒頭の素の散布図 (geom_point())。
  saveSVGBound "22-scatter-plain.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g" <> size 5 <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"

  -- R4DS L739: geom_point(aes(color=species, shape=island)) = 3 変数 (色=種・形=島)。
  saveSVGBound "23-scatter-shape-island.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> shapeBy "island" <> size 5 <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"

  -- R4DS L761: + facet_wrap(~island) = 島ごとのパネル。
  saveSVGBound "24-facet-island.svg" $
    p |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> color "species" <> shapeBy "species" <> size 4 <> alpha 0.85)
        <> facetWrap "island" 3
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"

  putStrLn "wrote 01-teaser .. 24-facet-island (24 SVG)"
