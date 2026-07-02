-- | チュートリアル 01: データ可視化 (R4DS 2e Ch1 "Data visualization")
--   https://r4ds.hadley.nz/data-visualize
--
--   R4DS 第 1 章が **表示する図を、 順番どおり・全数 (24 枚)** 再現する。
--   penguins 全量 (344 個体・欠損 2 行は R4DS と同様に除外) を使い、 R4DS 各図の
--   見た目を hgg の layer (mark …) で同じ図になるよう写す。 hgg では
--   data を |>> で束ね、 scatter/bar/boxplot 等の mark を layer で重ねて図を作り、
--   色・形・大きさは mark 内の colorBy/shapeBy/… で与える
--   (以下のコメントの "R4DS L###" は対応する R 原典の行番号)。
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
--   flipper_length_mm / body_mass_g は 2 行が欠損 (Maybe Int)。 hgg は Maybe 列を
--   直接読み mark/stat とも NA を自動除外する (= R の na.rm) ので raw を直読する。
--   明示除去したいときは `DF.filterJust` も使える (= R4DS の "removing 2 rows")。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Data.Text                (Text)
import qualified DataFrame                as DF
import qualified DataFrame.Functions      as F
import           DataFrame.Operators      ((|>))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound, saveSVG)
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)
import           Hgg.Plot.DataFrame   ()

-- lm 回帰直線 (statLm) の既定線色 (= 単一回帰線のとき、 R4DS と同じ青)。
smoothBlue :: Color
smoothBlue = fromHex "#3366FF"

main :: IO ()
main = do
  raw <- DF.readCsv "penguins.csv"

  -- 欠損値: flipper/body_mass は 2 行が NA (Maybe Int)。 hgg は Maybe 列を
  --   列名で直接読み、 mark も stat も NA を自動で落とす (= R の na.rm) ので、
  --   以降の図は raw をそのまま使う。 明示的に落としたいときは DF.filterJust も使える
  --   (R4DS の "removing 2 rows" に相当):
  let cleaned = raw |> DF.filterJust "flipper_length_mm"
                    |> DF.filterJust "body_mass_g"
  putStrLn $ "rows: raw = " <> show (fst (DF.dimensions raw))
           <> " / filterJust 後 = " <> show (fst (DF.dimensions cleaned))
           <> " (NA 2 行)。 以降の図は raw を直読 (NA 自動除外)。"

  -- =========================================================================
  -- §1.1 章扉の motivating plot (R4DS L137): final と同一の完成図
  -- =========================================================================
  saveSVGBoundStats "01-teaser.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
        <> palette okabeIto
        <> title "Body mass and flipper length"
        <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
        <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
        <> legendTitle "Species"
        <> theme ThemeGrey

  -- =========================================================================
  -- §1.2 散布図を一歩ずつ組み立てる
  -- =========================================================================

  -- 空パネル: layer を重ねない純粋 spec (purePlot = mempty) がそのまま空のパネル
  --   (R4DS の ggplot(penguins) に相当)。
  saveSVG "02-empty.svg" $
    purePlot

  -- 軸枠だけ: mark を重ねないので点は無い。 列を指定していないので目盛は
  --   既定レンジ (= hgg はデータ未指定なら 0-1 軸)。 R4DS の「軸枠だけ・mark 無し」
  --   に対応する step だが、 hgg では mark を足して初めて列・スケールが決まる。
  saveSVGBound "03-empty-axes.svg" $
    raw |>> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> theme ThemeGrey

  -- R4DS L202: 最初の散布図 (scatter mark)。
  saveSVGBound "04-scatter.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> theme ThemeGrey

  -- R4DS L241: 種で色分けした散布 (scatter <> colorBy "species"・ggplot 既定 hue)。
  saveSVGBound "05-color.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"
        <> theme ThemeGrey

  -- R4DS L265: colorBy "species" を散布と回帰の両 layer に効かせ → 種ごと 3 本の lm 線。
  saveSVGBoundStats "06-smooth-species.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> colorBy "species")
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"
        <> theme ThemeGrey

  -- R4DS L287: colorBy は散布点だけに付ける → 回帰線は全体 1 本 (ggplot 既定の青)。
  saveSVGBoundStats "07-smooth-global.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"
        <> theme ThemeGrey

  -- R4DS L310: 色+形で種を区別した散布 + lm 回帰線 全体 1 本。
  saveSVGBoundStats "08-shape.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"
        <> theme ThemeGrey

  -- R4DS L336: タイトル・整形ラベル + colorblind パレットを添えた完成図。
  saveSVGBoundStats "09-final.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
        <> layer (statLm "flipper_length_mm" "body_mass_g" <> color smoothBlue)
        <> palette okabeIto
        <> title "Body mass and flipper length"
        <> subtitle "Dimensions for Adelie, Chinstrap, and Gentoo Penguins"
        <> xLabel "Flipper length (mm)" <> yLabel "Body mass (g)"
        <> legendTitle "Species"
        <> theme ThemeGrey

  -- =========================================================================
  -- §1.4 1 変数の分布
  -- =========================================================================

  -- 棒は件数集計が要る。 ここでは件数を |> で先に集計してから bar mark で描く (値は不変)。
  let bySpecies = raw |> DF.groupBy ["species"]
                      |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

  -- R4DS L489: 種ごとの件数の棒 (Adelie 152 / Chinstrap 68 / Gentoo 124)。
  --   x はアルファベット順 (= ggplot factor 既定 / 本ライブラリの既定)。
  saveSVGBound "10-bar-species.svg" $
    bySpecies |>> layer (bar "species" "n")
                <> xLabel "species" <> yLabel "count"
                <> theme ThemeGrey

  -- R4DS L502: 件数降順に並べた棒。 scaleXDiscreteLimits で水準順を
  --   Adelie(152) > Gentoo(124) > Chinstrap(68) に固定する (R4DS の fct_infreq 相当)。
  saveSVGBound "11-bar-infreq.svg" $
    bySpecies |>> layer (bar "species" "n")
                <> scaleXDiscreteLimits ["Adelie", "Gentoo", "Chinstrap"]
                <> xLabel "species" <> yLabel "count"
                <> theme ThemeGrey

  -- R4DS L520: 体重のヒストグラム (binwidth 200)。
  saveSVGBound "12-histogram-bw200.svg" $
    raw |>> layer (histogram "body_mass_g" <> binWidth 200)
         <> xLabel "body_mass_g" <> yLabel "count"
         <> theme ThemeGrey

  -- R4DS L542: binWidth 20 (細かすぎてギザギザ)。
  saveSVGBound "13-histogram-bw20.svg" $
    raw |>> layer (histogram "body_mass_g" <> binWidth 20)
         <> xLabel "body_mass_g" <> yLabel "count"
         <> theme ThemeGrey

  -- R4DS L544: binWidth 2000 (粗すぎて 3 bin)。
  saveSVGBound "14-histogram-bw2000.svg" $
    raw |>> layer (histogram "body_mass_g" <> binWidth 2000)
         <> xLabel "body_mass_g" <> yLabel "count"
         <> theme ThemeGrey

  -- R4DS L560: 体重の密度曲線。
  saveSVGBound "15-density.svg" $
    raw |>> layer (density "body_mass_g")
         <> xLabel "body_mass_g" <> yLabel "density"
         <> theme ThemeGrey

  -- =========================================================================
  -- §1.5 2 変数の関係
  -- =========================================================================

  -- R4DS L630: 種 × 体重の箱ひげ図。
  saveSVGBound "16-boxplot.svg" $
    raw |>> layer (boxplot "body_mass_g" <> groupBy "species")
         <> xLabel "species" <> yLabel "body_mass_g"
         <> theme ThemeGrey

  -- R4DS L642: 種ごとに色分けした密度曲線 (linewidth 0.75)。
  saveSVGBound "17-density-color.svg" $
    raw |>> layer (density "body_mass_g" <> colorBy "species")
         <> xLabel "body_mass_g" <> yLabel "density"
         <> legendTitle "species"
         <> theme ThemeGrey

  -- R4DS L659: 種ごとに塗りつぶした密度曲線 (alpha 0.5)。
  saveSVGBound "18-density-fill.svg" $
    raw |>> layer (density "body_mass_g" <> colorBy "species"
                   <> densityFill True <> alpha 0.5)
         <> xLabel "body_mass_g" <> yLabel "density"
         <> legendTitle "species"
         <> theme ThemeGrey

  -- 島 × 種の件数 (stack / fill 用)。
  let byIslandSpecies = raw |> DF.groupBy ["island", "species"]
                            |> DF.aggregate [ F.count (F.col @Text "species") `F.as` "n" ]

  -- R4DS L680: 島ごと × 種で色分けした積み上げ棒 (既定 stack)。
  saveSVGBound "19-bar-stack.svg" $
    byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosStack)
                      <> xLabel "island" <> yLabel "count"
                      <> legendTitle "species"
                      <> theme ThemeGrey

  -- R4DS L692: position PosFill で各島の合計を 1 に揃える (y 軸は既定で "count" 表記のまま)。
  saveSVGBound "20-bar-fill.svg" $
    byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosFill)
                      <> xLabel "island" <> yLabel "count"
                      <> legendTitle "species"
                      <> theme ThemeGrey

  -- R4DS L704: 同上で y 軸ラベルを yLabel "proportion" に直す。
  saveSVGBound "21-bar-fill-proportion.svg" $
    byIslandSpecies |>> layer (bar "island" "n" <> colorBy "species" <> position PosFill)
                      <> xLabel "island" <> yLabel "proportion"
                      <> legendTitle "species"
                      <> theme ThemeGrey

  -- R4DS L720: §1.5.3 冒頭の素の散布図。
  saveSVGBound "22-scatter-plain.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g" <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> theme ThemeGrey

  -- R4DS L739: 3 変数の散布 (色=種・形=島)。
  saveSVGBound "23-scatter-shape-island.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> shapeBy "island" <> alpha 0.85)
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"
        <> theme ThemeGrey

  -- R4DS L761: facetWrap で島ごとのパネルに分割。
  saveSVGBound "24-facet-island.svg" $
    raw |>> layer (scatter "flipper_length_mm" "body_mass_g"
                   <> colorBy "species" <> shapeBy "species" <> alpha 0.85)
        <> facetWrap "island" 3
        <> xLabel "flipper_length_mm" <> yLabel "body_mass_g"
        <> legendTitle "species"
        <> theme ThemeGrey

  putStrLn "wrote 01-teaser .. 24-facet-island (24 SVG)"
