-- | R4DS 2e 「Exploratory data analysis」 章 (Ch10 EDA) の忠実再現。
--   元章: https://r4ds.hadley.nz/eda
--   diamonds / mpg / nycflights13::flights の実データ全量を使い、 R4DS 本文に
--   登場する全図 (23 枚) と 2 つの集計テーブルを掲載順に生成する。
--   ggplot2 の geom_histogram / geom_freqpoly / geom_boxplot / geom_count /
--   geom_tile / geom_bin2d を hgg の対応 mark で再現する。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.DeepSeq          (force, NFData)
import           Control.Exception        (try, evaluate, SomeException)
import           System.IO.Unsafe         (unsafePerformIO)
import           Data.Maybe               (fromMaybe, isNothing, mapMaybe)
import           Data.List                (sort, sortOn, nub)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Numeric                  (showFFloat)
import qualified DataFrame                as DF
import qualified DataFrame.Internal.Column as DFC
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.DataFrame   ()

-- === 列抽出ヘルパ (= 18-missing と同型。 例外安全に列を list 化) ===

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colMaybe :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [Maybe a]
colMaybe n df = fromMaybe [] (safeCol @(Maybe a) n df)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

-- | 数値列を Double で取り出す (Double / Int いずれの推論型でも吸収)。
--   ★ price のような整数値のみの列は DataFrame が Int 列に推論するため、
--   @colPlain \@Double@ だと空になる (= 異常値テーブルが空になった真因)。
numCol :: Text -> DF.DataFrame -> [Double]
numCol n df = case safeCol @Double n df of
  Just xs@(_:_) -> xs
  _ -> case safeCol @Int n df of
    Just xs@(_:_) -> map fromIntegral xs
    _             -> []

-- === 小道具 ===

-- | カテゴリ列の標準順 (cut は品質昇順、 ggplot の factor 順)。
cutOrder :: [Text]
cutOrder = ["Fair", "Good", "Very Good", "Premium", "Ideal"]

median :: [Double] -> Double
median [] = 0
median xs = let s = sort xs; n = length s
            in if even n then (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2
                         else s !! (n `div` 2)

fmt1 :: Double -> Text
fmt1 x = T.pack (showFFloat (Just 1) x "")

main :: IO ()
main = do
  diamonds <- DF.readCsv "../_data/_raw/diamonds.csv"
  let carat = numCol "carat" diamonds
      yv    = numCol "y"     diamonds
      xv    = numCol "x"     diamonds
      zv    = numCol "z"     diamonds
      price = numCol "price" diamonds
      cut   = colPlain @Text "cut"   diamonds
      colr  = colPlain @Text "color" diamonds

  -- ============================================================
  -- §Variation — 1 変数の分布
  -- ============================================================

  -- (1) carat の分布 (binwidth=0.5)
  saveSVGBound "01-hist-carat-bw05.svg" $
    diamonds |>> theme ThemeGrey <> layer (histogram "carat" <> binWidth 0.5)
             <> title "carat の分布 (binwidth=0.5)" <> xLabel "carat" <> yLabel "count"

  -- (2) 小さい diamond (carat<3) を細かい bin で (binwidth=0.01)
  let smallerDF = DF.fromNamedColumns
        [ ("carat", DF.fromList [ c | (c,_) <- zip carat price, c < 3 ])
        , ("price", DF.fromList [ p | (c,p) <- zip carat price, c < 3 ]) ]
  saveSVGBound "02-hist-carat-bw001.svg" $
    smallerDF |>> theme ThemeGrey <> layer (histogram "carat" <> binWidth 0.01)
              <> title "carat<3 の分布 (binwidth=0.01)" <> xLabel "carat" <> yLabel "count"

  -- ============================================================
  -- §Unusual values — 外れ値を histogram で探す
  -- ============================================================

  -- (3) y (幅 mm) の分布。 x 軸が異常に広い = 外れ値の痕跡
  saveSVGBound "03-hist-y-bw05.svg" $
    diamonds |>> theme ThemeGrey <> layer (histogram "y" <> binWidth 0.5)
             <> title "y (幅 mm) の分布" <> xLabel "y" <> yLabel "count"

  -- (4) y 軸を 0..50 にズーム (coord_cartesian) して低頻度 bin を見る
  saveSVGBound "04-hist-y-zoom.svg" $
    diamonds |>> theme ThemeGrey <> layer (histogram "y" <> binWidth 0.5)
             <> coordCartesianY 0 50
             <> title "y の分布 (y 軸を 0..50 にズーム)" <> xLabel "y" <> yLabel "count"

  -- (表1) 異常値 = y<3 | y>20 を price/x/y/z で抜き出し y 昇順
  let unusual = sortOn (\(_,_,y,_) -> y)
        [ (p,x,y,z) | (((p,x),y),z) <- zip (zip (zip price xv) yv) zv, y < 3 || y > 20 ]
  putStrLn "── 異常値 (y<3 | y>20)、 y 昇順 ──"
  putStrLn "  price      x      y      z"
  mapM_ (\(p,x,y,z) -> putStrLn (pad6 p ++ " " ++ pad6 x ++ " " ++ pad6 y ++ " " ++ pad6 z))
        unusual

  -- ============================================================
  -- §Missing values — 欠損の扱い
  -- ============================================================

  -- (5) y を NA recode (y<3 | y>20 → NA) して x vs y を散布。 NA 行は描かれない
  let xyKept = [ (x,y) | (x,y) <- zip xv yv, y >= 3 && y <= 20 ]
      diamonds2DF = DF.fromNamedColumns
        [ ("x", DF.fromList (map fst xyKept))
        , ("y", DF.fromList (map snd xyKept)) ]
  saveSVGBound "05-scatter-xy.svg" $
    diamonds2DF |>> theme ThemeGrey <> layer (scatter "x" "y" <> alpha 0.4)
                <> title "x vs y (異常な y は NA に recode)" <> xLabel "x" <> yLabel "y"

  -- (6) flights: 欠航 (dep_time が欠損) 別に sched_dep_time の頻度多角形
  flights <- DF.readCsv "../_data/_raw/flights.csv"
  let depTime  = colMaybe @Int "dep_time"        flights
      schedRaw = colPlain @Int "sched_dep_time"  flights
      cancelled = map isNothing depTime
      schedDec  = [ fromIntegral (s `div` 100) + fromIntegral (s `mod` 100) / 60
                  | s <- schedRaw ] :: [Double]
      flightsDF = DF.fromNamedColumns
        [ ("sched_dep_time", DF.fromList schedDec)
        , ("cancelled",      DF.fromList ([ if c then "TRUE" else "FALSE" | c <- cancelled ] :: [Text])) ]
  saveSVGBound "06-freqpoly-flights.svg" $
    flightsDF |>> theme ThemeGrey <> layer (freqpoly "sched_dep_time" <> binWidth 0.25 <> colorBy "cancelled")
              <> title "予定出発時刻の頻度多角形 (欠航別)"
              <> xLabel "sched_dep_time (時)" <> yLabel "count"

  -- ============================================================
  -- §Covariation — カテゴリ × 数値
  -- ============================================================

  -- (7) cut 別 price の頻度多角形 (count)
  saveSVGBound "07-freqpoly-price-count.svg" $
    diamonds |>> theme ThemeGrey <> layer (freqpoly "price" <> binWidth 500 <> colorBy "cut"
                       <> colorCats cutOrder)
             <> title "cut 別 price の頻度多角形 (count)" <> xLabel "price" <> yLabel "count"

  -- (8) 同じく density (after_stat(density)) で高さを揃える
  saveSVGBound "08-freqpoly-price-density.svg" $
    diamonds |>> theme ThemeGrey <> layer (freqpoly "price" <> binWidth 500 <> colorBy "cut"
                       <> colorCats cutOrder <> histogramDensity True)
             <> title "cut 別 price の頻度多角形 (density)" <> xLabel "price" <> yLabel "density"

  -- (9) cut 別 price の箱ひげ図
  saveSVGBound "09-box-price-cut.svg" $
    diamonds |>> theme ThemeGrey <> layer (boxplot "price" <> groupBy "cut")
             <> scaleXDiscreteLimits cutOrder
             <> title "cut 別 price の箱ひげ図" <> xLabel "cut" <> yLabel "price"

  -- mpg: class 別 hwy
  mpg <- DF.readCsv "../_data/mpg.csv"
  let mpgClass = colPlain @Text "class" mpg
      mpgHwy   = numCol "hwy" mpg

  -- (10) class 別 hwy の箱ひげ図 (class はアルファベット順)
  saveSVGBound "10-box-hwy-class.svg" $
    mpg |>> theme ThemeGrey <> layer (boxplot "hwy" <> groupBy "class")
        <> title "class 別 hwy の箱ひげ図" <> xLabel "class" <> yLabel "hwy"

  -- (11) fct_reorder: class を hwy 中央値の昇順に並べ替え
  let classMedian c = median [ h | (cl,h) <- zip mpgClass mpgHwy, cl == c ]
      classByMedian = sortOn classMedian (nub mpgClass)
  saveSVGBound "11-box-hwy-class-reorder.svg" $
    mpg |>> theme ThemeGrey <> layer (boxplot "hwy" <> groupBy "class")
        <> scaleXDiscreteLimits classByMedian
        <> title "class 別 hwy (hwy 中央値の昇順)" <> xLabel "class" <> yLabel "hwy"

  -- (12) 横向き (coord_flip)。 長いカテゴリ名に向く
  saveSVGBound "12-box-hwy-class-flip.svg" $
    mpg |>> theme ThemeGrey <> layer (boxplot "hwy" <> groupBy "class")
        <> scaleXDiscreteLimits classByMedian
        <> coordFlip
        -- coordFlip はデータ軸を反転するが軸タイトルは物理軸 (底=x/左=y) に固定される
        -- ため、 反転後の表示に合わせ底を hwy・左を class とする (R4DS fig と同じ向き)。
        <> title "class 別 hwy (横向き)" <> xLabel "hwy" <> yLabel "class"

  -- ============================================================
  -- §Two categorical variables — カテゴリ × カテゴリ
  -- ============================================================

  -- (13) cut × color の件数 (geom_count)
  saveSVGBound "13-count-cut-color.svg" $
    diamonds |>> theme ThemeGrey <> layer (countXY "cut" "color")
             <> scaleXDiscreteLimits cutOrder
             <> title "cut × color の件数 (geom_count)" <> xLabel "cut" <> yLabel "color"

  -- (表2) count(color, cut)
  let comboCount cl ct = length [ () | (c,t) <- zip colr cut, c == cl, t == ct ]
      colorLevels = sort (nub colr)
  putStrLn ""
  putStrLn "── count(color, cut) ──"
  mapM_ (\cl -> mapM_ (\ct ->
            putStrLn ("  " ++ T.unpack cl ++ "  " ++ T.unpack ct ++ "  " ++ show (comboCount cl ct)))
            cutOrder)
        colorLevels

  -- (14) geom_tile: color × cut を件数で塗る heatmap
  let tileRows = [ (cl, ct, fromIntegral (comboCount cl ct) :: Double)
                 | cl <- colorLevels, ct <- cutOrder ]
      tileDF = DF.fromNamedColumns
        [ ("color", DF.fromList ([ cl | (cl,_,_) <- tileRows ] :: [Text]))
        , ("cut",   DF.fromList ([ ct | (_,ct,_) <- tileRows ] :: [Text]))
        , ("n",     DF.fromList ([ n  | (_,_,n)  <- tileRows ] :: [Double])) ]
  saveSVGBound "14-tile-color-cut.svg" $
    tileDF |>> theme ThemeGrey <> layer (heatmap "color" "cut" "n")
           <> scaleYDiscreteLimits cutOrder
           <> title "color × cut の件数 (geom_tile)" <> xLabel "color" <> yLabel "cut"

  -- ============================================================
  -- §Two numerical variables — 数値 × 数値
  -- ============================================================

  -- (15) carat vs price の散布図 (carat<3)
  saveSVGBound "15-scatter-carat-price.svg" $
    smallerDF |>> theme ThemeGrey <> layer (scatter "carat" "price")
              <> title "carat vs price" <> xLabel "carat" <> yLabel "price"

  -- (16) alpha で重なりを可視化 (alpha=1/100)
  saveSVGBound "16-scatter-carat-price-alpha.svg" $
    smallerDF |>> theme ThemeGrey <> layer (scatter "carat" "price" <> alpha 0.01)
              <> title "carat vs price (alpha=1/100)" <> xLabel "carat" <> yLabel "price"

  -- (17) geom_bin2d: 2D bin の件数を連続色で
  saveSVGBound "17-bin2d-carat-price.svg" $
    smallerDF |>> theme ThemeGrey <> layer (bin2dCount "carat" "price")
              <> title "carat vs price (geom_bin2d, fill=count)"
              <> xLabel "carat" <> yLabel "price"

  -- (18) geom_hex 相当 (※ hgg は六角形 binning 未実装。 矩形 bin2d で代替)
  saveSVGBound "18-hex-carat-price.svg" $
    smallerDF |>> theme ThemeGrey <> layer (bin2dCount "carat" "price")
              <> title "carat vs price (geom_hex 代替 = 矩形 bin2d)"
              <> xLabel "carat" <> yLabel "price"

  -- (19) cut_width(carat, 0.1) で carat を 0.1 刻みに区切り price を箱ひげ図
  let caratBin c = 0.1 * fromIntegral (round (c / 0.1) :: Int)
      cwRows = [ (fmt1 (caratBin c), p) | (c,p) <- zip carat price, c < 3 ]
      cwLabels = map fmt1 (sort (nub [ caratBin c | (c,_) <- zip carat price, c < 3 ]))
      cwDF = DF.fromNamedColumns
        [ ("carat_bin", DF.fromList (map fst cwRows :: [Text]))
        , ("price",     DF.fromList (map snd cwRows :: [Double])) ]
  saveSVGBound "19-box-cutwidth.svg" $
    cwDF |>> theme ThemeGrey <> layer (boxplot "price" <> groupBy "carat_bin")
         <> scaleXDiscreteLimits cwLabels
         <> title "cut_width(carat, 0.1) 別 price の箱ひげ図"
         <> xLabel "carat (0.1 刻み)" <> yLabel "price"

  -- ============================================================
  -- §Patterns and models — モデルで強い関係を除く
  -- ============================================================

  -- log(price) ~ log(carat) の OLS で carat の効果を除き、 残差を exp で価格スケールへ
  let lcarat = map log carat
      lprice = map log price
      nAll   = fromIntegral (length carat) :: Double
      mx = sum lcarat / nAll
      my = sum lprice / nAll
      sxx = sum [ (x-mx)^(2::Int) | x <- lcarat ]
      sxy = sum [ (x-mx)*(y-my)   | (x,y) <- zip lcarat lprice ]
      b   = sxy / sxx
      a   = my - b*mx
      resid = [ exp (y - (a + b*x)) | (x,y) <- zip lcarat lprice ]
      residDF = DF.fromNamedColumns
        [ ("carat", DF.fromList (carat :: [Double]))
        , ("cut",   DF.fromList (cut   :: [Text]))
        , ("resid", DF.fromList (resid :: [Double])) ]

  -- (20) 残差 vs carat
  saveSVGBound "20-resid-carat.svg" $
    residDF |>> theme ThemeGrey <> layer (scatter "carat" "resid" <> alpha 0.2)
            <> title "残差 (carat の効果を除いた価格) vs carat"
            <> xLabel "carat" <> yLabel "resid"

  -- (21) cut 別 残差の箱ひげ図 = 品質が良いほど (相対的に) 高価
  saveSVGBound "21-resid-cut.svg" $
    residDF |>> theme ThemeGrey <> layer (boxplot "resid" <> groupBy "cut")
            <> scaleXDiscreteLimits cutOrder
            <> title "cut 別 残差の箱ひげ図" <> xLabel "cut" <> yLabel "resid"

  putStrLn ""
  putStrLn "Ch10 EDA: 21 figures written."

-- | 数値を右寄せ 6 桁幅で表示 (表整形用)。
pad6 :: Double -> String
pad6 x = let s = showFFloat (Just 2) x "" in replicate (max 0 (6 - length s)) ' ' ++ s
