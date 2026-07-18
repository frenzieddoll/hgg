-- | チュートリアル 08: 日付と時刻 (R4DS 2e Ch17 "Dates and times")
--   https://r4ds.hadley.nz/datetimes
--
--   flights の year/month/day/dep_time から時刻の成分を取り出し、 時間単位で
--   集計して「年内の便数推移・時間帯分布・曜日効果・分単位の遅延・丸め癖」 を見る。
--   dataframe には lubridate 相当が無いので、 Haskell の Data.Time で実日付演算し
--   (= 捏造でない実カレンダー)、 集計結果を DataFrame に組み立てて描く。
--
--   flights は全 12 月を保つ系統サンプル (1/20 = 16,839 行・実データ部分集合・値不変)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.DeepSeq          (force, NFData)
import           Control.Exception        (try, evaluate, SomeException)
import           System.IO.Unsafe         (unsafePerformIO)
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.List                (sortOn)
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Time.Calendar       (fromGregorian, DayOfWeek (..), dayOfWeek)
import           Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import qualified DataFrame                as DF
import qualified DataFrame.Internal.Column as DFC
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.DataFrame   ()

-- 例外セーフな列読み取り (型が合わなければ Nothing)。
safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colInt :: Text -> DF.DataFrame -> [Int]
colInt n df = fromMaybe [] (safeCol @Int n df)

colMaybeInt :: Text -> DF.DataFrame -> [Maybe Int]
colMaybeInt n df = case safeCol @(Maybe Int) n df of
  Just xs -> xs
  Nothing -> map Just (colInt n df)

-- 集計結果 (キー Int, 値 Double) を x/y 2 列の DataFrame に。
mkXY :: Text -> Text -> [(Int, Double)] -> DF.DataFrame
mkXY xn yn kvs =
  let sorted = sortOn fst kvs
  in DF.fromNamedColumns
       [ (xn, DF.fromList (map (fromIntegral . fst) sorted :: [Int]))
       , (yn, DF.fromList (map snd sorted :: [Double])) ]

-- 件数集計 (= count)。
countBy :: [Int] -> [(Int, Double)]
countBy ks = M.toList (M.fromListWith (+) [ (k, 1.0) | k <- ks ])

main :: IO ()
main = do
  df <- DF.readCsv "flights.csv"
  let years  = colInt "year"  df
      months = colInt "month" df
      days   = colInt "day"   df
      depT   = colMaybeInt "dep_time"        df   -- HHMM (キャンセルは Nothing)
      schedT = colMaybeInt "sched_dep_time"  df   -- HHMM
      depDel = colMaybeInt "dep_delay"        df

  -- 各行の実カレンダー日 (Data.Time)。 yday (= 1..365) と weekday を得る。
  let dayOf y m d = fromGregorian (toIntegral y) m d
      ydays   = [ snd (toOrdinalDate (dayOf y m d)) | (y,m,d) <- zip3 years months days ]
      wdays   = [ dayOfWeek (dayOf y m d)           | (y,m,d) <- zip3 years months days ]

  -- === 図1: 年内の便数推移 (日ごと、 = geom_freqpoly binwidth=1日) ===
  saveSVGBound "01-by-day.svg" $
    mkXY "yday" "n" (countBy ydays) |>>
      theme ThemeGrey <> layer (line "yday" "n")
      <> title "便数の年内推移 (1 日ごと)" <> xLabel "day of year" <> yLabel "便数"

  -- === 図2: 時間帯分布 (出発時刻の hour ごと、 = 1 日の中の山) ===
  let hours = [ t `div` 100 | Just t <- depT ]
  saveSVGBound "02-by-hour.svg" $
    mkXY "hour" "n" (countBy hours) |>>
      theme ThemeGrey <> layer (line "hour" "n")
      <> title "出発時刻の時間帯分布" <> xLabel "hour of day" <> yLabel "便数"

  -- === 図3: 曜日ごとの便数 (= wday(label=TRUE) + geom_bar) ===
  let wdayNum w = fromEnum w                   -- Data.Time DayOfWeek は 1 始まり (Mon=1..Sun=7)
      wdayLabel n = T.pack (["Mon","Tue","Wed","Thu","Fri","Sat","Sun"] !! (n - 1))
      wcounts = M.toList (M.fromListWith (+) [ (wdayNum w, 1.0::Double) | w <- wdays ])
      wdayDF = DF.fromNamedColumns
        [ ("wday", DF.fromList ([ wdayLabel n | (n,_) <- sortOn fst wcounts ] :: [Text]))
        , ("n",    DF.fromList ([ c           | (_,c) <- sortOn fst wcounts ] :: [Double])) ]
  saveSVGBound "03-by-weekday.svg" $
    wdayDF |>> theme ThemeGrey <> layer (bar "wday" "n")
           <> title "曜日ごとの便数 (週末は少ない)" <> xLabel "weekday" <> yLabel "便数"

  -- === 図4: 出発「分」 ごとの平均遅延 (= 実際の出発は :20-30/:50-60 が低遅延) ===
  let depMinDelay = [ (t `mod` 100, fromIntegral dl :: Double)
                    | (Just t, Just dl) <- zip depT depDel ]
      meanByMinute = [ (mn, sum vs / fromIntegral (length vs))
                     | (mn, vs) <- M.toList (M.fromListWith (++) [ (k,[v]) | (k,v) <- depMinDelay ]) ]
  saveSVGBound "04-delay-by-minute.svg" $
    mkXY "minute" "avg_delay" meanByMinute |>>
      theme ThemeGrey <> layer (line "minute" "avg_delay")
      <> title "出発「分」 ごとの平均遅延" <> xLabel "minute (0-59)" <> yLabel "平均 dep_delay"

  -- === 図5: 予定出発「分」 の頻度 (= 人は 0/30/5 の倍数を好む = 丸め癖) ===
  let schedMin = [ t `mod` 100 | Just t <- schedT ]
  saveSVGBound "05-sched-minute-freq.svg" $
    mkXY "minute" "n" (countBy schedMin) |>>
      theme ThemeGrey <> layer (line "minute" "n")
      <> title "予定出発「分」 の頻度 (0/30/5 の倍数に集中)" <> xLabel "minute (0-59)" <> yLabel "便数"

  putStrLn "wrote 01 .. 05 (5 SVG)"
  where
    toIntegral :: Int -> Integer
    toIntegral = fromIntegral
