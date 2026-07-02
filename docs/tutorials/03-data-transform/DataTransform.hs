-- | チュートリアル 03: データ変換 (R4DS 2e Ch3 "Data transformation")
--   https://r4ds.hadley.nz/data-transform
--
--   nycflights13 の flights (全 336,776 行) で dplyr の動詞を dataframe に
--   **1:1 で対応づける**。 この章は「表を変換する」のが主役で、 R4DS が本文で
--   描く図は 1 枚だけ (case study の打者成績散布図 = batters.svg)。 残りの例は
--   すべて tibble の印字なので、 各操作の結果 DataFrame を print する。
--
--   ・データは全量を使う (= 間引きなし)。 flights は ../_data/_raw/flights.csv
--     (gitignore・先頭 rownames 列は exclude)、 Lahman Batting は
--     ../_data/_raw/batting.csv (playerID/AB/H)。
--   ・キャンセル便の dep_delay/arr_delay/air_time 等は欠損 (Maybe Int)。 R の
--     算術は NA を伝播する (gain = NA など) ので、 mutate は 'F.nullLift2' で
--     NA 伝播させ **行を落とさない** (R と同じ 336,776 行を保つ)。
--   ・変換は dataframe の `|>` 前方パイプ (R4DS の `|>` と同型)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified DataFrame                as DF
import qualified DataFrame.Functions      as F
import           DataFrame.Internal.Column (columnTypeString)
import           DataFrame.Operators      ((|>), (.>), (.==), (.&&), (.||))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)
import           Hgg.Plot.DataFrame   ()

-- 章見出しと結果 (DataFrame) をまとめて表示する小ヘルパ。
sect :: String -> IO ()
sect s = putStrLn ("\n========== " <> s <> " ==========")

-- 列名リストの name の直後に new を差し込む (relocate / mutate .after 用)。
insertAfter :: Text -> [Text] -> [Text] -> [Text]
insertAfter name new xs = let (a, b) = break (== name) xs
                          in a <> take 1 b <> new <> drop 1 b

main :: IO ()
main = do
  flightsRaw <- DF.readCsv "../_data/_raw/flights.csv"
  -- 先頭の rownames 列 (CSV 化の副産物) を除く。
  let flights = flightsRaw |> DF.exclude ["rownames"]
      cols    = DF.columnNames flights

  -- =========================================================================
  -- §3.1 Introduction — flights を見る
  -- =========================================================================
  sect "flights (336,776 行・先頭のみ表示)"
  print (DF.dimensions flights)
  print (flights |> DF.take 10)

  -- R: glimpse(flights) — 各列の型と先頭値を縦に並べる。 dataframe の対応は
  -- describeColumns (列名・型・件数・欠損数の一覧)。
  sect "glimpse(flights) 相当 = describeColumns"
  print (DF.describeColumns flights)

  -- =========================================================================
  -- §3.2 Rows — filter() / arrange() / distinct()
  -- =========================================================================

  -- R: flights |> filter(dep_delay > 120)
  --   dep_delay はキャンセル便で欠損 (Maybe Int)。 R の filter は NA を FALSE 扱いで
  --   落とすので、 filterJust で欠損を除いてから比較すれば同じ結果になる。
  sect "filter(dep_delay > 120)"
  let bigDelays = flights |> DF.filterJust "dep_delay"
                          |> DF.filterWhere (F.col @Int "dep_delay" .> (120 :: DF.Expr Int))
  print (DF.dimensions bigDelays)
  print (bigDelays |> DF.take 10)

  -- R: flights |> filter(month == 1 & day == 1)
  sect "filter(month == 1 & day == 1)"
  let jan1 = flights |> DF.filterWhere
               (F.col @Int "month" .== (1 :: DF.Expr Int)
                  .&& F.col @Int "day" .== (1 :: DF.Expr Int))
  print (DF.dimensions jan1)
  print (jan1 |> DF.take 10)

  -- R: flights |> filter(month == 1 | month == 2)
  sect "filter(month == 1 | month == 2)"
  let janFeb = flights |> DF.filterWhere
                 (F.col @Int "month" .== (1 :: DF.Expr Int)
                    .|| F.col @Int "month" .== (2 :: DF.Expr Int))
  print (DF.dimensions janFeb)

  -- R: flights |> filter(month %in% c(1, 2))  ← | と == の近道
  --   dataframe には %in% は無いので filterBy で `elem` を使う (= 同値)。
  sect "filter(month %in% c(1, 2))"
  let janFeb2 = flights |> DF.filterBy (`elem` [1, 2]) (F.col @Int "month")
  print (DF.dimensions janFeb2)

  -- R: jan1 <- flights |> filter(month == 1 & day == 1)  ← 代入は表示されない。
  --   Haskell も let 束縛は表示されない (上の jan1 がそれ)。
  --
  -- ★Common mistakes (R4DS §3.2.2):
  --   ・filter(month = 1) は R では `=` 誤用でエラー。 Haskell は比較が `.==`、
  --     代入が無いので構文上ありえない (型/構文エラーで出荷前に弾かれる)。
  --   ・filter(month == 1 | 2) は R で "動くが意図と違う"。 Haskell では
  --     `.==` の結果 (Expr Bool) と `2` (Expr Int) を `.||` できず型エラーになる
  --     ので、 そもそも書けない (R の落とし穴をコンパイラが防ぐ)。

  -- R: flights |> arrange(year, month, day, dep_time)
  --   ★dataframe は欠損列を「基底型 (Int) のベクタ + null bitmap」で格納するため、
  --   sort の型注釈は基底型 @Int を使う (@(Maybe Int) だと型不一致で並べ替えが効かない)。
  --   ★相違: R は NA を常に末尾に置くが、 dataframe の null slot は基底既定値として並ぶ。
  --   値のある行の順序は一致 (先頭は day 1 の早朝便)。
  sect "arrange(year, month, day, dep_time)"
  let byDep = flights |> DF.sortBy
                [ DF.Asc (F.col @Int "year"),  DF.Asc (F.col @Int "month")
                , DF.Asc (F.col @Int "day"),   DF.Asc (F.col @Int "dep_time") ]
  print (byDep |> DF.take 10)

  -- R: flights |> arrange(desc(dep_delay))  ← 最も遅れた便が先頭
  sect "arrange(desc(dep_delay))"
  let worst = flights |> DF.sortBy [ DF.Desc (F.col @Int "dep_delay") ]
  print (worst |> DF.select ["year","month","day","dep_delay","carrier","flight"] |> DF.take 10)

  -- R: flights |> distinct()  ← 完全重複行の除去 (flights には無いので 336,776 のまま)
  --   ★dataframe 1.3 の distinct は NA を含む列があると `fromMaybeVec: Nothing slot`
  --   でクラッシュする (= 全列 distinct はこの版では不可)。 NA 無しの部分列 distinct
  --   (origin, dest) は下のとおり動く。 → README の LIMITATIONS 参照。

  -- R: flights |> distinct(origin, dest)
  sect "distinct(origin, dest)"
  let routes = flights |> DF.select ["origin", "dest"] |> DF.distinct
  print (DF.dimensions routes)
  print (routes |> DF.take 10)

  -- R: flights |> count(origin, dest, sort = TRUE)
  sect "count(origin, dest, sort = TRUE)"
  let routeCounts = flights |> DF.groupBy ["origin", "dest"]
                            |> DF.aggregate [ F.countAll `F.as` "n" ]
                            |> DF.sortBy [ DF.Desc (F.col @Int "n") ]
  print (routeCounts |> DF.take 10)

  -- =========================================================================
  -- §3.3 Columns — mutate() / select() / rename() / relocate()
  -- =========================================================================

  -- R: mutate(gain = dep_delay - arr_delay, speed = distance / air_time * 60)
  --   両辺 Maybe を nullLift2 で NA 伝播 (= R の算術)。 行は落とさない。
  let gainE  = F.nullLift2 (\d a -> d - a)
                 (F.col @(Maybe Int) "dep_delay") (F.col @(Maybe Int) "arr_delay")
                 `F.as` "gain"
      speedE = F.nullLift2 (\d a -> fromIntegral d / fromIntegral a * 60 :: Double)
                 (F.col @Int "distance") (F.col @(Maybe Int) "air_time")
                 `F.as` "speed"
  sect "mutate(gain, speed)"
  let withGain = flights |> DF.deriveMany [gainE, speedE]
  print (withGain |> DF.select ["carrier", "distance", "air_time", "gain", "speed"]
                  |> DF.take 10)

  -- R: mutate(..., .before = 1)  ← 新列を左端に
  sect "mutate(gain, speed, .before = 1)"
  print (withGain |> DF.select (["gain", "speed"] <> cols) |> DF.take 10)

  -- R: mutate(..., .after = day)  ← 新列を day の直後に
  sect "mutate(gain, speed, .after = day)"
  print (withGain |> DF.select (insertAfter "day" ["gain", "speed"] cols) |> DF.take 10)

  -- R: mutate(gain, hours = air_time/60, gain_per_hour = gain/hours, .keep = "used")
  --   .keep="used" は計算に関与した列 + 新列のみ残す。
  sect "mutate(gain, hours, gain_per_hour, .keep = \"used\")"
  let used = flights |> DF.deriveMany
               [ gainE
               , F.nullLift (\a -> fromIntegral a / 60 :: Double) (F.col @(Maybe Int) "air_time")
                   `F.as` "hours" ]
               |> DF.deriveMany
               [ F.nullLift2 (\g h -> fromIntegral g / h :: Double)
                   (F.col @(Maybe Int) "gain") (F.col @(Maybe Double) "hours")
                   `F.as` "gain_per_hour" ]
               |> DF.select ["dep_delay", "arr_delay", "air_time", "gain", "hours", "gain_per_hour"]
  print (used |> DF.take 10)

  -- R: select(year, month, day)
  sect "select(year, month, day)"
  print (flights |> DF.select ["year", "month", "day"] |> DF.take 5)

  -- R: select(year:day)  ← 範囲選択
  sect "select(year:day)"
  print (flights |> DF.selectBy [ DF.byNameRange ("year", "day") ] |> DF.take 5)

  -- R: select(!year:day)  ← 範囲の除外
  sect "select(!year:day)"
  print (flights |> DF.exclude ["year", "month", "day"] |> DF.take 5)

  -- R: select(where(is.character))  ← 文字列型の列のみ
  sect "select(where(is.character))"
  let isChar c = "Text" `T.isInfixOf` T.pack (columnTypeString c)
  print (flights |> DF.selectBy [ DF.byProperty isChar ] |> DF.take 5)

  -- R: select(tail_num = tailnum)  ← 選びつつ改名
  sect "select(tail_num = tailnum)"
  print (flights |> DF.select ["tailnum"] |> DF.rename "tailnum" "tail_num" |> DF.take 5)

  -- R: flights |> rename(tail_num = tailnum)  ← 他列を保ったまま改名
  sect "rename(tail_num = tailnum)"
  print (flights |> DF.rename "tailnum" "tail_num" |> DF.take 5)

  -- R: flights |> relocate(time_hour, air_time)  ← 既定は先頭へ移動
  sect "relocate(time_hour, air_time)"
  let relCols = ["time_hour", "air_time"] <> filter (`notElem` ["time_hour", "air_time"]) cols
  print (flights |> DF.select relCols |> DF.take 5)

  -- R: flights |> relocate(starts_with("arr"), .before = dep_time)
  sect "relocate(starts_with(\"arr\"), .before = dep_time)"
  let arrCols   = filter ("arr" `T.isPrefixOf`) cols
      rest      = filter (`notElem` arrCols) cols
      (a0, b0)  = break (== "dep_time") rest
      relBefore = a0 <> arrCols <> b0
  print (flights |> DF.select relBefore |> DF.take 5)

  -- =========================================================================
  -- §3.4 The pipe — 複数動詞の連結
  -- =========================================================================

  -- R: flights |> filter(dest == "IAH") |> mutate(speed = ...) |>
  --      select(year:day, dep_time, carrier, flight, speed) |> arrange(desc(speed))
  sect "pipe: IAH 行きの最速便"
  let toIAH = flights |> DF.filterWhere (F.col @Text "dest" .== F.lit ("IAH" :: Text))
                      |> DF.deriveMany [speedE]
                      |> DF.selectBy [ DF.byNameRange ("year", "day")
                                     , DF.byName "dep_time", DF.byName "carrier"
                                     , DF.byName "flight",   DF.byName "speed" ]
                      |> DF.sortBy [ DF.Desc (F.col @Double "speed") ]
  print (toIAH |> DF.take 10)
  -- ★R4DS の nested 版・中間オブジェクト版は同じ結果。 pipe が一番読みやすい。

  -- =========================================================================
  -- §3.5 Groups — group_by() / summarize() / slice_*()
  -- =========================================================================

  -- R: flights |> group_by(month) |> summarize(avg_delay = mean(dep_delay))
  --   ★na.rm なしだと R は全月 NA になる (欠損が混ざるため)。 dataframe の
  --   meanMaybe は欠損を無視する (= na.rm=TRUE 相当) ので、 下の na.rm=TRUE 版と
  --   同じ結果になる。 「na.rm 無し → 全 NA」 は R の挙動として本文で説明する。

  -- R: summarize(avg_delay = mean(dep_delay, na.rm = TRUE), n = n())
  --   ★dataframe 1.3 の grouped meanMaybe は欠損 slot を 0 として混ぜ平均が下振れする
  --   (内部バグ)。 回避: na.rm 相当に filterJust してから F.mean で平均を取り、 件数 n は
  --   全行で別集計し innerJoin で結合する (= R の na.rm=TRUE・n() と一致)。
  sect "group_by(month) |> summarize(avg_delay, n)"
  let avgByMonth = flights |> DF.filterJust "dep_delay" |> DF.groupBy ["month"]
                           |> DF.aggregate [ F.mean (F.col @Int "dep_delay") `F.as` "avg_delay" ]
      nByMonth   = flights |> DF.groupBy ["month"]
                           |> DF.aggregate [ F.countAll `F.as` "n" ]
      byMonth    = DF.innerJoin ["month"] avgByMonth nByMonth
                     |> DF.sortBy [ DF.Asc (F.col @Int "month") ]
  print byMonth

  -- R: flights |> group_by(dest) |> slice_max(arr_delay, n = 1) |> relocate(dest)
  --   各 dest の arr_delay 最大行 (タイは全部残す = with_ties)。 dataframe には
  --   slice_max が無いので「dest ごとの最大値」 を出して innerJoin で復元する
  --   (= タイ保持つき slice_max と同値)。 R4DS と同じく 105 dest で 108 行になる。
  sect "group_by(dest) |> slice_max(arr_delay, n = 1)"
  let arrNN   = flights |> DF.filterJust "arr_delay"
      destMax = arrNN |> DF.groupBy ["dest"]
                      |> DF.aggregate [ F.maximum (F.col @Int "arr_delay") `F.as` "arr_delay" ]
      tied    = DF.innerJoin ["dest", "arr_delay"] arrNN destMax   -- 各 dest の最大行 (タイ保持)
      -- ★R の slice_max は既定 na_rm = FALSE。 arr_delay が全 NA の dest (= LGA・キャンセル
      --   1 便) も NA 行を残すので、 destMax に現れない dest の行を加えて R と同じ 108 行にする。
      withMaxDests = DF.columnAsList (F.col @Text "dest") destMax
      allDests     = DF.columnAsList (F.col @Text "dest")
                       (flights |> DF.select ["dest"] |> DF.distinct)
      naDests      = filter (`notElem` withMaxDests) allDests
      naRows       = flights |> DF.filterBy (`elem` naDests) (F.col @Text "dest")
      sliceMax     = tied <> naRows
      relSlice     = ["dest"] <> filter (/= "dest") (DF.columnNames sliceMax)
  print (DF.dimensions sliceMax)
  print (sliceMax |> DF.select relSlice |> DF.take 10)

  -- R: daily <- flights |> group_by(year, month, day); daily |> summarize(n = n())
  --   ★複数変数 group_by の summarize は最後の群を 1 つ剥がす (dplyr のメッセージ)。
  --   dataframe は群を都度明示するのでメッセージは出ない。 結果は 365 行。
  sect "group_by(year, month, day) |> summarize(n)"
  let daily = flights |> DF.groupBy ["year", "month", "day"]
                      |> DF.aggregate [ F.countAll `F.as` "n" ]
                      |> DF.sortBy [ DF.Asc (F.col @Int "month"), DF.Asc (F.col @Int "day") ]
  print (DF.dimensions daily)
  print (daily |> DF.take 10)

  -- R: flights |> summarize(delay = mean(dep_delay, na.rm=TRUE), n = n(), .by = month)
  --   .by は per-operation grouping。 dataframe では groupBy + aggregate と同じ。
  sect ".by = c(origin, dest)"
  let avgByOD = flights |> DF.filterJust "dep_delay" |> DF.groupBy ["origin", "dest"]
                        |> DF.aggregate [ F.mean (F.col @Int "dep_delay") `F.as` "delay" ]
      nByOD   = flights |> DF.groupBy ["origin", "dest"]
                        |> DF.aggregate [ F.countAll `F.as` "n" ]
      byOD    = DF.innerJoin ["origin", "dest"] avgByOD nByOD
  print (DF.dimensions byOD)
  print (byOD |> DF.take 10)

  -- =========================================================================
  -- §3.6 Case study: aggregates and sample size  (★この章で唯一の図)
  -- =========================================================================

  -- R: batters <- Lahman::Batting |> group_by(playerID) |>
  --      summarize(performance = sum(H)/sum(AB), n = sum(AB))
  batting <- DF.readCsv "../_data/_raw/batting.csv"
  let batters = batting |> DF.groupBy ["playerID"]
                        |> DF.aggregate [ F.sum (F.col @Int "AB") `F.as` "n"
                                        , F.sum (F.col @Int "H")  `F.as` "hits" ]
                        |> DF.derive "performance"
                             (F.toDouble (F.col @Int "hits") / F.toDouble (F.col @Int "n"))
  sect "batters = Batting |> group_by(playerID) |> summarize(performance, n)"
  print (DF.dimensions batters)
  print (batters |> DF.select ["playerID", "performance", "n"] |> DF.take 10)

  -- R: batters |> filter(n > 100) |> ggplot(aes(n, performance)) +
  --      geom_point(alpha = 1/10) + geom_smooth(se = FALSE)
  let battersBig = batters |> DF.filterWhere (F.col @Int "n" .> (100 :: DF.Expr Int))
  sect "batters |> filter(n > 100)  → batters.svg"
  print (DF.dimensions battersBig)
  saveSVGBoundStats "batters.svg" $
    battersBig |>> theme ThemeGrey <> layer (scatter "n" "performance" <> color (fromHex "#000000") <> alpha 0.1)
               <> layer (statSmooth "n" "performance" 8 <> color (fromHex "#3366FF"))
               <> xLabel "n" <> yLabel "performance"

  -- R: batters |> arrange(desc(performance))
  --   ★打数が少ない選手が打率上位に来る (= 集計に件数を添える教訓)。
  sect "batters |> arrange(desc(performance)) の上位"
  print (batters |> DF.select ["playerID", "performance", "n"]
                 |> DF.sortBy [ DF.Desc (F.col @Double "performance") ] |> DF.take 10)

  putStrLn "\nwrote batters.svg"
