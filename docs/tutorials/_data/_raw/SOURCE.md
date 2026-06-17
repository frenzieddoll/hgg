# チュートリアル実データの出所 (Phase 28・捏造禁止の根拠)

すべて Rdatasets (https://vincentarelbundock.github.io/Rdatasets/) から取得した公開実データ。
取得日: 2026-06-14。

| ファイル | Rdatasets パス | 行数 | R4DS 2e での用途 |
|---|---|---|---|
| penguins.csv  | csv/palmerpenguins/penguins.csv | 344 | Ch1 Data visualization |
| diamonds.csv  | csv/ggplot2/diamonds.csv        | 53940 | Ch10 EDA |
| airlines.csv  | csv/nycflights13/airlines.csv   | 16 | Ch19 Joins |
| airports.csv  | csv/nycflights13/airports.csv   | 1458 | Ch19 Joins |
| planes.csv    | csv/nycflights13/planes.csv     | 3322 | Ch19 Joins |
| weather.csv   | csv/nycflights13/weather.csv    | 26115 | Ch19 Joins |
| flights.csv   | csv/nycflights13/flights.csv    | 336776 | Ch3 Transform / Ch17 Datetimes (小スライスを作って使う) |
| Batting.csv   | csv/Lahman/Batting.csv          | 115450 | Ch3 Transform の打者成績図 (case study) |
| table1-4b.csv | csv/tidyr/table*.csv            | 各6前後 | Ch5 Tidy |
| billboard.csv | csv/tidyr/billboard.csv         | 317 | Ch5 Tidy (pivot_longer) |
| students.csv  | github hadley/r4ds data/students.csv | 6 | Ch7 Data import |

注: penguins.csv 等の先頭 `rownames` 列は Rdatasets が付与したもの。各章で読み込み時に exclude する。
flights.csv (32MB) は repo に含めない (.gitignore)。各章で小スライスを作って章 dir に置く。

## 派生スライス (Phase 28 各章で使用・実データの部分集合 / 列射影。値は不変)

| ファイル | 由来 | 作り方 | 用途 |
|---|---|---|---|
| flights-slice.csv | flights.csv | 全 12 月を保つ系統サンプル (160 行ごと = 2105 行)・rownames 列除去 | Ch3 Transform / Ch17 Datetimes |
| batting.csv | Lahman/Batting.csv | 実値の 3 列射影 (playerID, AB, H)・rownames 除去 (115450 行) | Ch3 Transform の打者成績図 |
| flights-dt-slice.csv | flights.csv | 全月保持の系統サンプル (20 行ごと = 16839 行)・必要列のみ | Ch17 Datetimes |

## R4DS 本文の小 tibble (Ch18 Missing values・本文の値をそのまま CSV 化)

| ファイル | 由来 | 用途 |
|---|---|---|
| treatment.csv | R4DS Ch18 tribble | fill / coalesce |
| stocks.csv | R4DS Ch18 tibble | pivot_wider / complete |
| health.csv | R4DS Ch18 tibble | factor 空グループ |
