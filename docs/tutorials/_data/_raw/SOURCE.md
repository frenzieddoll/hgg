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
| babynames.csv | github hadley/babynames data/babynames.rda (= US SSA 全国データ 1880-2017) | 1924665 | Ch14 Strings |
| words.csv     | github tidyverse/stringr data/words.rda (= stringr `words`・常用語) | 980 | Ch15 Regexps |
| sentences.csv | github tidyverse/stringr data/sentences.rda (= stringr `sentences`・Harvard sentences) | 720 | Ch15 Regexps |
| fruit.csv     | github tidyverse/stringr data/fruit.rda (= stringr `fruit`) | 80 | Ch15 Regexps |
| gss_cat.csv   | github tidyverse/forcats data/gss_cat.rda (= forcats `gss_cat`・General Social Survey 抽出) | 21483 | Ch16 Factors |

注: penguins.csv 等の先頭 `rownames` 列は Rdatasets が付与したもの。各章で読み込み時に exclude する。

stringr の words/sentences/fruit は小サイズゆえ **repo 追跡** (babynames と違い .gitignore しない)。
取得 = `tidyverse/stringr` の `data/{words,sentences,fruit}.rda` (★**bzip2 圧縮** RDX2・babynames の
XZ とは別) を Python の R serialization parser (`scripts/gen-stringr-data-csv.py`・圧縮自動判定) で
単一文字ベクトルとして展開。検証済: 件数 980/720/80・words[1]="a"・fruit に "apple"・
sentences[1]="The birch canoe slid on the smooth planks." が R4DS / stringr と一致。捏造なし。
flights.csv (32MB) は repo に含めない (.gitignore)。各章で小スライスを作って章 dir に置く。

babynames.csv (53MB) も repo に含めない (.gitignore・full local 読み)。取得 = `hadley/babynames`
の `data/babynames.rda` (XZ 圧縮 RDX2) を Python の R serialization parser で `year,sex,name,n,prop`
に展開 (= R `babynames` パッケージそのもの・SSA 全国データ 1880-2017)。検証済: 行数 1924665・
1880 F Mary n=7065・§14.5.1 の `str_length(name)==15` 上位 = Franciscojavier/Johnchristopher/
Christopherjohn/Christopherjame/Christophermich が R4DS と一致。捏造なし。

gss_cat.csv (21,483 行 × 9 列) = R `forcats` パッケージの `data/gss_cat.rda` (RDX2) を
Python の R serialization parser で展開 (General Social Survey 抽出・Phase 28 Ch16 Factors 用)。
★babynames/stringr と違い **factor 列** (marital/race/rincome/partyid/relig/denom) を含むため、
INTSXP の `levels` 属性を読んでラベル復元する拡張 parser (`scripts/gen-gsscat-csv.py`) を使用。
罠: CHARSXP は R の ref テーブルに積まれない (SYMSXP/ENVSXP のみ AddReadRef) ので、CHARSXP を
refs に入れると 2 個目以降の factor の levels/class タグ参照がずれる (= marital だけラベル化
された症状の真因・修正済)。検証済: 行数 21,483・relig 最多 = Protestant (10846)・各 factor の
levels 順が R4DS §16.4 と一致。捏造なし。 ★gss_cat は ~1.7MB ゆえ **repo 追跡** (.gitignore しない)。

factor levels (一次根拠・gss_cat 固有の定義順序):
- marital (6): No answer / Never married / Separated / Divorced / Widowed / Married
- race (4): Other / Black / White / Not applicable
- rincome (16): No answer / Don't know / Refused / $25000 or more / $20000 - 24999 /
  $15000 - 19999 / $10000 - 14999 / $8000 to 9999 / $7000 to 7999 / $6000 to 6999 /
  $5000 to 5999 / $4000 to 4999 / $3000 to 3999 / $1000 to 2999 / Lt $1000 / Not applicable
- partyid (10): No answer / Don't know / Other party / Strong republican / Not str republican /
  Ind,near rep / Independent / Ind,near dem / Not str democrat / Strong democrat
- relig (16): No answer / Don't know / Inter-nondenominational / Native american / Christian /
  Orthodox-christian / Moslem/islam / Other eastern / Hinduism / Buddhism / Other / None /
  Jewish / Catholic / Protestant / Not applicable
- denom (30): No answer / Don't know / No denomination / Other / Episcopal / … / Not applicable
  (30 水準の全列挙は `scripts/gen-gsscat-csv.py` 実行出力を参照)

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
