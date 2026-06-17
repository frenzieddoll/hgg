# 07. データの読み込み (R4DS 2e Ch.7 "Data import")

> 一次情報: **R for Data Science 2e, Ch.7 "Data import"**
> <https://r4ds.hadley.nz/data-import>
> データ: R4DS リポジトリの **`data/students.csv`**・**`data/0{1,2,3}-sales.csv`**
> (<https://github.com/hadley/r4ds/tree/main/data> から取得、 [`data/`](data/) に同梱)。

R4DS 第 7 章は **readr**(`read_csv` 系)で平文の矩形ファイルを R に読み込む章です。
図はテーブル 1 枚のみ(プロット図なし)。 ここでは **readr の各機能 → 本プロジェクトが
使う `dataframe` パッケージの API** に忠実対応づけます。 実行コードは
[`DataImport.hs`](DataImport.hs)、 実行は:

```sh
cd docs/tutorials/07-data-import
cabal run tut-07-data-import
```

★`dataframe` の挙動が readr と食い違う箇所は **近似でごまかさず、 実測した差異を honest に
記録** します(末尾「近似せず記録した相違」)。 `dataframe` に無い機能(`skip` / `comment` /
factor 型 / parquet 書込)は前処理や対応表で補います。

---

## §7.2 ファイルから読む(Reading data from a file)

R の `read_csv("data/students.csv")` は `dataframe` の `readCsv` に対応します。 読み込むと
列名と推論された型(= readr の col spec)が決まります。 `students.csv` の 1 行目は BOM 付きで、
列名に空白(`Student ID`)・大文字・ドット(`favourite.food`)が混じり、 `AGE` には空欄と
`five` が含まれます。

```haskell
students0 <- DF.readCsv "data/students.csv"   -- = read_csv(...)
```

```
Student ID | Full Name | favourite.food | mealPlan | AGE
   Int     |   Text    |   Maybe Text   |   Text   | Maybe Text
1          | Sunil …   | Just "Straw…"  | Lunch …  | Just "4"
3          | Jayendra… | Nothing        | Break…   | Just "7"
4          | Leon …    | Just "Anch…"   | Lunch …  | Nothing
5          | Chidiegwu | Just "Pizza"   | Break…   | Just "five"
```

BOM は自動除去されます。 `AGE` は `five` が混じるため数値化されず `Maybe Text` のまま
(R でも同じ理由で character)。

### §7.2.1 実務的な調整(Practical advice)

**欠損値の指定(`na`)**: R は既定で `""` のみを `NA` とみなすので、 文字列 `N/A` も欠損に
したい場合 `na = c("N/A", "")` を渡します。 `dataframe` では `ReadOptions` の
`missingIndicators` に渡します:

```haskell
students <- DF.readCsvWithOpts
              DF.defaultReadOptions { DF.missingIndicators = ["N/A", ""] }
              "data/students.csv"
```

> ★相違: この版の `dataframe` は **既定で `N/A` も欠損扱い** するため、 上の `na` 指定の
> 前後で結果は変わりません(`favourite.food` の `N/A` は既定読み込みの時点で `Nothing`)。
> R は既定が `""` のみなのでこの指定が効きます。 API 対応として `missingIndicators` を示します。

**非構文名の整形(= `rename` / `janitor::clean_names()`)**: 空白やドットを含む列名を
snake_case に直します:

```haskell
renamed = students |> DF.renameMany
  [ ("Student ID", "student_id"), ("Full Name", "full_name")
  , ("favourite.food", "favourite_food"), ("mealPlan", "meal_plan"), ("AGE", "age") ]
```

**型の修正(= `factor` / `parse_number(if_else(...))`)**: `age` の `five` を `5` に直し、
数値列にします:

```haskell
fixAge Nothing       = Nothing
fixAge (Just "five") = Just 5
fixAge (Just t)      = readMaybe (T.unpack t)   -- = parse_number(if_else(age=="five","5",age))

cleaned = renamed |> DF.apply fixAge "age"      -- age :: Maybe Int になる
```

> ★相違: この版の `dataframe` には R の **factor(`<fct>`)に当たる独立の型が無い**ため、
> `meal_plan = factor(meal_plan)` は再現せず `Text` のままにしています(順序つき水準は
> 後の章のトピック)。

### §7.2.3 その他の引数(Other arguments)

readr は文字列をそのまま CSV として読めます(`read_csv("a,b,c\n1,2,3")`)。 `dataframe` は
FilePath からのみ読むので、 **文字列を一時ファイルに書いてから読む** ヘルパ `readInline` で
同じことを示します。

| R | dataframe |
|---|---|
| `read_csv("a,b,c\n1,2,3\n4,5,6")` | `readInline defaultReadOptions "a,b,c\n1,2,3\n4,5,6\n"` |
| `read_csv(…, skip = 2)` | **前処理**で先頭 2 行を捨てて読む(`readSkip`、 下記★) |
| `read_csv(…, comment = "#")` | **前処理**で `#` 行を捨てて読む(`readComment`、 下記★) |
| `read_csv(…, col_names = FALSE)` | `defaultReadOptions { headerSpec = NoHeader }` |
| `read_csv(…, col_names = c("x","y","z"))` | `… { headerSpec = ProvideNames ["x","y","z"] }` |

> ★相違: `dataframe` の `ReadOptions` には **`skip` / `comment` に当たる引数が無い** ため、
> 行を前処理で落としてから読む小ヘルパで補いました(機能を実装で埋めた honest な対応)。
>
> ★相違: `col_names = FALSE` のとき、 R は列を `X1`, `X2`, … と名付けますが、 `dataframe` は
> `"0"`, `"1"`, `"2"` と **0 始まりの連番**を付けます。

### §7.2.4 他のファイル形式(Other file types)

readr の `read_csv2`(`;`)・`read_tsv`(タブ)・`read_delim`(任意区切り)は **区切り文字違い**
です。 `dataframe` では `ReadOptions` の `columnSeparator` を変えるだけで対応できます。

| R | dataframe |
|---|---|
| `read_csv2()`(`;` 区切り) | `defaultReadOptions { columnSeparator = ';' }` |
| `read_tsv()`(タブ区切り) | `… { columnSeparator = '\t' }` |
| `read_delim(delim = "\|")` | `… { columnSeparator = '\|' }` |
| `read_fwf()` / `read_table()` / `read_log()` | **未対応**(固定幅・空白区切り・Apache ログ専用パーサは `dataframe` に無い) |

---

## §7.3 列型の制御(Controlling column types)

CSV は列の型情報を持たないので、 readr も `dataframe` も値から型を推測します。

### §7.3.1 型推論(Guessing types)

R4DS の例(logical / numeric / date / string の 4 列)を読むと、 推論結果が出ます:

```
logical | numeric |    date    | string
 Text   |  Text   |    Day     |  Text
TRUE    | 1       | 2021-01-15 | abc
T       | Inf     | 2021-02-16 | ghi
```

> ★相違(実測): `dataframe` の推論は readr と挙動が異なります。
> - **logical 列**(`TRUE`/`false`/`T`)は readr では logical になりますが、 `dataframe` は
>   `Text` のままです(真偽値への自動推論をしない)。
> - **numeric 列**は `Inf` を含むため `dataframe` では数値と認識されず `Text` になります
>   (readr は `Inf` を有効な数値として double に推論)。
> - **date 列**(ISO8601)は `dataframe` も正しく `Day` 型に推論します。

### §7.3.2 欠損が型推論を壊す(Missing values, column types, and problems)

`.` で欠損を表す 1 列 CSV を既定で読むと、 `.` のせいで数値列にならず `Text` になります
(R4DS と同じ挙動)。 `na = "."` を指定すると `.` が欠損になり、 数値列として推論されます:

```haskell
simpleNa <- readInline DF.defaultReadOptions { DF.missingIndicators = ["."] } "x\n10\n.\n20\n30\n"
-- x :: Maybe Int = [Just 10, Nothing, Just 20, Just 30]
```

> R は「数値だと宣言 → `problems()` で失敗箇所を特定 → `na` を直す」という流れですが、
> `dataframe` には `problems()` 相当が無いので、 ここでは `na` の指定で数値化される所まで示します。

### §7.3.3 列型の明示(Column types)

readr の 9 つの列型は `dataframe` の `SchemaType`(`schemaType @Int` 等)や `ReadOptions` の
`typeSpec` に対応します。

| readr | dataframe |
|---|---|
| `col_logical()` / `col_double()` / `col_integer()` | 推論に任せる / `typeSpec = SpecifyTypes [(col, schemaType @Double)] …`(`@Int` 等) |
| `col_character()`(数値 ID 等) | `schemaType @Text`(先頭ゼロ ID の保持に有用) |
| `col_factor()` / `col_date()` / `col_datetime()` | factor は型無し(§7.2.1)、 date/datetime は `Day` 等に推論 |
| `col_skip()` | 読み込み後に `select` で外す(列を読まない指定は無い) |
| `cols(.default = col_character())` | `typeSpec = NoInference`(全列を文字列のまま読む) |
| `cols_only(x = …)` | 読み込み後に `select ["x"]` |

```haskell
-- cols(.default = col_character()) 相当 → 全列 Text
allChar <- readInline DF.defaultReadOptions { DF.typeSpec = DF.NoInference } "x,y,z\n1,2,3\n"
```

---

## §7.4 複数ファイルの読み込み(Reading data from multiple files)

R: `read_csv(sales_files, id = "file")` は複数 CSV を **縦に積み**、 `file` 列で出所を残します。
`dataframe` には縦結合 1 関数が無いので、 **各列をリスト化して連結し再構築** します(honest な補い)。
ファイル探索は R の `list.files(pattern = ...)` を `listDirectory` + 接尾辞フィルタで再現:

```haskell
entries <- listDirectory "data"
let salesFiles = sort [ "data/" ++ f | f <- entries, "sales.csv" `isSuffixOf` f ]

stacked <- stackSalesFiles salesFiles   -- file 列を足して縦積み
```

```
      file        |  month   | year | brand | item |  n
data/01-sales.csv | January  | 2019 | 1     | 1234 | 3
data/02-sales.csv | February | 2019 | 1     | 1234 | 8
data/03-sales.csv | March    | 2019 | 2     | 8288 | 6
```

3 ファイル(7 + 6 + 6 行)が **19 行**に積まれ、 `file` 列で出所を辿れます(R4DS と同じ 19 行)。

---

## §7.5 ファイルへの書き出し(Writing to a file)

| R | dataframe |
|---|---|
| `write_csv(x, "f.csv")` | `DF.writeCsv "f.csv" x`(★欠損列は不可、 下記) |
| `write_tsv(x, "f.tsv")` | `writeSeparated`(区切り指定) |
| `write_rds()` / `read_rds()` | R 固有(RDS バイナリ)。 Haskell の等価は独自バイナリ直列化 |
| `write_parquet()` / `read_parquet()` | **読込は対応**(`readParquet`)。 書込は umbrella 非公開 |

```haskell
DF.writeCsv "students-clean.csv" writable   -- = write_csv(...)
roundTrip <- DF.readCsv "students-clean.csv" -- 読み戻すと型情報は失われる (R4DS の指摘どおり)
```

CSV で保存すると **型情報が失われる**(読み戻すと改めて推論)ため、 中間キャッシュには不向き
です。 R は代替として RDS / parquet を挙げます。 `dataframe` は **parquet の読み込みに対応**
(`readParquet`)していますが、 書き出しは umbrella から公開されていないため、 ここでは
対応表で示すにとどめます。

> ★相違: この版の `writeCsv` は欠損(`Nothing`)を含む列を直列化できず落ちるため、 §7.5 の
> デモは欠損なしの 3 列(`student_id` / `full_name` / `meal_plan`)だけを書き出しています。

---

## §7.6 手組み(Data entry)

R: `tibble()` は列ごと、 `tribble()` は行ごとにデータを書き下します。 `dataframe` では
`fromNamedColumns [(name, fromList xs)]` で列ごとに組みます:

```haskell
-- tibble(x = c(1,2,5), y = c("h","m","g"), z = c(0.08,0.83,0.60))
byCol = DF.fromNamedColumns
  [ ("x", DF.fromList ([1,2,5] :: [Int]))
  , ("y", DF.fromList (["h","m","g"] :: [Text]))
  , ("z", DF.fromList ([0.08,0.83,0.60] :: [Double])) ]
```

> ★相違: Haskell には `tribble`(行レイアウトで書く糖衣)の専用構文は無いので、 行タプルの
> リストを書いて `unzip3` で列に組み替えれば「行ごとに読みやすく並べる」意図を同じに表せます。

```haskell
let rows = [ (1 :: Int, "h" :: Text, 0.08 :: Double), (2, "m", 0.83), (5, "g", 0.60) ]
    (xs, ys, zs) = unzip3 rows
    byRow = DF.fromNamedColumns [ ("x", DF.fromList xs), ("y", DF.fromList ys), ("z", DF.fromList zs) ]
```

---

## 近似せず記録した相違(まとめ)

- **既定の欠損指定**: `dataframe` は既定で `N/A` も欠損扱い(R は既定 `""` のみ)。 §7.2.1 の
  `na = c("N/A","")` は前後で結果が変わらない。
- **factor 型なし**: R の `<fct>` に当たる独立型が無く、 `meal_plan` は `Text` のまま。
- **`skip` / `comment` 引数なし**: 行を前処理で落として補った。
- **`col_names = FALSE` の命名**: R は `X1..Xn`、 `dataframe` は `"0".."n-1"`。
- **型推論の差**: logical(`T`/`F`)は `Text` のまま・`Inf` を含む数値列は `Text` 化(date は
  正しく `Day`)。 readr の推論より保守的。
- **`problems()` なし**: 型宣言の失敗箇所を一覧する関数が無い。
- **縦結合 1 関数なし**: 複数ファイルの行積みは列リスト連結 + `fromNamedColumns` で実装。
- **`writeCsv` の欠損列制限**: 欠損を含む列は直列化できない。
- **parquet 書込**: 読込(`readParquet`)は可、 書込は umbrella 非公開。
- **固定幅 / ログ形式**: `read_fwf` / `read_table` / `read_log` 相当は未対応。
- **データ出所**: `students.csv`・`0{1,2,3}-sales.csv` は R4DS リポジトリの `data/` から取得
  (捏造なし)。
