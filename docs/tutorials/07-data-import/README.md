# 07. Data Import

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.7 "Data import"**
> <https://r4ds.hadley.nz/data-import>
> Data: **`data/students.csv`** and **`data/0{1,2,3}-sales.csv`** from the R4DS repository
> (obtained from <https://github.com/hadley/r4ds/tree/main/data>, included in [`data/`](data/)).

Chapter 7 of R4DS covers reading flat rectangular files into R using **readr** (`read_csv` family).
The chapter contains only one table figure (no plot diagrams). Here we map **readr's features to the
`dataframe` package API** used in this project, maintaining faithful correspondence. The complete
execution code is in [`DataImport.hs`](DataImport.hs); to run:

```sh
cd docs/tutorials/07-data-import
cabal run tut-07-data-import
```

Where `dataframe` behavior diverges from readr, we **record measured differences honestly without
approximation** (see "Differences recorded without approximation" at the end). Features missing from
`dataframe` (`skip` / `comment` / factor type / parquet write) are handled via preprocessing or
correspondence tables.

---

## §7.2 Reading from a file

R's `read_csv("data/students.csv")` corresponds to `dataframe`'s `readCsv`. Upon loading, the column
names and inferred types (equivalent to readr's col spec) are determined. The first line of
`students.csv` has a BOM; column names contain spaces (`Student ID`), capitals, and dots
(`favourite.food`); and the `AGE` column contains both empty cells and `five`.

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

The BOM is automatically removed. `AGE` remains `Maybe Text` because `five` is mixed in, preventing
numeric coercion (R exhibits the same behavior for this reason).

### §7.2.1 Practical advice

**Specifying missing values (`na`)**: R treats only `""` as `NA` by default, so if you want the string
`N/A` to be treated as missing, pass `na = c("N/A", "")`. In `dataframe`, pass it to the `ReadOptions`
field `missingIndicators`:

```haskell
students <- DF.readCsvWithOpts
              DF.defaultReadOptions { DF.missingIndicators = ["N/A", ""] }
              "data/students.csv"
```

> **Difference**: In this version of `dataframe`, **`N/A` is treated as missing by default**, so the
> result before and after the `na` specification above doesn't change (`favourite.food`'s `N/A` is
> already `Nothing` at default load). R's default only includes `""`; this specification takes effect
> there. We show `missingIndicators` for API correspondence.

**Reformatting non-syntactic names (= `rename` / `janitor::clean_names()`)**: Convert column names
containing spaces or dots to snake_case:

```haskell
renamed = students |> DF.renameMany
  [ ("Student ID", "student_id"), ("Full Name", "full_name")
  , ("favourite.food", "favourite_food"), ("mealPlan", "meal_plan"), ("AGE", "age") ]
```

**Fixing column types (= `factor` / `parse_number(if_else(...))`)**: Fix `age`'s `five` to `5` and
convert to a numeric column:

```haskell
fixAge Nothing       = Nothing
fixAge (Just "five") = Just 5
fixAge (Just t)      = readMaybe (T.unpack t)   -- = parse_number(if_else(age=="five","5",age))

cleaned = renamed |> DF.apply fixAge "age"      -- age :: Maybe Int
```

> **Difference**: This version of `dataframe` lacks an **independent type equivalent to R's factor
> (`<fct>`)**; we don't reproduce `meal_plan = factor(meal_plan)` and keep it as `Text` (ordered
> factor levels are a topic for later chapters).

### §7.2.3 Other arguments

readr can read a string directly as CSV (`read_csv("a,b,c\n1,2,3")`). `dataframe` reads only from
FilePath, so we use a helper `readInline` that **writes the string to a temporary file and reads it**
to demonstrate the same thing.

| R | dataframe |
|---|---|
| `read_csv("a,b,c\n1,2,3\n4,5,6")` | `readInline defaultReadOptions "a,b,c\n1,2,3\n4,5,6\n"` |
| `read_csv(…, skip = 2)` | **Preprocessing**: discard first 2 lines before reading (`readSkip`, see below★) |
| `read_csv(…, comment = "#")` | **Preprocessing**: discard lines starting with `#` (`readComment`, see below★) |
| `read_csv(…, col_names = FALSE)` | `defaultReadOptions { headerSpec = NoHeader }` |
| `read_csv(…, col_names = c("x","y","z"))` | `… { headerSpec = ProvideNames ["x","y","z"] }` |

> **Difference**: `dataframe`'s `ReadOptions` has **no arguments equivalent to `skip` / `comment`**,
> so we compensate with a small helper that drops rows via preprocessing (filling the gap with
> implementation, honestly).
>
> **Difference**: When `col_names = FALSE`, R names columns `X1`, `X2`, …, but `dataframe` uses
> **zero-indexed sequential names** `"0"`, `"1"`, `"2"`.

### §7.2.4 Other file formats

readr's `read_csv2` (`;` delimiter), `read_tsv` (tab delimiter), and `read_delim` (arbitrary delimiter)
differ only in **delimiter choice**. `dataframe` handles this by changing the `columnSeparator` field
in `ReadOptions`:

| R | dataframe |
|---|---|
| `read_csv2()` (`;` delimiter) | `defaultReadOptions { columnSeparator = ';' }` |
| `read_tsv()` (tab delimiter) | `… { columnSeparator = '\t' }` |
| `read_delim(delim = "\|")` | `… { columnSeparator = '\|' }` |
| `read_fwf()` / `read_table()` / `read_log()` | **Not supported** (fixed-width, whitespace-delimited, Apache log parsers are not in `dataframe`) |

---

## §7.3 Controlling column types

CSV files have no type information, so both readr and `dataframe` infer types from values.

### §7.3.1 Guessing types

Reading the R4DS example (4 columns: logical, numeric, date, string) shows inference results:

```
logical | numeric |    date    | string
 Text   |  Text   |    Day     |  Text
TRUE    | 1       | 2021-01-15 | abc
T       | Inf     | 2021-02-16 | ghi
```

> **Difference (measured)**: `dataframe`'s inference behaves differently from readr:
> - **Logical columns** (`TRUE`/`false`/`T`) become logical in readr, but `dataframe` keeps them as
>   `Text` (no automatic boolean inference).
> - **Numeric columns** containing `Inf` are recognized as `Text` in `dataframe`, not numeric
>   (readr treats `Inf` as a valid number and infers double).
> - **Date columns** (ISO8601) are correctly inferred as `Day` type by `dataframe`.

### §7.3.2 Missing values breaking type inference

A single-column CSV using `.` to denote missing values, read with default options, won't become
numeric—`.` prevents it, keeping it `Text` (same behavior as R4DS). Specifying `na = "."` makes `.`
missing, and the column is inferred as numeric:

```haskell
simpleNa <- readInline DF.defaultReadOptions { DF.missingIndicators = ["."] } "x\n10\n.\n20\n30\n"
-- x :: Maybe Int = [Just 10, Nothing, Just 20, Just 30]
```

> R follows the flow: "declare numeric → identify failures with `problems()` → fix `na`". `dataframe`
> lacks `problems()` equivalent, so we show up to where the `na` specification makes the column numeric.

### §7.3.3 Explicit column types

readr's 9 column types correspond to `dataframe`'s `SchemaType` (`schemaType @Int`, etc.) and the
`ReadOptions` field `typeSpec`.

| readr | dataframe |
|---|---|
| `col_logical()` / `col_double()` / `col_integer()` | Rely on inference / `typeSpec = SpecifyTypes [(col, schemaType @Double)] …` (`@Int`, etc.) |
| `col_character()` (numeric IDs, etc.) | `schemaType @Text` (useful for preserving leading zeros) |
| `col_factor()` / `col_date()` / `col_datetime()` | factor unsupported (§7.2.1); date/datetime inferred as `Day`, etc. |
| `col_skip()` | Read then drop with `select` (no "don't read" option) |
| `cols(.default = col_character())` | `typeSpec = NoInference` (read all columns as text) |
| `cols_only(x = …)` | Read then `select ["x"]` |

```haskell
-- cols(.default = col_character()) equivalent → all columns as Text
allChar <- readInline DF.defaultReadOptions { DF.typeSpec = DF.NoInference } "x,y,z\n1,2,3\n"
```

---

## §7.4 Reading data from multiple files

R's `read_csv(sales_files, id = "file")` **stacks multiple CSVs vertically**, keeping a `file` column
to track origin. `dataframe` has no single concat function, so we **list columns, concatenate, and
rebuild** (honestly filling the gap). File discovery uses `listDirectory` + suffix filter to recreate
R's `list.files(pattern = ...)`:

```haskell
entries <- listDirectory "data"
let salesFiles = sort [ "data/" ++ f | f <- entries, "sales.csv" `isSuffixOf` f ]

stacked <- stackSalesFiles salesFiles   -- add file column and stack vertically
```

```
      file        |  month   | year | brand | item |  n
data/01-sales.csv | January  | 2019 | 1     | 1234 | 3
data/02-sales.csv | February | 2019 | 1     | 1234 | 8
data/03-sales.csv | March    | 2019 | 2     | 8288 | 6
```

Three files (7 + 6 + 6 rows) are **stacked to 19 rows**, with the `file` column allowing tracing of
origin (19 rows, same as R4DS).

---

## §7.5 Writing to a file

| R | dataframe |
|---|---|
| `write_csv(x, "f.csv")` | `DF.writeCsv "f.csv" x` (★missing columns not supported, see below) |
| `write_tsv(x, "f.tsv")` | `writeSeparated` (delimiter specification) |
| `write_rds()` / `read_rds()` | R-specific (RDS binary). Haskell equivalent is custom binary serialization |
| `write_parquet()` / `read_parquet()` | **Read supported** (`readParquet`). Write not public from umbrella |

```haskell
DF.writeCsv "students-clean.csv" writable   -- = write_csv(...)
roundTrip <- DF.readCsv "students-clean.csv" -- Type info lost on read-back (as R4DS notes)
```

CSV storage **loses type information** (re-inferred on read-back), making it unsuitable for intermediate
caching. R suggests RDS / parquet as alternatives. `dataframe` **supports parquet reading**
(`readParquet`) but doesn't publicly expose writing, so we only note it in the correspondence table.

> **Difference**: This version's `writeCsv` cannot serialize columns containing missing values
> (`Nothing`) and crashes, so the §7.5 demo only writes 3 columns without missing values
> (`student_id` / `full_name` / `meal_plan`).

---

## §7.6 Data entry

R's `tibble()` builds data column-wise; `tribble()` builds row-wise. `dataframe` builds column-wise
using `fromNamedColumns [(name, fromList xs)]`:

```haskell
-- tibble(x = c(1,2,5), y = c("h","m","g"), z = c(0.08,0.83,0.60))
byCol = DF.fromNamedColumns
  [ ("x", DF.fromList ([1,2,5] :: [Int]))
  , ("y", DF.fromList (["h","m","g"] :: [Text]))
  , ("z", DF.fromList ([0.08,0.83,0.60] :: [Double])) ]
```

> **Difference**: Haskell lacks dedicated syntax for `tribble` (row-wise layout sugar). Writing a list
> of row tuples and unpacking with `unzip3` conveys the same "readable row-by-row layout" intent.

```haskell
let rows = [ (1 :: Int, "h" :: Text, 0.08 :: Double), (2, "m", 0.83), (5, "g", 0.60) ]
    (xs, ys, zs) = unzip3 rows
    byRow = DF.fromNamedColumns [ ("x", DF.fromList xs), ("y", DF.fromList ys), ("z", DF.fromList zs) ]
```

---

## Differences recorded without approximation (Summary)

- **Default missing value handling**: `dataframe` treats `N/A` as missing by default (R defaults to
  `""` only). The `na = c("N/A","")` case in §7.2.1 produces the same result before and after.
- **No factor type**: R's `<fct>` type is absent; `meal_plan` remains `Text`.
- **No `skip` / `comment` arguments**: Rows are dropped via preprocessing.
- **`col_names = FALSE` naming**: R uses `X1..Xn`; `dataframe` uses `"0".."n-1"`.
- **Type inference differences**: logical (`T`/`F`) stays `Text`; numeric columns with `Inf` become
  `Text` (date correctly becomes `Day`). More conservative than readr.
- **No `problems()` function**: No way to list column type declaration failures.
- **No single vertical concat function**: Multi-file row stacking implemented via column list
  concatenation + `fromNamedColumns`.
- **`writeCsv` missing column restriction**: Columns with missing values cannot serialize.
- **Parquet writing**: Reading (`readParquet`) supported; writing not public from umbrella.
- **Fixed-width / log formats**: `read_fwf` / `read_table` / `read_log` equivalents not supported.
- **Data provenance**: `students.csv` and `0{1,2,3}-sales.csv` obtained from R4DS repository's
  `data/` (not fabricated).
