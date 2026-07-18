# 19. Joins

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.19 "Joins"**
> <https://r4ds.hadley.nz/joins>
> Data: **nycflights13** 5 tables — `flights` (all 336,776 rows) / `airlines` / `airports` (1,458) / `planes` (3,322) / `weather` (26,115). Complete real data.

Learn methods to combine multiple dataframes by **key** (variable connecting tables). This chapter covers two join families:

- **mutating join** (`left_join` / `inner_join` / `right_join` / `full_join`): **Add variables** from matching observations.
- **filtering join** (`semi_join` / `anti_join`): **Filter rows** by match presence.

Finally, **non-equi join** (matching by operators other than `==`) is covered. Run code: [`Joins.hs`](Joins.hs).

```sh
cd docs/tutorials/19-joins
cabal run tut-19-joins
```

> **This chapter emphasizes table operations.** All R4DS Ch19 figures are **hand-drawn conceptual diagrams** (ER, dot plots, Venn diagrams) explaining joins; no ggplot figures from real data appear. This chapter explains concepts in prose and faithfully **reproduces table outputs with real data** per join type (conceptual diagrams are outside plot scope).

> **Why implement join ourselves?** Hackage `dataframe` has `leftJoin` / `innerJoin` etc., but they fold duplicate non-key columns into `These` type, require nullable keys for `fullOuterJoin`, and differ semantically from dplyr. To faithfully reproduce R4DS table output (disambiguating `year.x`/`year.y`, padding unmatched with `NA`), we self-implemented join in Haskell (CLAUDE.md principle: "fill gaps by implementing"). Split into index computation (`leftIdx`/`innerIdx`/`fullIdx`/`rightIdx`/`semiIdx`/`antiIdx`) and column re-selection (`pickJust`/`pickFlat`).

---

## 1. Keys

Joins always pair a **primary key** (variable uniquely identifying each observation) with a **foreign key** (pointing to it in another table). In nycflights13:

| Table | Primary key | Description |
|---|---|---|
| `airlines` | `carrier` | 2-letter airline code |
| `airports` | `faa` | 3-letter airport code |
| `planes` | `tailnum` | Aircraft tail number |
| `weather` | `origin` + `time_hour` | **Composite key** (location + time) |

Foreign key examples: `flights$tailnum` → `planes$tailnum`, `flights$carrier` → `airlines$carrier`, `flights$origin`/`flights$dest` → `airports$faa`.

### Verifying primary keys

Check if keys are truly unique by `count()` on key and filtering for `n > 1`:

| R | hgg |
|---|---|
| `planes \|> count(tailnum) \|> filter(n>1)` | Count duplicates |
| `weather \|> count(time_hour, origin) \|> filter(n>1)` | Composite key same way |

Both `planes` and `weather` have **0 duplicates** (valid primary keys). Missing keys also 0. However, "no duplicates" alone doesn't guarantee primary key status. For example, `airports`'s `(alt, lat)` has **1 duplicate**, unfit for primary key.

### Surrogate keys

`flights` has no primary key, but `time_hour`·`carrier`·`flight` together are unique (0 duplicates). Still, adding a simple **row-number surrogate key** is convenient:

| R | hgg |
|---|---|
| `flights \|> mutate(id = row_number(), .before = 1)` | `insertVector "id" [1..n]` + `select` to front |

---

## 2. Mutating join — `left_join`

Of 4 mutating joins, `left_join` is almost always used. Output always keeps **`x` (left table) rows**, appending **matching right variables to the right**. First, narrow to 6 columns for clarity:

```haskell
flights2 = DF.select ["year","time_hour","origin","dest","tailnum","carrier"] flights
```

### Add metadata

| R | hgg |
|---|---|
| `flights2 \|> left_join(airlines)` | `leftIdx (carrier) (carrier)` → insert `name` column |

Join on `carrier` adds airline name `name` to the right. Similarly add temperature/wind from `weather`, aircraft info from `planes`.

```
year  time_hour             origin dest tailnum  carrier  name
2013  2013-01-01T10:00:00Z  EWR    IAH  N14228   UA       United Air Lines Inc.
2013  2013-01-01T10:00:00Z  JFK    MIA  N619AA   AA       American Airlines Inc.
...
```

Unmatched rows get `NA` for new variables. For example, `tailnum == "N3ALAA"` isn't in `planes`, so `type`·`engines`·`seats` all become `Nothing` (= `NA`).

### Explicit keys with `join_by`

Default `left_join` uses **all common variables as keys** (**natural join**). This backfires: `flights2 |> left_join(planes)` shares both `year` and `tailnum`, so both become composite key. But `flights$year` (departure year) and `planes$year` (manufacture year) **differ in meaning**, so no matches; result is all `NA`.

To join on `tailnum` only, specify explicitly with `join_by(tailnum)`:

| R | hgg |
|---|---|
| `flights2 \|> left_join(planes, join_by(tailnum))` | Use only `tailnum`; rename `year` to `year.x`/`year.y` |

Output **disambiguates** `year` into `year.x` (flights) and `year.y` (planes) (R's `suffix` equivalent). Different column name keys also work via `join_by`:

| R | hgg |
|---|---|
| `left_join(airports, join_by(dest == faa))` | `leftIdx (dest) (faa)` |
| `left_join(airports, join_by(origin == faa))` | `leftIdx (origin) (faa)` |

In `dest == faa`, destinations not in `airports` (like `BQN`) get `NA` for `name`.

---

## 3. Filtering join — `semi_join` / `anti_join`

Filtering joins add no variables, just **filter `x` rows** by match status:

- **semi_join**: Keep `x` rows with matches in `y`.
- **anti_join**: Keep `x` rows with **no match** in `y`.

| R | hgg |
|---|---|
| `airports \|> semi_join(flights2, join_by(faa == origin))` | `semiIdx (faa) (origin)` → reselect rows |
| `airports \|> semi_join(flights2, join_by(faa == dest))` | `semiIdx (faa) (dest)` (101 destination airports) |
| `flights2 \|> anti_join(airports, join_by(dest == faa)) \|> distinct(dest)` | `antiIdx` + `nub` |

`faa == origin` semi_join narrows to 3 departure airports (EWR / JFK / LGA).
anti_join is handy for finding **implicit missing**. Destinations not in `airports`: 4

```
dest
BQN    ← Aguadilla (Puerto Rico)
SJU    ← San Juan
STT    ← St. Thomas (US Virgin Islands)
PSE    ← Ponce
```

Aircraft not in `planes`: **722** (1 has `tailnum` itself NA).

---

## 4. How joins work

Small 2-table example with `x`·`y` (key, `val_x`/`val_y`):

```
x: key val_x      y: key val_y
   1   x1            1   y1
   2   x2            2   y2
   3   x3            4   y3
```

| join | Kept rows | Result |
|---|---|---|
| `inner_join` | Keys in both | Keys 1, 2 |
| `left_join` | All `x` rows | 1,2,3 (`key=3`'s `val_y` = `NA`) |
| `right_join` | All `y` rows | 1,2,4 (`key=4`'s `val_x` = `NA`) |
| `full_join` | All rows from `x` or `y` | 1,2,3,4 (missing side = `NA`) |

Outer joins (left/right/full) are unified: imagine adding virtual rows (values `NA`) matching no keys to the other side.

### Rows aren't always 1-to-1

When `x`'s 1 row matches `y`'s **multiple rows**, that row **duplicates** by match count. Duplicate keys in both tables yield **many-to-many**, causing combinatorial explosion:

```
df1: key=1,2,2   df2: key=1,2,2   →   inner_join yields 5 rows (key=2 is 2×2)
```

| R | hgg |
|---|---|
| `df1 \|> inner_join(df2, join_by(key))` | `innerIdx` naturally expands 1-to-many |

---

## 5. Non-equi joins

Joins matching by operators other than `==`. When not equality, both keys' values differ, so output always **keeps both keys** (`keep = TRUE` equivalent · `key.x`/`key.y`).

### Cross join — all combinations

Cartesian product: `nrow(x) * nrow(y)` rows. Useful for generating all name pairs (self-join).

| R | hgg |
|---|---|
| `df \|> cross_join(df)` | List comprehension all pairs (4×4 = 16 rows) |

### Inequality join

Operators `<`·`<=`·`>=`·`>` narrow the match set. Restricting cross join by inequality yields "all **combinations**" not "all **permutations**":

| R | hgg |
|---|---|
| `df \|> inner_join(df, join_by(id < id))` | `[(i,j) \| a < b]` (6 rows) |

### Rolling join — closest match

From rows satisfying the inequality, take **only the closest 1**. Useful when dates don't align exactly; find "closest date before a given date".

Employee birthday / party quarter example: assign each employee "closest prior party".

| R | hgg |
|---|---|
| `employees \|> left_join(parties, join_by(closest(birthday >= party)))` | For each birthday, take max `party <= birthday` |

```
name   birthday    q   party
Hazel  2022-01-03  NA  NA          ← Before 1/10 → no party
Lily   2022-02-14  1   2022-01-10
Ada    2022-04-04  2   2022-04-04
...
```

Birthdays before 1/10 have no party (confirmable via `anti_join`).

### Overlap join — interval overlap

Helper predicates for interval joins: `between` / `within` / `overlaps`.

First, parties have **period** (`start`–`end`). Self-join for overlaps finds input error: Q2 and Q3 boundaries overlap:

| R | hgg |
|---|---|
| `parties \|> inner_join(parties, join_by(overlaps(start, end, start, end), q < q))` | `sa <= eb && ea >= sb && qa < qb` |

```
start.x     end.x       start.y     end.y
2022-04-04  2022-07-11  2022-07-11  2022-10-02   ← Q2 end and Q3 start overlap
```

Fix `end` (Q2 to 07-10), then use `between` to assign employees to parties. With `start` from year-start (1/1), early January birthdays no longer miss:

| R | hgg |
|---|---|
| `employees \|> inner_join(parties, join_by(between(birthday, start, end)))` | `start <= birthday <= end` |

> **Honest note on random input.** R source uses `set.seed(123)` + `babynames` to randomly generate 100 employees. R's RNG can't be perfectly reproduced externally, so we show rolling/overlap join logic with **fixed roster of 10 representative employees**. Only input data changed (not random); rolling/overlap **join methodology is identical to R4DS**.

---

## Reference table (summary of correspondence)

| dplyr | hgg |
|---|---|
| `left_join(y)` / `inner_join` / `right_join` / `full_join` | `leftIdx`/`innerIdx`/`rightIdx`/`fullIdx` + column re-selection |
| `join_by(a == b)` | Different column names as `leftIdx (a) (b)` |
| `semi_join` / `anti_join` | `semiIdx` / `antiIdx` + row re-select |
| `cross_join` | List comprehension all pairs |
| `join_by(a < b)` | Restrict pairs by inequality |
| `join_by(closest(a >= b))` | Max/min satisfying condition, 1 only |
| `join_by(between/overlaps(...))` | Interval containment / overlap test |

> **Honest limitations.** (1) Ch19 conceptual diagrams (ER, Venn) aren't real-data figures, so we omit visualizations (explained in prose). (2) rolling/overlap employee data can't reproduce R's random seed, so we use fixed roster (join method identical). (3) `dataframe` join semantics differ from dplyr, so we self-implemented (see chapter intro).

Previous → [`18-missing`](../18-missing/).
Next → `11-modeling` (beyond R4DS 2e scope · bonus).
