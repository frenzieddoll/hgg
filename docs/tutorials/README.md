# hgg Tutorials — Based on R for Data Science 2e

> 🌐 **English** | [日本語](README.ja.md)

Practical tutorials for hgg + dataframe with **1:1 correspondence** to the chapter structure of
[R for Data Science 2e](https://r4ds.hadley.nz/) (Hadley Wickham et al.).
Each chapter uses R4DS 2e as the primary reference and employs the **actual datasets from R4DS**
(penguins / flights / diamonds / table series)—no synthetic data. Sources are documented in
[`_data/_raw/SOURCE.md`](_data/_raw/SOURCE.md).

Each chapter pairs R (tidyverse) code with hgg code, making it easy for readers familiar with
ggplot2 / dplyr / tidyr to transition directly. DataFrame transformations are written using
dataframe's `|>` forward pipe (equivalent to R4DS's `|>`).

> Comprehensive specifications for marks / encodings / decorations used in each chapter are
> available in the [API Reference](../api-guide/README.md) (tutorials provide a learning path;
> reference serves as a dictionary).

## Chapter List (Corresponding to R4DS 2e chapters)

R4DS 2e contains **29 chapters across 6 parts**. This tutorial **faithfully reproduces all chapters**
(see [`r4ds-faithful-mandate`]). The `Ch` column shows the authoritative R4DS chapter number.
Directory numbers may differ for historical reasons (being standardized over time).

### Part 1: Whole game

| Ch | R4DS Chapter | dir | Real Data | Status |
|---|---|---|---|---|
| 1 | [Data visualization](https://r4ds.hadley.nz/data-visualize) | [`01-visualize`](01-visualize/) | penguins | Complete |
| 2 | [Workflow: basics](https://r4ds.hadley.nz/workflow-basics) | [`02-workflow-basics`](02-workflow-basics/) | (code chapter, no plots) | Complete |
| 3 | [Data transformation](https://r4ds.hadley.nz/data-transform) | [`03-data-transform`](03-data-transform/) | flights / batting | Complete |
| 4 | [Workflow: code style](https://r4ds.hadley.nz/workflow-style) | [`04-workflow-style`](04-workflow-style/) | (code chapter) | Complete |
| 5 | [Data tidying](https://r4ds.hadley.nz/data-tidy) | [`05-data-tidying`](05-data-tidying/) | table series / billboard | Complete |
| 6 | [Workflow: scripts and projects](https://r4ds.hadley.nz/workflow-scripts) | [`06-workflow-scripts`](06-workflow-scripts/) | (code chapter) | Complete |
| 7 | [Data import](https://r4ds.hadley.nz/data-import) | [`07-data-import`](07-data-import/) | students | Complete |
| 8 | [Workflow: getting help](https://r4ds.hadley.nz/workflow-help) | [`08-getting-help`](08-getting-help/) | (code chapter) | Complete |

### Part 2: Visualize

| Ch | R4DS Chapter | dir | Real Data | Status |
|---|---|---|---|---|
| 9 | [Layers](https://r4ds.hadley.nz/layers) | [`09-layers`](09-layers/) | mpg | Complete |
| 10 | [Exploratory data analysis](https://r4ds.hadley.nz/eda) | [`10-eda`](10-eda/) | diamonds / mpg / flights | Complete |
| 11 | [Communication](https://r4ds.hadley.nz/communication) | [`11-communication`](11-communication/) | mpg | Complete |

### Part 3: Transform

| Ch | R4DS Chapter | dir | Real Data | Status |
|---|---|---|---|---|
| 12 | [Logical vectors](https://r4ds.hadley.nz/logicals) | [`12-logical`](12-logical/) | flights | Complete |
| 13 | [Numbers](https://r4ds.hadley.nz/numbers) | [`13-numbers`](13-numbers/) | flights | Complete |
| 14 | [Strings](https://r4ds.hadley.nz/strings) | [`14-strings`](14-strings/) | babynames | Complete |
| 15 | [Regular expressions](https://r4ds.hadley.nz/regexps) | [`15-regexps`](15-regexps/) | words / fruit / babynames | Complete |
| 16 | [Factors](https://r4ds.hadley.nz/factors) | [`16-factors`](16-factors/) | gss_cat | Complete |
| 17 | [Dates and times](https://r4ds.hadley.nz/datetimes) | [`17-datetimes`](17-datetimes/) | flights | Complete |
| 18 | [Missing values](https://r4ds.hadley.nz/missing-values) | [`18-missing`](18-missing/) | treatment / stocks / health | Complete |
| 19 | [Joins](https://r4ds.hadley.nz/joins) | [`19-joins`](19-joins/) | nycflights13 5 tables | Complete |

### Part 4: Import

| Ch | R4DS Chapter | dir | Real Data | Status |
|---|---|---|---|---|
| 20 | [Spreadsheets](https://r4ds.hadley.nz/spreadsheets) | `20-spreadsheets` | students / penguins | Planned※ |
| 21 | [Databases](https://r4ds.hadley.nz/databases) | `21-databases` | (SQL) | Planned※ |
| 22 | [Arrow](https://r4ds.hadley.nz/arrow) | `22-arrow` | seattle library | Planned※ |
| 23 | [Hierarchical data](https://r4ds.hadley.nz/rectangling) | `23-hierarchical` | (JSON) | Planned |
| 24 | [Web scraping](https://r4ds.hadley.nz/webscraping) | `24-webscraping` | (HTML) | Planned※ |

### Part 5: Program

| Ch | R4DS Chapter | dir | Real Data | Status |
|---|---|---|---|---|
| 25 | [Functions](https://r4ds.hadley.nz/functions) | `25-functions` | diamonds / flights | Planned |
| 26 | [Iteration](https://r4ds.hadley.nz/iteration) | `26-iteration` | (multiple CSV, etc.) | Planned |
| 27 | [A field guide to base R](https://r4ds.hadley.nz/base-r) | `27-base-r` | — | Planned※ |

### Part 6: Communicate

| Ch | R4DS Chapter | dir | Real Data | Status |
|---|---|---|---|---|
| 28 | [Quarto](https://r4ds.hadley.nz/quarto) | `28-quarto` | — | Planned※ |
| 29 | [Quarto formats](https://r4ds.hadley.nz/quarto-formats) | `29-quarto-formats` | — | Planned※ |

> **※ Chapters marked with ※ are tightly integrated with R/RStudio tooling** (Excel/Google Sheets,
> DBI/SQL, Arrow, rvest, base R syntax, Quarto). These are **adapted faithfully to their spirit
> and examples**, mapping them to equivalent Haskell + hgg / dataframe approaches. Each chapter's
> README honestly records the adaptation's rationale and constraints. Elements that cannot be
> adapted are explained conceptually with clear indication.
>
> **Standardization completed (2026-06-20)**: Directory leading numbers aligned with R4DS `Ch` numbers
> (`07-communication`→`11-`, `08-datetimes`→`17-`, `09-missing`→`18-`, `10-joins`→`19-`).
> For Ch9 Layers, the faithful version `09-layers` was adopted as canonical (superseding the
> relaxed earlier version `05-layers` with 14 figures only, moved to `_trash/`). From this point
> forward, directory number = `Ch` number.

## Chapter Structure

Each chapter directory contains:

- `README.md` — Lesson text (links to corresponding R4DS sections + R↔hgg correspondence table)
- `<Name>.hs` — Executable plot generation program
- `*.csv` — Real data used in the chapter (processed/extracted from `_data/_raw/`)
- `*.svg` — Generated plots

## How to Run

Plot generation programs are executed with the chapter directory as the current working directory
(since `cabal run` defaults to repo root):

```sh
cd docs/tutorials/01-visualize
cabal run tut-01-visualize
```

## Related Documentation

- [API Reference](../api-guide/README.md)
- [Migration from ggplot2](../migration-from-ggplot.md)
- Primary source: [R for Data Science 2e](https://r4ds.hadley.nz/)
