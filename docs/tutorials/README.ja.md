# hgg チュートリアル — R for Data Science 2e ベース

> 🌐 [English](README.ja.md) | **日本語**

[R for Data Science 2e](https://r4ds.hadley.nz/)(Hadley Wickham ほか)の章構成に
**1:1 で対応** させた、hgg + dataframe による実践チュートリアル群です。
各章は R4DS 2e 本体を一次情報とし、**R4DS が使う実データ**(penguins / flights /
diamonds / table 系)をそのまま用います(捏造データは一切ありません。出所は
[`_data/_raw/SOURCE.md`](_data/_raw/SOURCE.md))。

各章は R(tidyverse)のコードと hgg のコードを並べ、ggplot2 / dplyr / tidyr に
慣れた読者がそのまま移行できるように書いています。DataFrame の変換は dataframe の
`|>` 前方パイプ(R4DS の `|>` と同型)で記述します。

> 各章で使う mark / encoding / 装飾の**網羅的な仕様**は
> [API リファレンス](../api-guide/README.ja.md) を参照(チュートリアルは学習導線、
> リファレンスは辞書、 と役割を分けています)。

## 章一覧(R4DS 2e 章に対応)

R4DS 2e は **全 29 章・6 部**。本チュートリアルは **全章を忠実再現する**(方針:
[`r4ds-faithful-mandate`])。`Ch` 列が R4DS 章番号(=正)。ディレクトリ番号は歴史的事情で
章番号と一致しないものがある(順次整理予定)。

### 第 1 部 Whole game

| Ch | R4DS 章 | dir | 実データ | 状態 |
|---|---|---|---|---|
| 1 | [Data visualization](https://r4ds.hadley.nz/data-visualize) | [`01-visualize`](01-visualize/) | penguins | 完了 |
| 2 | [Workflow: basics](https://r4ds.hadley.nz/workflow-basics) | [`02-workflow-basics`](02-workflow-basics/) | (コード章・図なし) | 完了 |
| 3 | [Data transformation](https://r4ds.hadley.nz/data-transform) | [`03-data-transform`](03-data-transform/) | flights / batting | 完了 |
| 4 | [Workflow: code style](https://r4ds.hadley.nz/workflow-style) | [`04-workflow-style`](04-workflow-style/) | (コード章) | 完了 |
| 5 | [Data tidying](https://r4ds.hadley.nz/data-tidy) | [`05-data-tidying`](05-data-tidying/) | table 系 / billboard | 完了 |
| 6 | [Workflow: scripts and projects](https://r4ds.hadley.nz/workflow-scripts) | [`06-workflow-scripts`](06-workflow-scripts/) | (コード章) | 完了 |
| 7 | [Data import](https://r4ds.hadley.nz/data-import) | [`07-data-import`](07-data-import/) | students | 完了 |
| 8 | [Workflow: getting help](https://r4ds.hadley.nz/workflow-help) | [`08-getting-help`](08-getting-help/) | (コード章) | 完了 |

### 第 2 部 Visualize

| Ch | R4DS 章 | dir | 実データ | 状態 |
|---|---|---|---|---|
| 9 | [Layers](https://r4ds.hadley.nz/layers) | [`09-layers`](09-layers/) | mpg | 完了 |
| 10 | [Exploratory data analysis](https://r4ds.hadley.nz/eda) | [`10-eda`](10-eda/) | diamonds / mpg / flights | 完了 |
| 11 | [Communication](https://r4ds.hadley.nz/communication) | [`11-communication`](11-communication/) | mpg | 完了 |

### 第 3 部 Transform

| Ch | R4DS 章 | dir | 実データ | 状態 |
|---|---|---|---|---|
| 12 | [Logical vectors](https://r4ds.hadley.nz/logicals) | [`12-logical`](12-logical/) | flights | 完了 |
| 13 | [Numbers](https://r4ds.hadley.nz/numbers) | [`13-numbers`](13-numbers/) | flights | 完了 |
| 14 | [Strings](https://r4ds.hadley.nz/strings) | [`14-strings`](14-strings/) | babynames | 完了 |
| 15 | [Regular expressions](https://r4ds.hadley.nz/regexps) | [`15-regexps`](15-regexps/) | words / fruit / babynames | 完了 |
| 16 | [Factors](https://r4ds.hadley.nz/factors) | [`16-factors`](16-factors/) | gss_cat | 完了 |
| 17 | [Dates and times](https://r4ds.hadley.nz/datetimes) | [`17-datetimes`](17-datetimes/) | flights | 完了 |
| 18 | [Missing values](https://r4ds.hadley.nz/missing-values) | [`18-missing`](18-missing/) | treatment / stocks / health | 完了 |
| 19 | [Joins](https://r4ds.hadley.nz/joins) | [`19-joins`](19-joins/) | nycflights13 5 表 | 完了 |

### 第 4 部 Import

| Ch | R4DS 章 | dir | 実データ | 状態 |
|---|---|---|---|---|
| 20 | [Spreadsheets](https://r4ds.hadley.nz/spreadsheets) | `20-spreadsheets` | students / penguins | 予定※ |
| 21 | [Databases](https://r4ds.hadley.nz/databases) | `21-databases` | (SQL) | 予定※ |
| 22 | [Arrow](https://r4ds.hadley.nz/arrow) | `22-arrow` | seattle library | 予定※ |
| 23 | [Hierarchical data](https://r4ds.hadley.nz/rectangling) | `23-hierarchical` | (JSON) | 予定 |
| 24 | [Web scraping](https://r4ds.hadley.nz/webscraping) | `24-webscraping` | (HTML) | 予定※ |

### 第 5 部 Program

| Ch | R4DS 章 | dir | 実データ | 状態 |
|---|---|---|---|---|
| 25 | [Functions](https://r4ds.hadley.nz/functions) | `25-functions` | diamonds / flights | 予定 |
| 26 | [Iteration](https://r4ds.hadley.nz/iteration) | `26-iteration` | (複数 csv 等) | 予定 |
| 27 | [A field guide to base R](https://r4ds.hadley.nz/base-r) | `27-base-r` | — | 予定※ |

### 第 6 部 Communicate

| Ch | R4DS 章 | dir | 実データ | 状態 |
|---|---|---|---|---|
| 28 | [Quarto](https://r4ds.hadley.nz/quarto) | `28-quarto` | — | 予定※ |
| 29 | [Quarto formats](https://r4ds.hadley.nz/quarto-formats) | `29-quarto-formats` | — | 予定※ |

> **※印の章は R/RStudio ツーリングに密着**(Excel/Google Sheets・DBI/SQL・Arrow・
> rvest・base R 構文・Quarto)。これらは R4DS の**意図と例を忠実になぞりつつ**、
> Haskell + hgg / dataframe での等価手段に翻案する(各章 README に翻案の根拠と
> 制約を honest に記録)。翻案不能な要素は「概念のみ説明」とし、その旨を明示する。
>
> **整理済 (2026-06-20)**: ディレクトリ先頭番号を R4DS の `Ch` 番号に揃えた
> (`07-communication`→`11-`・`08-datetimes`→`17-`・`09-missing`→`18-`・`10-joins`→`19-`)。
> Ch9 Layers は重複していた 2 dir のうち忠実版 `09-layers` を正準とし、緩基準の旧稿
> `05-layers`(14 図のみ)は `_trash/` へ退役。以降 dir 番号 = `Ch` 列で一致。

## 各章の構成

各章ディレクトリには次が含まれます:

- `README.md` — レッスン本文(R4DS 該当節へのリンク + R↔hgg 対応表)
- `<Name>.hs` — 実行可能な図生成プログラム
- `*.csv` — その章で使う実データ(`_data/_raw/` から整形・抽出したもの)
- `*.svg` — 生成された図

## 実行方法

図生成プログラムは章ディレクトリを CWD にして実行します
(`cabal run` の CWD は repo root のため):

```sh
cd docs/tutorials/01-visualize
cabal run tut-01-visualize
```

## 関連ドキュメント

- [API リファレンス](../api-guide/README.ja.md)
- [ggplot2 からの移行](../migration-from-ggplot.ja.md)
- 一次情報: [R for Data Science 2e](https://r4ds.hadley.nz/)
