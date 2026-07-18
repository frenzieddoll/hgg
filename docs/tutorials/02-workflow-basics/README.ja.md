# 02. ワークフローの基礎 (R4DS 2e Ch.2 "Workflow: basics")

> 🌐 [English](README.ja.md) | **日本語**

> 一次情報: **R for Data Science 2e, Ch.2 "Workflow: basics"**
> <https://r4ds.hadley.nz/workflow-basics>

R4DS 第 2 章は **図を一切描かない**「R コーディングの基礎」章です(オブジェクト作成・
コメント・命名・関数呼び出し)。 本チュートリアルは R4DS の各コード例を **Haskell の
等価コードに忠実に対応づけて**実行し、 同じ結果を出します。 R と Haskell で考え方が
違う箇所は明示します(省略・置換はしません)。 実行コードは [`WorkflowBasics.hs`](WorkflowBasics.hs)。

```sh
cd docs/tutorials/02-workflow-basics
cabal run tut-02-workflow-basics
```

## §2.1 Coding basics

基本的な計算は R と同じ中置演算子・優先順位で書けます。

| R | hgg/Haskell | 結果 |
|---|---|---|
| `1 / 200 * 30` | `1 / 200 * 30` | `0.15` |
| `(59 + 73 + 2) / 3` | `(59 + 73 + 2) / 3` | `44.6667` |
| `sin(pi / 2)` | `sin (pi / 2)` | `1.0` |

オブジェクトの作成。 R は再代入できる `<-`、 Haskell は**不変の束縛** `let`:

| R | Haskell |
|---|---|
| `x <- 3 * 4` | `let x = 3 * 4` |

どちらも束縛しただけでは表示されず、 名前を評価して初めて値が出ます(`x` → `12`)。

ベクトル。 R は `c(...)`、 Haskell はリスト(または `Data.Vector`):

| R | Haskell |
|---|---|
| `primes <- c(2, 3, 5, 7, 11, 13)` | `let primes = [2,3,5,7,11,13]` |

> **★相違(broadcast)**: R はベクトルへの算術を全要素に自動適用しますが、 Haskell は
> 自動 broadcast しないので `map` で各要素に適用します。
> `primes * 2` → `map (* 2) primes` = `[4,6,10,14,22,26]`、
> `primes - 1` → `map (subtract 1) primes` = `[1,2,4,6,10,12]`。

## §2.2 Comments

コメントは R が `#`、 Haskell が `--`(`{- ... -}` でブロック)。 用途は同じく
「**why** を書く(how/what でなく)」。 `WorkflowBasics.hs` のコメントがそのまま例です。

## §2.3 What's in a name?

命名規約。 R4DS は **snake_case** を推奨。 Haskell の慣例は **camelCase**(関数・束縛)/
**PascalCase**(型・コンストラクタ)で、 本プロジェクトも camelCase(`CLAUDE.md`)。

| R (snake_case) | Haskell (camelCase) |
|---|---|
| `this_is_a_really_long_name <- 2.5` | `let thisIsAReallyLongName = 2.5` |
| `r_rocks <- 2^3` | `let rRocks = 2 ^ 3` |

> **★大文字小文字・綴りは厳密**。 R では `r_rock` / `R_rocks` は実行時に
> "object not found"。 Haskell では `rRock` / `RRocks` は**コンパイル時**にスコープ
> エラー(出荷前に型検査で捕まる、 という違い)。

## §2.4 Calling functions

関数呼び出し。 R には名前付き引数がありますが Haskell にはありません(位置引数、
レコードで擬似的に表現)。 等差列 `seq()` は Haskell のレンジ記法が最も自然です。

| R | Haskell |
|---|---|
| `seq(from = 1, to = 10)` | `enumFromTo 1 10` |
| `seq(1, 10)` | `[1 .. 10]` |
| `x <- "hello world"` | `let greeting = "hello world"` |

引用符・括弧は対で閉じる必要があるのは両言語共通です。

## §2.5 Exercises(要点)

1. `my_varıable` の `ı` はトルコ語の点なし i で `my_variable` とは別字。 Haskell でも
   識別子が 1 文字違えば別物(スコープエラー)。 「綴り・字種は厳密」。
2. `libary(todyverse)` / `aes(x = displ y = hwy)` / `method = "lm` のタイポ修正
   (`library(tidyverse)` / `aes(x = displ, y = hwy)` / `method = "lm"`)。 Haskell でも
   import 名・引数の綴りと括弧/引用符の対は厳密。
3–4. RStudio ショートカット / `ggsave` の保存対象は IDE・R 固有の話題(対応 IDE 機能や
   `saveSVG` で代替可能だが、 図の再現対象ではないため割愛)。

## メモ

この章は図を生成しないため SVG はありません。 R4DS の全コード例を Haskell 等価で
実行し、 出力で確認します(`cabal run tut-02-workflow-basics`)。
