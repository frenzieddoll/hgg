# 18. 欠損値 — Missing values

> 一次情報: **R for Data Science 2e, Ch.18 "Missing values"**
> <https://r4ds.hadley.nz/missing-values>
> データ: R4DS 本文の実例 **treatment / stocks / health**(本文の値をそのまま CSV 化)

「明示的な欠損(`NA`)」 と「暗黙的な欠損(行そのものが無い)」 の扱いを学びます。
表操作が中心で、R4DS が描く図は 2 枚(factor の空グループの有無)だけです。
実行コードは [`Missing.hs`](Missing.hs)。dataframe に `fill`/`complete` 等は無いので、
中身を Haskell で計算して結果を `DF.fromNamedColumns` で組み立てます。

## 実行

```sh
cd docs/tutorials/18-missing
cabal run tut-18-missing
```

---

## 1. 明示的な欠損 — `fill`(前方補完)

`treatment` の `person` は、同じ人の続きでは `NA` になっています。直前の値で
埋める「前方補完(last observation carried forward)」 を行います。

| R | hgg |
|---|---|
| `treatment |> fill(person)` | 自前 `fillForward :: [Maybe a] -> [Maybe a]` |

```
person            treatment response        person(fill 後)
Derrick Whitmore  1         7          →     Derrick Whitmore
NA                2         10               Derrick Whitmore
NA                3         NA               Derrick Whitmore
Katherine Burke   1         4                Katherine Burke
```

## 2. 固定値で穴埋め — `coalesce`

`NA` を決まった値(ここでは 0)に置き換えます。

| R | hgg |
|---|---|
| `coalesce(response, 0)` | `map (fromMaybe 0) responseVals` |

## 3. 暗黙的な欠損 — `pivot_wider` で明示化

`stocks` は **2020 Q4 が明示 `NA`**、**2021 Q1 は行そのものが無い**(暗黙の欠損)です。
`qtr` を列に展開すると、無かった組み合わせが `NA` として現れます。

| R | hgg |
|---|---|
| `stocks |> pivot_wider(names_from=qtr, values_from=price)` | 自前 pivot(qtr 値→列・欠損は `Nothing`) |

```
year   q1       q2     q3     q4
2020   1.88     0.59   0.35   NA      ← 2020 Q4 は明示 NA
2021   NA       0.92   0.17   2.66    ← 2021 Q1 は行が無かった → NA
```

## 4. 全組み合わせを補う — `complete`

`(year × qtr)` の全 8 組を生成し、無い行を `NA` で補います。

| R | hgg |
|---|---|
| `stocks |> complete(year, qtr)` | 全組を生成し各組の price を引く(無ければ `Nothing`) |

これで `2021 Q1 = NA` が 1 行として明示化されます。

## 5. factor と空グループ(図 2 枚)

`health` の `smoker` は水準 `{yes, no}` ですが全員 `no` です。`yes` は**空グループ**。

### 空グループを落とす(既定)(`01-drop-empty.svg`)

観測された値だけ数えると、`no` の棒しか出ません。

![drop empty](01-drop-empty.svg)

### 空グループも保持(`drop = FALSE`)(`02-keep-empty.svg`)

水準を全部残すと、`yes = 0` も(高さ 0 の棒として)x 軸に現れます。

| R | hgg |
|---|---|
| `geom_bar()`(既定) | 観測値のみで集計して `bar` |
| `scale_x_discrete(drop = FALSE)` | 全水準を明示して `bar`(`yes=0` を含める) |

![keep empty](02-keep-empty.svg)

> dataframe に factor 型は無いので「空グループ」 は自動では現れません。R4DS の
> `drop = FALSE` の意図(取りうる水準をすべて見せる)を、`yes=0` を明示的に
> 加えることで再現しています。

---

## この章で出てきた対応表(まとめ)

| tidyr / dplyr | hgg |
|---|---|
| `fill(col)` | 自前 `fillForward`(前方補完) |
| `coalesce(x, v)` | `map (fromMaybe v)` |
| `pivot_wider` | 自前 pivot(欠損は `Nothing`) |
| `complete(a, b)` | 全組生成 + 各組を引く |
| `scale_x_discrete(drop=FALSE)` | 全水準を明示して `bar` |

前章 → [`17-datetimes`](../17-datetimes/)。
次章 → [`19-joins`](../19-joins/)(Ch19 Joins)。
