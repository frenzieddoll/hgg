# 16. 因子 (R4DS 2e Ch.16 "Factors")

> 一次情報: **R for Data Science 2e, Ch.16 "Factors"**
> <https://r4ds.hadley.nz/factors>
> データ: **gss_cat**(General Social Survey 抽出・21,483 行 × 9 列。
> `../_data/_raw/gss_cat.csv`)。 forcats 同梱。

factor は「取りうる値が固定されたカテゴリ変数」 を表す型です。 R では **forcats**
(tidyverse) の `fct_*` 関数群で、 水準 (levels) の**順序**や**中身**を操作します。
本章は R4DS 第 16 章の全節(16.2 基礎 / 16.3 GSS / 16.4 順序変更 / 16.5 水準変更 /
16.6 順序付き因子)を実データで忠実に再現します。 **解説 → コード → 図** を並べた
walkthrough です。 完全な実行コードは [`Factors.hs`](Factors.hs)。

forcats の `fct_*` は analyze 側 module **`Hanalyze.Data.Factor`**(Phase 28 Ch16)に
実装済みです。 `Factor` 型は `facLevels`(水準ラベルの順序付きリスト)+ `facCodes`
(各観測の水準コード・0 始まり・NA = `-1`)を持ち、 forcats と同じ「水準順序」 概念を表します。

```sh
cd docs/tutorials/16-factors
cabal run tut-16-factors   # 01-…svg .. 05-marital-bar.svg + 各節の表出力を生成
```

## 忠実性メモ(R4DS との差異を実測して honest 記録)

- **横向き dot plot(`aes(x=値, y=カテゴリ)`)**: hgg の `scatter` は categorical
  位置を直接描けないため、 水準を数値 index にして `axisBreaksLabeled` で y 軸ラベルを
  差します(`saveDotH` ヘルパ)。 見た目は R の `geom_point` + 離散 y 軸と同等です。
- **線の配色**: R4DS の §16.4 折れ線は `scale_color_brewer(palette="Set1")`。 本章は
  hgg 既定の hue palette を使います(色は違いますが**系列の順序**は
  `fct_reorder2` で R と一致)。
- **count() の 0 件水準**: R の `count()` は既定で 0 件水準を落とします(本章も同様)。
  `fctCount` 自体は 0 件水準も返します(`scale_x_discrete(drop=FALSE)` 相当)。
- **数値は捏造しません**: count 表・reorder 後の水準順・lump 結果はすべて gss_cat 実データから
  算出し、 R4DS の出力と突合済みです(各節に値を併記)。

---

## 16.2 Factor basics

文字ベクトルには 2 つの問題があります。 (1) 打ち間違いに気づけない、 (2) 並びが
アルファベット順になる。 factor は **取りうる水準を固定**してこれを解決します。

```r
# R (forcats)
x1 <- c("Dec", "Apr", "Jan", "Mar")
month_levels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
factor(x1)                      # 水準はアルファベット順
factor(x1, levels = month_levels)   # 意味的順序を与える
fct(x1)                         # 出現順 (forcats・誤値はエラーにできる安全版)
```

```haskell
-- hgg (Hanalyze.Data.Factor)
levels (factor x1)                    -- ["Apr","Dec","Jan","Mar"]  (sort)
levels (factorWith monthLevels x1)    -- monthLevels の順を保持
levels (fct x1)                       -- ["Dec","Apr","Jan","Mar"]  (出現順)
asTexts (factorWith monthLevels x1)   -- ["Dec","Apr","Jan","Mar"]  (元の値へ)
```

`factor` は水準をソート、 `factorWith` は明示水準、 `fct`(forcats)は出現順です。
`facCodes` は 0 始まりの整数コードで、 `asTexts` でラベルへ戻せます(NA は `Nothing`)。

## 16.3 General Social Survey

gss_cat は総合社会調査 (GSS) の抽出で、 factor 列 6(marital / race / rincome / partyid /
relig / denom)と整数列 3(year / age / tvhours)を持ちます。 水準の頻度は `count()` で見ます。

```r
gss_cat |> count(race)
```

```haskell
fctCount (factorWith raceLevels race)   -- (水準, 件数)。0 件水準は drop
```

実データの出力(R4DS と一致):

```
Other   1959
Black   3129
White  16395            -- Not applicable は 0 件ゆえ drop
```

最多の水準: **relig = Protestant(10,846)**、 **partyid = Independent(4,119)**。

## 16.4 Modifying factor order

### 並べ替え前 — 解釈しにくい

`relig` ごとに 1 日のテレビ視聴時間 `tvhours` の平均を出して点で描きます。 水準が
gss_cat の定義順のままだと、 点が上下に散らばって傾向が読めません。

```r
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(tvhours = mean(tvhours, na.rm = TRUE), n = n())
ggplot(relig_summary, aes(x = tvhours, y = relig)) + geom_point()
```

```haskell
-- relig 別 tvhours 平均を Data.Factor の既定水準順で描く
saveDotH "01-…svg" religPresent religMeanMap "tvhours" "relig" "…"
```

![relig vs tvhours(並べ替え前)](01-relig-tvhours-unordered.svg)

### `fct_reorder` で並べ替え

`fct_reorder(relig, tvhours)` は各水準を `tvhours` の値で**昇順**に並べ替えます。
すると傾向が一目で読めます — "Don't know" が最も視聴し、 Hindu や Other eastern が最少です。

```r
ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()
```

```haskell
-- fctReorder で水準を tvhours 平均の昇順へ (集約は中央値 = 各水準 1 値ゆえ恒等)
let religReord = levels (fctReorder medianD (factorWith religPresent religPresent) religMeans)
saveDotH "02-…svg" religReord religMeanMap "tvhours" "fct_reorder(relig, tvhours)" "…"
```

![relig vs tvhours(fct_reorder 後)](02-relig-tvhours-reorder.svg)

### `fct_relevel` で特定水準を先頭へ

収入 `rincome` 別の平均年齢を描くとき、 既定の水準順(金額順)は妥当です。 ただ
"Not applicable" だけは金額ではないので、 `fct_relevel` で**先頭**(= y 軸の下端)へ移します。

```r
ggplot(rincome_summary,
       aes(x = age, y = fct_relevel(rincome, "Not applicable"))) + geom_point()
```

```haskell
let rincomeReleveled = levels (fctRelevel ["Not applicable"]
                                 (factorWith rincomePresent rincomePresent))
saveDotH "03-…svg" rincomeReleveled rincomeMeanMap "age" "fct_relevel(…)" "…"
```

![rincome vs age(fct_relevel)](03-rincome-age-relevel.svg)

### `fct_reorder2` で凡例順を線の右端に合わせる

年齢ごとの婚姻状態 `marital` の構成比を折れ線で描きます。 `fct_reorder2(marital, age, prop)`
は**最大の `age` における `prop`** で水準を**降順**に並べ替えるので、 凡例の色順が
グラフ右端の線の高さ順と一致して読みやすくなります。

```r
by_age <- gss_cat |> filter(!is.na(age)) |> count(age, marital) |>
  group_by(age) |> mutate(prop = n / sum(n))
ggplot(by_age, aes(x = age, y = prop,
                   color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) + labs(color = "marital")
```

```haskell
let maritalReord2 = levels (fctReorder2 (factorWith maritalPresent longCat) longAge longProp)
DF.empty |>> theme ThemeGrey <> layer (line (inline longAge) (inline longProp)
                      <> colorBy (inlineCat longCat) <> colorCats maritalReord2)
   <> xLabel "age" <> yLabel "prop" <> legendTitle "marital"
```

![marital prop by age(fct_reorder2)](04-marital-age-line.svg)

> 配色は R の Set1 と異なりますが、 凡例の順序(Widowed → Married → …)は線の右端の
> 高さ順で R と一致します。

### `fct_infreq` |> `fct_rev` で棒を頻度順に

棒グラフは頻度順に並べると見やすくなります。 `fct_infreq` で頻度**降順**、 続けて
`fct_rev` で反転すると頻度**昇順**になります。

```r
gss_cat |> mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) + geom_bar()
```

```haskell
let maritalOrder = levels (fctRev (fctInfreq (factorWith maritalLevels marital)))
DF.empty |>> theme ThemeGrey <> layer (bar (inlineCat names) (inline counts))
   <> scaleXDiscreteLimits maritalOrder
```

![marital 棒(fct_infreq |> fct_rev)](05-marital-bar.svg)

## 16.5 Modifying factor levels

### `fct_recode` — ラベルの改名

`fct_recode` は水準ラベルを読みやすく改名します。 複数の旧ラベルを同じ新ラベルに
向ければ**併合**されます。

```r
gss_cat |> mutate(partyid = fct_recode(partyid,
  "Republican, strong" = "Strong republican",
  "Republican, weak"   = "Not str republican",
  "Independent, near rep" = "Ind,near rep",
  "Independent, near dem" = "Ind,near dem",
  "Democrat, weak"     = "Not str democrat",
  "Democrat, strong"   = "Strong democrat")) |> count(partyid)
```

```haskell
fctRecode [ ("Republican, strong", "Strong republican")
          , ("Republican, weak",   "Not str republican")
          , ("Independent, near rep", "Ind,near rep")
          , ("Independent, near dem", "Ind,near dem")
          , ("Democrat, weak",     "Not str democrat")
          , ("Democrat, strong",   "Strong democrat") ] partyFac
```

出力(R4DS と一致): No answer 154 / Don't know 1 / Other party 393 /
Republican, strong 2314 / Republican, weak 3032 / Independent, near rep 1791 /
Independent 4119 / Independent, near dem 2499 / Democrat, weak 3690 / Democrat, strong 3490。

### `fct_collapse` — 複数水準を併合

```r
gss_cat |> mutate(partyid = fct_collapse(partyid,
  "other" = c("No answer","Don't know","Other party"),
  "rep" = c("Strong republican","Not str republican"),
  "ind" = c("Ind,near rep","Independent","Ind,near dem"),
  "dem" = c("Not str democrat","Strong democrat"))) |> count(partyid)
```

```haskell
fctCollapse [ ("other", ["No answer","Don't know","Other party"])
            , ("rep",   ["Strong republican","Not str republican"])
            , ("ind",   ["Ind,near rep","Independent","Ind,near dem"])
            , ("dem",   ["Not str democrat","Strong democrat"]) ] partyFac
```

出力(R4DS と一致): **other 548 / rep 5346 / ind 8409 / dem 7180**。

### `fct_lump_*` — 小さい水準を "Other" にまとめる

`fct_lump_lowfreq` は、 "Other" が最小水準のままでいられる範囲で低頻度水準をまとめます。
relig では Protestant 以外がすべて Other に入ります。

```r
gss_cat |> mutate(relig = fct_lump_lowfreq(relig)) |> count(relig)
#> Protestant 10846 / Other 10637
```

```haskell
fctCount (fctLumpLowfreq (factorWith religLevels relig))   -- Protestant 10846 / Other 10637
```

`fct_lump_n(relig, n = 10)` は頻度上位 10 水準を残します。 ただし relig には**もともと
"Other"(224 件)という水準があり**、 まとめ先の "Other" と併合されるため、 結果は
「9 個の固有水準 + Other」 になります(R4DS 演習 16.5.1 Q3 の論点)。

```haskell
sortBy (Down . snd) (fctCount (fctLumpN 10 (factorWith religLevels relig)))
```

出力(R4DS と一致): Protestant 10846 / Catholic 5124 / None 3523 / Christian 689 /
**Other 458**(= 元 224 + lump 234)/ Jewish 388 / Buddhism 147 /
Inter-nondenominational 109 / Moslem/islam 104 / Orthodox-christian 95。

## 16.6 Ordered factors

`ordered()` は水準間に `<` 順序を持つ因子を作ります。 ggplot2 では viridis 連続配色、
線形モデルでは多項式 contrast が当たります(本実装は順序フラグ `facOrdered` を保持・
配色/contrast 連動は概念のみ)。

```r
ordered(c("a", "b", "c"))    #> Levels: a < b < c
```

```haskell
let oz = ordered ["a","b","c"] ["a","b","c"]
levels oz     -- ["a","b","c"]   (a < b < c)
isOrdered oz  -- True
```

## 16.7 Summary

forcats は因子の水準順序と中身を扱う道具一式を与えます。 順序変更(`fct_reorder` /
`fct_relevel` / `fct_reorder2` / `fct_infreq` / `fct_rev`)、 水準変更(`fct_recode` /
`fct_collapse` / `fct_lump_*`)はいずれも `Hanalyze.Data.Factor` の対応関数で
再現できます。 さらに学ぶなら McNamara & Horton のカテゴリデータ整形の論文が薦められています。

---

## 演習

### 16.3.1

1. **`rincome` の分布を探り、 既定の棒グラフの何が悪いか?** — `rincome` は金額順の
   ordered factor だが、 既定の棒では水準ラベルが長く重なる。 `coordFlip`(横棒)に
   するか、 ラベルを回転すると読める。 また "Not applicable" が他と混じるので
   `fct_relevel` で端へ寄せると良い。
2. **最も多い `relig` / `partyid` は?** — relig = **Protestant**(10,846)、
   partyid = **Independent**(4,119)。 `fctCount` の最大要素で確認できる。
3. **`denom` はどの `relig` に対応するか?** — `denom`(教派)はキリスト教系
   (relig = Protestant / Catholic 等)にのみ意味を持つ。 relig×denom のクロス集計
   (両列を組にした `fctCount`)で、 非キリスト教では denom が "Not applicable" に
   集中することが確認できる。

### 16.4.1

1. **`tvhours` に不審な値は? 平均は妥当か?** — 1 日 24 時間に近い極端値が混じる。
   外れ値に弱いので、 平均より**中央値**(`fctReorder` の集約関数を `medianD` に)が
   頑健。 本章の `summarizeMean` は平均だが、 集約関数を差し替えれば中央値版になる。
2. **各 factor の水準順は恣意的か原理的か?** — `marital`(No answer→Married)や
   `rincome`(金額順)・`partyid`(政党スペクトル)は**原理的**。 `relig` / `denom` は
   おおむね恣意的(頻度や意味で並べ直す価値がある)。
3. **"Not applicable" を先頭にすると、 なぜグラフの一番下に来るのか?** — 離散軸は
   **第 1 水準を原点(下端)** に置くため。 `fct_relevel(…, "Not applicable")` は
   それを第 1 水準にするので、 y 軸の最下段に描かれる。

### 16.5.1

1. **民主/共和/無党派の割合は時系列でどう動いたか?** — `partyid` を `fct_collapse` で
   3 群(dem/rep/ind)にまとめ、 `year` ごとに割合を出して折れ線にすると推移が見える
   (§16.4 の `by_age` と同じ手順で x を `year` に替える)。
2. **`rincome` をどう少数カテゴリに畳むか?** — `fct_collapse` で
   "$20000 - 24999"〜"$25000 or more" を "High"、 低額帯を "Low"、 残りを "Other/NA" に
   まとめる。 金額順を壊さないようまとまりごとに括る。
3. **`fct_lump` の例でなぜ 10 でなく 9 群なのか?** — `fct_lump_n(relig, n=10)` の
   まとめ先 "Other" が、 relig に**元からある "Other" 水準**(224 件)と併合されるため。
   結果は「固有 9 + 併合 Other」 になる(上記 §16.5 参照)。

---

前章 → [`15-regexps`](../15-regexps/)。
次章 → [`17-datetimes`](../17-datetimes/)(Ch17 Dates and times)。
