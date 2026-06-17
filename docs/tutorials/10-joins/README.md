# 10. 結合 — Joins

> 一次情報: **R for Data Science 2e, Ch.19 "Joins"**
> <https://r4ds.hadley.nz/joins>
> データ: **nycflights13** の 5 表 — `flights`(全 336,776 行)/ `airlines` /
> `airports`(1,458)/ `planes`(3,322)/ `weather`(26,115)。すべて実データ全量。

複数のデータフレームを **キー**(両表をつなぐ変数)で結合する方法を学びます。
この章で扱う join は 2 系統です。

- **mutating join**(`left_join` / `inner_join` / `right_join` / `full_join`):
  一致する観測から**変数を足す**。
- **filtering join**(`semi_join` / `anti_join`):一致の有無で**行を絞る**。

最後に **non-equi join**(`==` 以外で照合する join)も扱います。実行コードは
[`Joins.hs`](Joins.hs)。

```sh
cd docs/tutorials/10-joins
cabal run tut-10-joins
```

> **この章は表操作が主役です。** R4DS Ch19 の図はすべて join の概念を説明する
> 手描きの**解説イラスト**(ER 図・ドット対応図・Venn 図)で、実データから描く
> ggplot 図は **1 枚もありません**。本章はその概念を散文で説明し、各 join の
> **表出力を実データで忠実に再現**します(概念イラストは plot ライブラリの対象外)。

> **join はなぜ自前実装か。** Hackage `dataframe` にも `leftJoin` / `innerJoin` 等は
> ありますが、重複する非キー列を `These` 型に畳む・`fullOuterJoin` が nullable キーを
> 要求する等、dplyr とは意味論が異なります。R4DS の表出力(`year.x`/`year.y` の
> 曖昧性解消、未一致を `NA` で補填)を忠実再現するため、join を Haskell で自前実装
> しました(CLAUDE.md「機能不足は実装で埋める」)。インデックス計算
> (`leftIdx`/`innerIdx`/`fullIdx`/`rightIdx`/`semiIdx`/`antiIdx`)と
> 列の引き直し(`pickJust`/`pickFlat`)に分けてあります。

---

## 1. キー(Keys)

join には必ず **主キー**(各観測を一意に決める変数)と、別表でそれを指す
**外部キー**が対になって関わります。nycflights13 では:

| 表 | 主キー | 説明 |
|---|---|---|
| `airlines` | `carrier` | 2 文字の航空会社コード |
| `airports` | `faa` | 3 文字の空港コード |
| `planes` | `tailnum` | 機体番号 |
| `weather` | `origin` + `time_hour` | **複合主キー**(場所と時刻) |

外部キーの例:`flights$tailnum` → `planes$tailnum`、`flights$carrier` →
`airlines$carrier`、`flights$origin`/`flights$dest` → `airports$faa`。

### 主キーの検証

主キーが本当に一意かは、キーで `count()` して `n > 1` を探せば確かめられます。

| R | hgg |
|---|---|
| `planes \|> count(tailnum) \|> filter(n>1)` | `counts` で重複キーを数える |
| `weather \|> count(time_hour, origin) \|> filter(n>1)` | 複合キーで同様 |

`planes` も `weather` も重複 **0 件**(主キーとして妥当)。欠損キーも同様に 0 件です。
ただし「重複が無い」だけでは主キーの保証になりません。たとえば `airports` の
`(alt, lat)` は **1 件**の重複があり、主キーには不適です。

### 代理キー

`flights` には主キーがありませんが、`time_hour`・`carrier`・`flight` の 3 つで一意に
決まります(重複 0 件)。とはいえ、行番号による単純な**代理キー**を足すのが扱いやすい:

| R | hgg |
|---|---|
| `flights \|> mutate(id = row_number(), .before = 1)` | `insertVector "id" [1..n]` + `select` で先頭へ |

---

## 2. mutating join — `left_join`

4 つの mutating join のうち、ほぼ常に使うのが `left_join` です。出力は必ず **`x`
(左表)と同じ行**を保ち、一致する右表の変数を**右端に足します**。まず見やすいよう
6 変数に絞った `flights2` を作ります。

```haskell
flights2 = DF.select ["year","time_hour","origin","dest","tailnum","carrier"] flights
```

### メタデータを足す

| R | hgg |
|---|---|
| `flights2 \|> left_join(airlines)` | `leftIdx (carrier) (carrier)` → `name` を `insertVector` |

`carrier` をキーに航空会社名 `name` が右端に付きます。同様に `weather` から気温・
風速を、`planes` から機材情報を足せます。

```
year  time_hour             origin dest tailnum  carrier  name
2013  2013-01-01T10:00:00Z  EWR    IAH  N14228   UA       United Air Lines Inc.
2013  2013-01-01T10:00:00Z  JFK    MIA  N619AA   AA       American Airlines Inc.
...
```

一致しない行は新変数が `NA` になります。たとえば `tailnum == "N3ALAA"` は `planes`
に無いので、`type`・`engines`・`seats` がすべて `Nothing`(= `NA`)。

### キーを明示する — `join_by`

既定の `left_join` は**両表に共通する全変数**をキーにします(**自然結合**)。これが
裏目に出る例:`flights2 |> left_join(planes)` は `year` も `tailnum` も共通なので、
両方を複合キーにしてしまいます。ところが `flights$year`(出発年)と `planes$year`
(製造年)は**意味が違う**ため一致せず、`NA` だらけになります。

`tailnum` だけで結合したいので `join_by(tailnum)` を明示します:

| R | hgg |
|---|---|
| `flights2 \|> left_join(planes, join_by(tailnum))` | キー列だけで `leftIdx`・`year` を `year.x`/`year.y` に改名 |

出力では `year` が `year.x`(flights)と `year.y`(planes)に**曖昧性解消**されます
(R の `suffix` と同じ)。異なる列名どうしのキー指定も `join_by` で書けます:

| R | hgg |
|---|---|
| `left_join(airports, join_by(dest == faa))` | `leftIdx (dest) (faa)` |
| `left_join(airports, join_by(origin == faa))` | `leftIdx (origin) (faa)` |

`dest == faa` 版では、`airports` に無い就航先(`BQN` 等)は `name` が `NA` になります。

---

## 3. filtering join — `semi_join` / `anti_join`

filtering join は変数を足さず、一致の有無で **`x` の行を絞る**だけです。

- **semi_join**:`y` に一致がある `x` の行を残す。
- **anti_join**:`y` に一致が**無い** `x` の行を残す。

| R | hgg |
|---|---|
| `airports \|> semi_join(flights2, join_by(faa == origin))` | `semiIdx (faa) (origin)` → 行を再選択 |
| `airports \|> semi_join(flights2, join_by(faa == dest))` | `semiIdx (faa) (dest)`(就航先 101 空港) |
| `flights2 \|> anti_join(airports, join_by(dest == faa)) \|> distinct(dest)` | `antiIdx` + `nub` |

`faa == origin` の semi_join は出発 3 空港(EWR / JFK / LGA)だけに絞ります。
anti_join は**暗黙の欠損**を見つけるのに便利です。`airports` に無い就航先は 4 つ:

```
dest
BQN    ← Aguadilla (プエルトリコ)
SJU    ← San Juan
STT    ← St. Thomas (米領ヴァージン諸島)
PSE    ← Ponce
```

`planes` に無い機体番号は **722 件**(うち 1 件は `tailnum` 自体が `NA`)。

---

## 4. join の仕組み(How do joins work?)

小さな 2 表 `x`・`y`(キー `key`、値 `val_x`/`val_y`)で各 join の行の動きを見ます。

```
x: key val_x      y: key val_y
   1   x1            1   y1
   2   x2            2   y2
   3   x3            4   y3
```

| join | 残る行 | 結果 |
|---|---|---|
| `inner_join` | 両方にあるキーのみ | `key` 1, 2 |
| `left_join` | `x` を全保持 | 1,2,3(`key=3` の `val_y` = `NA`) |
| `right_join` | `y` を全保持 | 1,2,4(`key=4` の `val_x` = `NA`) |
| `full_join` | `x` か `y` にある全行 | 1,2,3,4(欠けた側が `NA`) |

外部 join(left/right/full)は「どのキーにも一致しなければ一致する仮想行(値は
`NA`)」を相手側に足す、と考えると統一的に理解できます。

### 行の対応は 1 対 1 とは限らない

`x` の 1 行が `y` の**複数行**に一致すると、その行は一致数だけ**複製**されます。
両表でキーが重複すると **多対多**になり、組合せ爆発が起こります:

```
df1: key=1,2,2   df2: key=1,2,2   →   inner_join は 5 行 (key=2 が 2×2)
```

| R | hgg |
|---|---|
| `df1 \|> inner_join(df2, join_by(key))` | `innerIdx` が 1 対多を自然に展開 |

---

## 5. 非等値 join(Non-equi joins)

`==` の代わりに不等号などで照合する join です。等値でないと両キーの値が違うため、
出力には常に**両キー**を残します(`keep = TRUE` 相当・`key.x`/`key.y`)。

### cross join — 全組合せ

`nrow(x) * nrow(y)` 行のデカルト積。名前の全ペア生成(自己結合)に使えます。

| R | hgg |
|---|---|
| `df \|> cross_join(df)` | リスト内包の全組合せ(4×4 = 16 行) |

### 不等号 join

`<`・`<=`・`>=`・`>` で一致集合を絞ります。cross join を不等号で制限すると、
「全**順列**」でなく「全**組**」が得られます:

| R | hgg |
|---|---|
| `df \|> inner_join(df, join_by(id < id))` | `[(i,j) \| a < b]`(6 行) |

### rolling join — 最も近い 1 件

不等号を満たす行の中から**最も近い 1 件**だけを取ります。日付がぴったり揃わない
2 表で「ある日付以前で最も近い日」を探すのに便利です。

四半期パーティの例:各従業員に「誕生日以前で最も近いパーティ」を割り当てます。

| R | hgg |
|---|---|
| `employees \|> left_join(parties, join_by(closest(birthday >= party)))` | 各誕生日に対し `party <= birthday` の最大を取る |

```
name   birthday    q   party
Hazel  2022-01-03  NA  NA          ← 1/10 より前 → パーティ無し
Lily   2022-02-14  1   2022-01-10
Ada    2022-04-04  2   2022-04-04
...
```

1/10 より前の誕生日にはパーティが付きません(`anti_join` で確認できる)。

### overlap join — 期間の重なり

区間どうしを扱う不等号 join のヘルパ(`between` / `within` / `overlaps`)です。

まずパーティに**期間**(`start`〜`end`)を持たせます。自己結合で期間の重なりを
検査すると、入力ミスで Q2 と Q3 が境界で重なっているのが見つかります:

| R | hgg |
|---|---|
| `parties \|> inner_join(parties, join_by(overlaps(start, end, start, end), q < q))` | `sa <= eb && ea >= sb && qa < qb` |

```
start.x     end.x       start.y     end.y
2022-04-04  2022-07-11  2022-07-11  2022-10-02   ← Q2 の終わりと Q3 の始まりが重なる
```

`end` を修正(Q2 を 07-10 に)したうえで、`between` で各従業員をパーティに割り当て
ます。`start` を年初(1/1)から取るので、今度は 1 月初旬の誕生日も漏れません:

| R | hgg |
|---|---|
| `employees \|> inner_join(parties, join_by(between(birthday, start, end)))` | `start <= birthday <= end` |

> **乱数入力についての正直な注記。** R 原文は `set.seed(123)` と `babynames` で
> 従業員 100 名をランダム生成します。R の乱数生成器は外部から完全再現できないため、
> 本章は**代表的な固定ロスター 10 名**で同じ join ロジックを示します。置き換えたのは
> 乱数の入力データだけで、rolling / overlap join の**方式そのものは R4DS と同一**です。

---

## この章で出てきた対応表(まとめ)

| dplyr | hgg |
|---|---|
| `left_join(y)` / `inner_join` / `right_join` / `full_join` | `leftIdx`/`innerIdx`/`rightIdx`/`fullIdx` + 列引き直し |
| `join_by(a == b)` | 異名キーを `leftIdx (a) (b)` |
| `semi_join` / `anti_join` | `semiIdx` / `antiIdx` + 行再選択 |
| `cross_join` | 全組合せ内包 |
| `join_by(a < b)` | 不等号で組を絞る |
| `join_by(closest(a >= b))` | 条件を満たす最大/最小を 1 件 |
| `join_by(between/overlaps(...))` | 区間の包含・重なり判定 |

> **正直な制約。** (1) Ch19 の概念イラスト(ER 図・Venn 図等)は実データ図でないため
> 描画していません(散文で説明)。(2) rolling / overlap の従業員データは R の乱数を
> 再現できないため固定ロスターで代替(join 方式は同一)。(3) `dataframe` の join は
> dplyr と意味論が異なるため join を自前実装(本文冒頭参照)。

前章 → [`09-missing`](../09-missing/)。
次章 → `11-modeling`(R4DS 2e 範囲外・補足)。
