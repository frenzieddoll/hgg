# 15. 正規表現 — Regular expressions

> 一次情報: **R for Data Science 2e, Ch.15 "Regular expressions"**
> <https://r4ds.hadley.nz/regexps>
> データ: **stringr** の `words`(980)/ `fruit`(80)/ `sentences`(720)+ **babynames**(1,924,665 行)。

正規表現(regex)は文字列のパターンを記述する簡潔で強力な言語で、stringr の `str_*`
関数のほとんどがパターン引数として受け取ります。本章では

- **パターンの基礎**(`str_view`・`.` `?` `+` `*` `|` `[]` `[^]`)
- **主要関数**(`str_detect` / `str_count` / `str_replace` / `separate_wider_regex`)
- **パターン詳細**(エスケープ・アンカー `^` `$` `\b`・文字クラス `\d` `\s` `\w`・量指定子・グループ)
- **パターン制御**(`ignore_case` / `fixed` / `coll`)
- **実践**(`words` / `babynames` でのパターン作成)

を学びます。実行コードは [`Regexps.hs`](Regexps.hs)。

```sh
cd docs/tutorials/15-regexps
cabal run tut-15-regexps
```

> **この章は表とベクトル出力が主役です。** R4DS Ch15 の唯一の図はオートコンプリートの
> スクリーンショットと「`x` を含む名前の割合」の時系列 1 枚だけで、本章はパターンの効果を
> **マッチ結果のベクトル/表**で忠実再現します。

> **regex は analyze 側に実装。** stringr/tidyr 相当は
> `Hanalyze.Data.Strings`
> の regex 節(Phase 28 Ch15)に実装済で、バックエンドは純 Haskell の **`regex-tdfa`**(POSIX ERE)です。
>
> ★**PCRE ショートハンドの扱い**: `regex-tdfa` は POSIX ERE ゆえ `\d` `\s` `\w` を解しません
> (実測)。本モジュールはこれらを **POSIX クラス**(`\d`→`[[:digit:]]` 等)に内部変換するので、
> R と**同じパターン文字列**がそのまま動きます。単語境界 `\b` は tdfa が直接対応します。
>
> ★**後方参照 `\1` は POSIX ERE に無く非対応**です。§15.4.5 と演習 15.4.7(6) の `(.)\1\1` 等は
> **概念のみ**説明します(置換文字列の中の `\1`=キャプチャ参照は自前実装で対応済)。
>
> ★**引数順**: hgg の regex 関数は **pattern 先・string 後**です(stringr は string 先ですが、
> Haskell では `map (strDetect pat) xs` のように部分適用しやすいため)。

---

## 15.2 パターンの基礎

`str_view()` は 2 番目の引数にパターンを渡すと、マッチ部分を `<>` で囲んで見せます。
本章では同等の `strViewMatch` で再現します。最も単純なパターンは**文字や数字の並び**で、
そのまま完全一致を探します。

```
str_view(fruit, "berry"):
[6]  | bil<berry>
[7]  | black<berry>
[10] | blue<berry>
...
[76] | straw<berry>
```

`.` は**任意の 1 文字**にマッチします。`"a...e"` は「a の 3 文字あとに e」:

```
str_view(fruit, "a...e"):
[1]  | <apple>
[7]  | bl<ackbe>rry
[48] | mand<arine>
[62] | pine<apple>
[64] | pomegr<anate>
...
```

**量指定子**は直前要素の繰り返し回数を制御します。`?`(0–1 回)・`+`(1 回以上)・
`*`(0 回以上)。**選択**は `|`、**文字クラス**は `[abc]`(いずれか)/ `[^abc]`(以外):

| R | hgg | 意味 |
|---|---|---|
| `str_view(x, "an")` | `strViewMatch "an" x` | 連続する "an" |
| `str_view(x, "a.")` | `strViewMatch "a." x` | a + 任意 1 文字 |
| `str_view(x, "a\|e")` | `strViewMatch "a\|e" x` | a または e |

```
str_view(c("apple","pair","banana"), "an"):   [3] | b<an><an>a
str_view(..., "a."):   [1] | <ap>ple   [2] | p<ai>r   [3] | b<an><an>a
str_view(..., "a|e"):  [1] | <a>ppl<e>  [2] | p<a>ir   [3] | b<a>n<a>n<a>
```

---

## 15.3 主要関数

### 15.3.1 検出 `str_detect()`

`str_detect()` はパターンにマッチするかの**論理ベクトル**を返します。`filter()` /
`count()` / `mutate()` と組み合わせるのが基本です。

```
str_detect(c("apple","banana","pear"), "p") = [True, False, True]
```

babynames で「`x` を含む名前」を人数(`wt = n`)で集計(**974 名**・case-sensitive):

```
babynames |> filter(str_detect(name, "x")) |> count(name, wt = n, sort = TRUE):
  name           n
  Alexander 665,492
  Alexis    399,551
  Alex      278,705
  Alexandra 232,223
  Max       148,787
  Alexa     123,032
```

(R4DS と完全一致。`str_detect` は既定で **case-sensitive** なので、大文字始まりの "Xavier" は
ここに入りません。)

### 15.3.2 カウント `str_count()`

`str_count()` は 1 文字列内のマッチ**回数**を返します(重複しない)。名前の母音/子音数を
`[aeiou]` / `[^aeiou]` で(大小無視のため小文字化してから):

```
babynames |> mutate(vowels = str_count(name, "[aeiou]"), consonants = ...):  (先頭 5 名)
  name       vowels consonants
  Mary       1      3
  Anna       2      2
  Emma       2      2
  Elizabeth  4      5
  Minnie     3      3
```

### 15.3.3 置換 `str_replace()` / `str_replace_all()`

マッチを置換します。置換文字列の `\1`..`\9` は**キャプチャグループ参照**です。

| R | hgg | 結果 |
|---|---|---|
| `str_replace_all("a-b-c", "-", "+")` | `strReplaceAll "-" "+" "a-b-c"` | `a+b+c` |
| `str_replace_all("hello", "[aeiou]", "-")` | `strReplaceAll "[aeiou]" "-" "hello"` | `h-ll-` |
| `str_replace_all("abcd", "([a-z])([a-z])", "\\2\\1")` | `strReplaceAll "([a-z])([a-z])" "\\2\\1" "abcd"` | `badc`(隣接 2 文字を入替) |

### 15.3.4 列抽出 `separate_wider_regex()`

名前付きグループで 1 列を複数列に分割します。`(名前, 部分パターン)` を順に並べ、名前なし
(`Nothing`)の部分は捨てます:

```
separate_wider_regex(str, c("<", name="[A-Za-z]+", ">-", gender="[A-Z]", "-", age="[0-9]+")):
 name    | gender | age
---------|--------|-----
 Sheryl  | F      | 34
 Kisha   | F      | 45
 Brandon | N      | 33
 Sharon  | F      | 38
 Penny   | F      | 58
```

hgg:

```haskell
separateWiderRegex "str"
  [ (Nothing, "<"), (Just "name", "[A-Za-z]+"), (Nothing, ">-")
  , (Just "gender", "[A-Z]"), (Nothing, "-"), (Just "age", "[0-9]+") ] df
```

---

## 15.4 パターン詳細

### 15.4.1 エスケープ

メタ文字(`. ^ $ | ( ) [ ] { } * + ? \`)をリテラルとして探すには `\` でエスケープします。
Haskell 文字列リテラルでは `\` 自体もエスケープが要るので、`\.`(リテラルのドット)は
ソース上 `"\\."` と書きます。リテラル文字列からパターンを安全に作るには `strEscape`(§15.6)。

### 15.4.2 アンカー `^` `$` `\b`

`^` は文字列の**先頭**、`$` は**末尾**にマッチします(本実装は R 既定の single-line)。
`\b` は**単語境界**です。`words`(980 語)で実演:

```
str_subset(words, "^y")  (y で始まる) = ["year","yes","yesterday","yet","you","young"]
str_subset(words, "x$")  (x で終わる) = ["box","sex","six","tax"]

str_detect("the cat sat", "\\bcat\\b") = True
str_detect("category",    "\\bcat\\b") = False    (cat が単語の一部なので非マッチ)
```

### 15.4.3 文字クラス `[...]` と短縮形 `\d` `\s` `\w`

`[abc]`(いずれか)・`[^abc]`(以外)・`[a-z]`(範囲)。短縮形 `\d`(数字)・`\s`(空白)・
`\w`(単語文字)は **POSIX クラスに内部変換**されて動きます:

```
str_detect("abc123", "\\d")        = True     (\d → [[:digit:]])
str_extract("order 42 ok", "\\d+") = Just "42"
```

### 15.4.4 量指定子 `{n}` `{n,}` `{n,m}`

`{n}`(ちょうど n 回)・`{n,}`(n 回以上)・`{n,m}`(n〜m 回)。

```
str_subset(words, "^...$")   (ちょうど 3 文字) = 110 語  (例 act, add, age, ago, air, all, and, any)
str_subset(words, "[a-z]{7,}")  (7 文字以上)  = 219 語
```

### 15.4.5 グループ `()` と後方参照

`()` は**キャプチャグループ**を作り、`str_match()` で取り出せます:

```
str_match("2026-06-19", "(\\d{4})-(\\d{2})-(\\d{2})")
  = [Just "2026-06-19", Just "2026", Just "06", Just "19"]    (先頭=whole match、以降=各 group)
```

> ★**後方参照 `\1` の注記**: R/PCRE では `(.)\1\1`(同じ文字が 3 回)のように、**パターン内**で
> 前のグループを参照できます。本実装の `regex-tdfa` は **POSIX ERE** で後方参照を持たないため、
> この用法は**概念のみ**の紹介とします(置換文字列内の `\1` 参照は `strReplace` で対応済)。
> 「重なる文字を探す」等の課題は、必要なら Haskell 側のロジック(隣接比較)で代替できます。

---

## 15.5 パターン制御

`regex()` のフラグでマッチ挙動を変えます。最頻出は `ignore_case`:

```
str_detect("Banana", "banana")                              = False
str_detect("Banana", regex("banana", ignore_case = TRUE))   = True
   ↑ hgg: strDetectWith True "banana" "Banana"
```

> **`fixed()` / `coll()` の注記**: `fixed()` はパターンを**リテラル文字列**として扱い(regex を
> 無効化)、`coll()` は**ロケール照合**(言語ごとの大小・アクセント同一視)を行います。本リポジトリは
> Ch14 同様 ICU(locale 照合)を採用していないため、`coll()` は概念注記にとどめます。リテラル一致が
> 必要なら `strEscape` でメタ文字を無害化して通常マッチに帰着できます。

---

## 15.6 実践 — パターンをコードで作る

`strEscape` でリテラル文字列を安全なパターン片にし、`strFlatten "|"` で選択肢に連結すると、
**データからパターンを生成**できます:

```
str_escape("a.b+c")  = a\.b\+c

コードで生成: str_flatten("|", str_escape(c("apple","banana","pear")))  = apple|banana|pear
str_subset(c("apple pie","grape","pear tart"), 上記)  = ["apple pie","pear tart"]
```

「単一の regex」と「複数の `str_detect` の組合せ」はしばしば等価です:

```
str_subset(words, "^x|x$")                       = ["box","sex","six","tax"]
filter str_detect "^x" OR str_detect "x$"        = ["box","sex","six","tax"]   (同じ結果)
```

---

## 15.7 正規表現が使える他の場所

正規表現は stringr 以外でも使えます。

- **tidyr**: `separate_wider_regex()`(§15.3.4 で実演)。
- **R の `matches()` / `pivot_longer(names_pattern=)`**: **列名を regex で選ぶ/分解する**用途。
  本リポジトリでは、列名のリストに `strDetect` を適用して選別できます
  (例: `filter (strDetect "^x_") columnNames`)。
- **base R の `list.files(pattern=)` / `apropos()`**: ファイル名・オブジェクト名の regex 絞り込み。
  Haskell でも取得したリストに `strSubset` を適用すれば同等です。

---

## 15.8 まとめ

本章では正規表現の**基礎**(`. ? + * | [] [^]`)、**主要関数**(`str_detect` / `str_count` /
`str_replace` / `separate_wider_regex`)、**詳細**(アンカー・文字クラス・量指定子・グループ)、
**制御**(`ignore_case` 等)を扱いました。tdfa(POSIX ERE)の制約(後方参照・`coll` 非対応)は
正直に注記しています。次章では**因子(factor)**を学びます。

---

## 演習

> R4DS Ch15 の演習(15.3.5・15.4.7・15.6.4 = 計 15 問)を hgg で再現します。

### 15.3.5

**(1)** 最も母音が多い名前は? 母音の**割合**が最も高い名前は?(ヒント: 分母は?)

> **答(方針)**: `vowels = strCount "[aeiou]" (toLower name)`、割合は `vowels / strLength name`。
> 母音数の最大は長い名前(例 "Mariadelosangeles" 等)に出やすく、割合の最大は短く母音だけの名前
> (例 "Aoi"/"Ea" 等、割合 1.0)になる。分母は**名前の文字数**(`str_length`)である点が肝。

**(2)** `"a/b/c/d/e"` の `/` をすべて `\` に置換せよ。逆に `\` を `/` に戻そうとすると何が起きるか?

> **答**: `strReplaceAll "/" "\\\\" "a/b/c/d/e"` で `a\b\c\d\e`。逆変換で `\` をパターンに使うと
> `\` 自身がエスケープ文字なので、`"\\"`(バックスラッシュ 1 個)は「次の文字をエスケープ」と
> 解釈され**不正パターン**になりがち。リテラルの `\` を探すにはパターン側で `"\\\\"` と二重に要る
> (この「エスケープ地獄」が次の §15.4.1 の主題)。

**(3)** `str_replace_all()` で `str_to_lower()` の簡易版を実装せよ。

> **答**: 各大文字を対応する小文字に個別置換する。例:
> `foldr (\(u,l) -> strReplaceAll u l) name (zip ["A".."Z"] ["a".."z"])`。
> ただし文字単位なので `strToUpper`/`T.toLower` の方が実務的(これは演習用の原理確認)。

**(4)** 自国で一般的な表記の電話番号にマッチする正規表現を作れ。

> **答(日本の例)**: `\(?0\d{1,4}\)?[-\s]?\d{1,4}[-\s]?\d{4}`(市外局番 0 始まり・任意の括弧/ハイフン/空白)。
> hgg では `strDetect "0[0-9]{1,4}[- ]?[0-9]{1,4}[- ]?[0-9]{4}" tel`。

### 15.4.7

**(1)** リテラル文字列 `'\` や `$^$` にマッチするには?

> **答**: `'\` は `"'\\\\"`(`'` + エスケープした `\`)。`$^$` は各メタ文字をエスケープして
> `"\\$\\^\\$"`。`strEscape "$^$"` でも `\$\^\$` が得られる。

**(2)** 次のパターンが `\` にマッチしない理由を説明せよ: `""`、`"\\"`、`"\\\\"`。

> **答**: `""` は空(何も指定なし)。`"\\"` は regex 的に「バックスラッシュ 1 個」=「次をエスケープ」
> だが続きが無く不正/未完。リテラルの `\` 1 個にマッチさせるには **regex として `\\`** が要り、
> それを Haskell リテラルで書くと `"\\\\"`(4 つの `\`)になる。

**(3)** `words` から次にマッチする正規表現を作れ:
  1. "y" で始まる → `^y` → `["year","yes","yesterday","yet","you","young"]`
  2. "y" で始まらない → `^[^y]`
  3. "x" で終わる → `x$` → `["box","sex","six","tax"]`
  4. ちょうど 3 文字(`str_length` を使わずに) → `^...$`(または `^[a-z]{3}$`)→ 110 語
  5. 7 文字以上 → `[a-z]{7,}`(または `.......`)→ 219 語
  6. 母音→子音の並びを含む → `[aeiou][^aeiou]`
  7. 母音→子音 の並びが 2 回連続 → `([aeiou][^aeiou]){2}`
  8. 母音→子音 の並びだけで構成される → `^([aeiou][^aeiou])+$`

> いずれも `strSubset パターン words` で抽出できる(1・3・4・5 は本文 §15.4 で実値確認済)。

**(4)** 英米綴りの 11 語に各々マッチする最短 regex を作れ(airplane/aeroplane など)。

> **答(例)**: `a(ir|ero)plane`・`alumin(i?)um` → `alumini?um`・`analog(ue)?`・`ar?se`・
> `cent(er\|re)` → `cent(er|re)`・`defen[cs]e`・`do(ugh)?nut`・`gr[ae]y`・`model?ling` →
> `modell?ing`・`s[kc]eptic`・`summari[sz]e`。共通部を残し差分だけ `(...)`/`?`/`[...]` で表す。

**(5)** `words` の各語の最初と最後の文字を入れ替えよ。入替後も `words` に含まれるのは?

> **答(方針)**: `swap w = strSub (-1) (-1) w <> strSub 2 (strLength w - 1) w <> strSub 1 1 w`、
> `filter (`elem` words) (map swap words)`。回文的な語や、入替で別の実在語になる対
> (例 "war"↔"raw")が残る。

**(6)** 次の各パターン(regex かそれを表す文字列かに注意)が何にマッチするか言葉で説明せよ。

> 1. `^.*$` … 任意の 1 行全体(空文字も)。
> 2. `"\\{.+\\}"` … `{` と `}` に囲まれた**1 文字以上**の並び(リテラルの波括弧)。
> 3. `\d{4}-\d{2}-\d{2}` … `YYYY-MM-DD` 形式の日付。
> 4. `"\\\\{4}"` … リテラルのバックスラッシュ **4 個**連続。
> 5. `\..\..\..` … 「リテラルのドット + 任意 1 文字」が 3 回(例 `.a.b.c`)。
> 6. `(.)\1\1` … **同じ文字が 3 回連続**(★後方参照ゆえ tdfa 非対応・概念のみ)。
> 7. `"(..)\\1"` … 「任意 2 文字」が**そのまま繰り返される**(例 `abab`・★後方参照ゆえ非対応・概念のみ)。

**(7)** <https://regexcrossword.com/challenges/beginner> の初級クロスワードを解け。

> **答**: 外部サイトの演習。本リポジトリの `strDetect` で各候補を検証しながら解ける(self-study)。

### 15.6.4

**(1)** 次を「単一の regex」と「複数の `str_detect` の組合せ」の両方で解け。
  1. x で始まる or 終わる → `^x|x$` /(`strDetect "^x"` または `strDetect "x$"`)→ `["box","sex","six","tax"]`
  2. 母音で始まり子音で終わる → `^[aeiou].*[^aeiou]$` /(両 `strDetect` の AND)
  3. 異なる母音をすべて(a,e,i,o,u)含む語はあるか → 単一 regex は煩雑なので
     `all (\v -> strDetect v w) ["a","e","i","o","u"]` の組合せが明快。

> (1) は本文 §15.6 で両方の結果が一致することを実値確認済。

**(2)** 「i before e except after c」の規則の**支持/反例**を探すパターンを作れ。

> **答**: 支持(ie で c の後でない)= `[^c]ie`、規則違反の候補 = `cie`(c の後に ie)や `ei`
> (ei が現れる)。`strSubset "cie" words` と `strSubset "[^c]ei" words` の件数を比べると、
> 英語では例外が多く規則が万能でないことが見える。

**(3)** `colors()` の "lightgray"/"darkblue" のような修飾語を自動検出するには?

> **答(方針)**: 既知の修飾接頭辞 `^(light|dark|medium|...)` を `strDetect` で検出し、
> `strRemove "^(light|dark|medium)"` で剥がして素の色名を得る。修飾を剥がした結果が
> 素の色名集合に含まれるかで「修飾版」と判定する。

**(4)** base R の任意のデータセット名にマッチする regex を作れ(`(...)` 内のグルーピング名を剥がす)。

> **答(方針)**: データセット一覧の各項目について、`" \\(.*\\)$"`(末尾の `(grouping)`)を
> `strRemove` で除去してから名前を取る。`strDetect "^[A-Za-z][A-Za-z0-9.]*$"` 等で識別子形を絞る。
