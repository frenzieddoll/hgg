# 15. Regular expressions

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.15 "Regular expressions"**
> <https://r4ds.hadley.nz/regexps>
> Data: **stringr** `words` (980) / `fruit` (80) / `sentences` (720) + **babynames** (1,924,665 rows).

Regular expressions (regex) are a concise, powerful language for describing string patterns. Most `str_*` functions in stringr accept patterns as arguments. This chapter covers

- **Pattern basics** (`str_view` · `.` `?` `+` `*` `|` `[]` `[^]`)
- **Key functions** (`str_detect` / `str_count` / `str_replace` / `separate_wider_regex`)
- **Pattern details** (escaping · anchors `^` `$` `\b` · character classes `\d` `\s` `\w` · quantifiers · groups)
- **Pattern control** (`ignore_case` / `fixed` / `coll`)
- **Practice** (pattern creation with `words` / `babynames`)

Run code: [`Regexps.hs`](Regexps.hs)

```sh
cd docs/tutorials/15-regexps
cabal run tut-15-regexps
```

> **This chapter emphasizes tables and vector output.** The only figure in R4DS Ch15 is an autocomplete screenshot and one time series of "names containing `x`"; this chapter faithfully **reproduces match results as vectors/tables**.

> **Regex is implemented on the analyze side.** stringr/tidyr equivalents are in
> `Hanalyze.Data.Strings`
> regex section (Phase 28 Ch15), with backend as pure Haskell **`regex-tdfa`** (POSIX ERE).
>
> ★**PCRE shorthand handling**: `regex-tdfa` is POSIX ERE, so it doesn't recognize `\d` `\s` `\w`
> (confirmed by testing). This module **internally converts** them to **POSIX classes** (`\d`→`[[:digit:]]` etc.),
> so R's **exact pattern strings** work unchanged. Word boundary `\b` is directly supported by tdfa.
>
> ★**Backreferences `\1` are unsupported** (not in POSIX ERE). §15.4.5 and exercise 15.4.7(6) with `(.)\1\1` etc. are
> **explained conceptually only** (backreferences in replacement strings are self-implemented).
>
> ★**Argument order**: hgg regex functions are **pattern first, string second** (unlike stringr's string first;
> Haskell allows partial application like `map (strDetect pat) xs`).

---

## 15.2 Pattern basics

`str_view()` with a pattern as the second argument surrounds matches with `<>`. This chapter uses equivalent `strViewMatch`. The simplest pattern is **literal characters or digits**, matching exactly:

```
str_view(fruit, "berry"):
[6]  | bil<berry>
[7]  | black<berry>
[10] | blue<berry>
...
[76] | straw<berry>
```

`.` matches **any single character**. `"a...e"` means "a, then 3 chars, then e":

```
str_view(fruit, "a...e"):
[1]  | <apple>
[7]  | bl<ackbe>rry
[48] | mand<arine>
[62] | pine<apple>
[64] | pomegr<anate>
...
```

**Quantifiers** control repetition of the preceding element. `?` (0–1 times) · `+` (1+ times) ·
`*` (0+ times). **Alternation** is `|`, **character classes** are `[abc]` (any) / `[^abc]` (not):

| R | hgg | Meaning |
|---|---|---|
| `str_view(x, "an")` | `strViewMatch "an" x` | Consecutive "an" |
| `str_view(x, "a.")` | `strViewMatch "a." x` | a + any 1 char |
| `str_view(x, "a\|e")` | `strViewMatch "a\|e" x` | a or e |

```
str_view(c("apple","pair","banana"), "an"):   [3] | b<an><an>a
str_view(..., "a."):   [1] | <ap>ple   [2] | p<ai>r   [3] | b<an><an>a
str_view(..., "a|e"):  [1] | <a>ppl<e>  [2] | p<a>ir   [3] | b<a>n<a>n<a>
```

---

## 15.3 Key functions

### 15.3.1 Detection `str_detect()`

`str_detect()` returns a **logical vector** of whether patterns match. Combine with `filter()` /
`count()` / `mutate()`:

```
str_detect(c("apple","banana","pear"), "p") = [True, False, True]
```

In babynames, count names containing `x` by headcount (`wt = n`) (**974 names**, case-sensitive):

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

(Exactly matches R4DS. `str_detect` is **case-sensitive** by default, so uppercase "Xavier" doesn't appear.)

### 15.3.2 Count `str_count()`

`str_count()` returns **count of matches** within a single string (non-overlapping). For name vowels/consonants,
use `[aeiou]` / `[^aeiou]` (after lowercasing):

```
babynames |> mutate(vowels = str_count(name, "[aeiou]"), consonants = ...):  (first 5 names)
  name       vowels consonants
  Mary       1      3
  Anna       2      2
  Emma       2      2
  Elizabeth  4      5
  Minnie     3      3
```

### 15.3.3 Replacement `str_replace()` / `str_replace_all()`

Replaces matches. Backreferences `\1`..`\9` in the replacement refer to **capture groups**.

| R | hgg | Result |
|---|---|---|
| `str_replace_all("a-b-c", "-", "+")` | `strReplaceAll "-" "+" "a-b-c"` | `a+b+c` |
| `str_replace_all("hello", "[aeiou]", "-")` | `strReplaceAll "[aeiou]" "-" "hello"` | `h-ll-` |
| `str_replace_all("abcd", "([a-z])([a-z])", "\\2\\1")` | `strReplaceAll "([a-z])([a-z])" "\\2\\1" "abcd"` | `badc` (swap adjacent 2) |

### 15.3.4 Column extraction `separate_wider_regex()`

Splits one column into multiple using named groups. List `(name, sub-pattern)` pairs; `Nothing`-named parts are discarded:

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

## 15.4 Pattern details

### 15.4.1 Escaping

To match metacharacters (`. ^ $ | ( ) [ ] { } * + ? \`) literally, escape with `\`. In Haskell strings, `\` itself is escaped, so `\.` (literal dot) is written as `"\\."`. To safely build patterns from literal strings, use `strEscape` (§15.6).

### 15.4.2 Anchors `^` `$` `\b`

`^` matches **start** of string, `$` matches **end** (single-line mode, R's default).
`\b` matches **word boundary**. Demo with `words` (980 words):

```
str_subset(words, "^y")  (starts with y) = ["year","yes","yesterday","yet","you","young"]
str_subset(words, "x$")  (ends with x)   = ["box","sex","six","tax"]

str_detect("the cat sat", "\\bcat\\b") = True
str_detect("category",    "\\bcat\\b") = False    (cat is part of word, no boundary match)
```

### 15.4.3 Character classes `[...]` and shorthands `\d` `\s` `\w`

`[abc]` (any) · `[^abc]` (not any) · `[a-z]` (range). Shorthands `\d` (digit) · `\s` (space) ·
`\w` (word char) are **internally converted to POSIX classes** and work:

```
str_detect("abc123", "\\d")        = True     (\d → [[:digit:]])
str_extract("order 42 ok", "\\d+") = Just "42"
```

### 15.4.4 Quantifiers `{n}` `{n,}` `{n,m}`

`{n}` (exactly n times) · `{n,}` (n+ times) · `{n,m}` (n to m times).

```
str_subset(words, "^...$")   (exactly 3 chars) = 110 words  (e.g., act, add, age, ago, air, all, and, any)
str_subset(words, "[a-z]{7,}")  (7+ chars)    = 219 words
```

### 15.4.5 Groups `()` and backreferences

`()` creates a **capture group**, extractable with `str_match()`:

```
str_match("2026-06-19", "(\\d{4})-(\\d{2})-(\\d{2})")
  = [Just "2026-06-19", Just "2026", Just "06", Just "19"]    (whole match first, then groups)
```

> ★**Backreference `\1` note**: R/PCRE allows **within-pattern** references like `(.)\1\1` (same char 3 times).
> Our `regex-tdfa` is **POSIX ERE** (no backreferences), so these are **conceptual only**
> (replacement string backreferences in `strReplace` are self-implemented).
> "Find repeating chars" tasks can use Haskell-side logic (adjacent comparison) if needed.

---

## 15.5 Pattern control

Flags in `regex()` change match behavior. Most common: `ignore_case`:

```
str_detect("Banana", "banana")                              = False
str_detect("Banana", regex("banana", ignore_case = TRUE))   = True
   ↑ hgg: strDetectWith True "banana" "Banana"
```

> **`fixed()` / `coll()` note**: `fixed()` treats pattern as **literal string** (disables regex);
> `coll()` does **locale collation** (language-aware case/accent equivalence). This repository doesn't adopt ICU
> (locale collation) per Ch14, so `coll()` is conceptual. For literal matching, use `strEscape`
> to neutralize metacharacters and fall back to normal matching.

---

## 15.6 Practice — Building patterns from code

With `strEscape` converting literal strings to safe pattern fragments, and `strFlatten "|"` joining alternatives,
you can **generate patterns from data**:

```
str_escape("a.b+c")  = a\.b\+c

Generate from code: str_flatten("|", str_escape(c("apple","banana","pear")))  = apple|banana|pear
str_subset(c("apple pie","grape","pear tart"), above)  = ["apple pie","pear tart"]
```

"Single regex" and "multiple `str_detect` combinations" are often equivalent:

```
str_subset(words, "^x|x$")                       = ["box","sex","six","tax"]
filter str_detect "^x" OR str_detect "x$"        = ["box","sex","six","tax"]   (same result)
```

---

## 15.7 Regex in other contexts

Regex works beyond stringr.

- **tidyr**: `separate_wider_regex()` (§15.3.4).
- **R's `matches()` / `pivot_longer(names_pattern=)`**: **Select/split column names by regex**.
  This repository applies `strDetect` to column name lists (e.g., `filter (strDetect "^x_") columnNames`).
- **base R `list.files(pattern=)` / `apropos()`**: regex filtering of filenames/object names.
  Haskell: apply `strSubset` to fetched lists for equivalence.

---

## 15.8 Summary

This chapter covered regex **basics** (`. ? + * | [] [^]`), **key functions** (`str_detect` / `str_count` /
`str_replace` / `separate_wider_regex`), **details** (anchors · character classes · quantifiers · groups),
**control** (`ignore_case` etc.). tdfa (POSIX ERE) constraints (no backreferences · no `coll`) are honestly documented. Next chapter covers **factors**.

---

## Exercises

> Reproduce R4DS Ch15 exercises (15.3.5 · 15.4.7 · 15.6.4 = 15 questions total) in hgg.

### 15.3.5

**(1)** Which name has the most vowels? Which has the highest vowel **ratio**? (Hint: denominator?)

> **Answer (approach)**: `vowels = strCount "[aeiou]" (toLower name)`, ratio = `vowels / strLength name`.
> Most vowels appear in long names (e.g., "Mariadelosangeles"); highest ratio appears in short all-vowel names
> (e.g., "Aoi"/"Ea", ratio 1.0). Key: denominator is **name length** (`str_length`).

**(2)** Replace all `/` in `"a/b/c/d/e"` with `\`. What happens if you try to convert `\` back to `/`?

> **Answer**: `strReplaceAll "/" "\\\\" "a/b/c/d/e"` yields `a\b\c\d\e`. Converting back: `\` in a pattern is
> an escape character, so `"\\"` (one backslash) means "escape next char" but has no next char, creating an **invalid pattern**.
> To match literal `\`, the pattern needs `\\` (regex), which in Haskell becomes `"\\\\"` (4 backslashes).
> This "escaping hell" is §15.4.1's subject.

**(3)** Implement a minimal `str_to_lower()` using `str_replace_all()`.

> **Answer**: Individually replace each uppercase with lowercase. Example:
> `foldr (\(u,l) -> strReplaceAll u l) name (zip ["A".."Z"] ["a".."z"])`.
> Char-by-char is inefficient; use `strToUpper`/`T.toLower` in practice (this is exercise pedagogy).

**(4)** Create a regex matching phone numbers as commonly written in your country.

> **Answer (Japan example)**: `\(?0\d{1,4}\)?[-\s]?\d{1,4}[-\s]?\d{4}` (area code 0 · optional parens/dashes/spaces).
> hgg: `strDetect "0[0-9]{1,4}[- ]?[0-9]{1,4}[- ]?[0-9]{4}" tel`.

### 15.4.7

**(1)** Match literal string `'\` or `$^$`.

> **Answer**: `'\` becomes `"'\\\\"`  (single quote + escaped backslash). `$^$` becomes `"\\$\\^\\$"` (escape each metachar).
> `strEscape "$^$"` also returns `\$\^\$`.

**(2)** Explain why these patterns don't match `\`: `""`, `"\\"``, `"\\\\"`.

> **Answer**: `""` is empty (nothing specified). `"\\"` is regex for "one backslash" = "escape next char",
> but nothing follows → invalid. To match literal `\`, regex needs `\\`, which in Haskell becomes `"\\\\"` (4 backslashes).

**(3)** From `words`, build regexes matching:
  1. Start with "y" → `^y` → `["year","yes","yesterday","yet","you","young"]`
  2. Don't start with "y" → `^[^y]`
  3. End with "x" → `x$` → `["box","sex","six","tax"]`
  4. Exactly 3 chars (no `str_length`) → `^...$` or `^[a-z]{3}$` → 110 words
  5. 7+ chars → `[a-z]{7,}` or `.......` → 219 words
  6. Contains vowel→consonant sequence → `[aeiou][^aeiou]`
  7. Has vowel→consonant sequence twice → `([aeiou][^aeiou]){2}`
  8. Composed only of vowel→consonant sequences → `^([aeiou][^aeiou])+$`

> All extractable via `strSubset pattern words` (1, 3, 4, 5 confirmed with real values in §15.4).

**(4)** Build shortest regex for 11 British/American spelling pairs (airplane/aeroplane etc.).

> **Answer (examples)**: `a(ir|ero)plane` · `alumin(i?)um` → `alumini?um` · `analog(ue)?` · `ar?se` ·
> `cent(er|re)` · `defen[cs]e` · `do(ugh)?nut` · `gr[ae]y` · `modell?ing` · `s[kc]eptic` · `summari[sz]e`.
> Keep common parts, express differences with `()` / `?` / `[...]`.

**(5)** Swap first and last character of each word in `words`. Which swapped versions remain in `words`?

> **Answer (approach)**: `swap w = strSub (-1) (-1) w <> strSub 2 (strLength w - 1) w <> strSub 1 1 w`;
> `filter (`elem` words) (map swap words)`. Palindromic words and pairs that swap to other real words
> (e.g., "war"↔"raw") remain.

**(6)** Explain what each pattern (note: regex vs. string encoding) matches:

> 1. `^.*$` … Any full line (including empty).
> 2. `"\\{.+\\}"` … Content between `{` and `}` (**1+ chars**, literal braces).
> 3. `\d{4}-\d{2}-\d{2}` … Date in `YYYY-MM-DD` format.
> 4. `"\\\\{4}"` … Literal backslash **4 times** consecutively.
> 5. `\..\..\..` … "Literal dot + any char" repeated 3 times (e.g., `.a.b.c`).
> 6. `(.)\1\1` … **Same char 3 times** (★backreference; tdfa doesn't support; conceptual only).
> 7. `"(..)\\1"` … "Any 2 chars" repeated (e.g., `abab`) (★backreference; unsupported; conceptual).

**(7)** Solve the beginner regex crossword at <https://regexcrossword.com/challenges/beginner>.

> **Answer**: External exercise. Use `strDetect` in this repository to verify candidates (self-study).

### 15.6.4

**(1)** Solve in both "single regex" and "multiple `str_detect` combinations":
  1. Starts with x or ends with x → `^x|x$` / (`strDetect "^x"` or `strDetect "x$"`) → `["box","sex","six","tax"]`
  2. Starts with vowel, ends with consonant → `^[aeiou].*[^aeiou]$` / (both `strDetect` AND)
  3. Contains all different vowels (a,e,i,o,u)? → Single regex tedious; clearer:
     `all (\v -> strDetect v w) ["a","e","i","o","u"]` combination.

> (1) confirmed with real values in §15.6 matching both approaches.

**(2)** Find **supporting** and **counterexample** patterns for "i before e except after c" rule.

> **Answer**: Support (ie not after c) = `[^c]ie`; violations = `cie` (c then ie) or `ei` (ei anywhere).
> Comparing `strSubset "cie" words` vs `strSubset "[^c]ei" words` shows English has many exceptions,
> making the rule unreliable.

**(3)** Auto-detect modifiers like "lightgray"/"darkblue" in `colors()`.

> **Answer (approach)**: Detect known prefixes `^(light|dark|medium|...)` with `strDetect`,
> strip with `strRemove`, check if remainder is in base color set → classify as modified.

**(4)** Build regex matching any base R dataset name (strip `(...)` grouping).

> **Answer (approach)**: For each dataset item, remove `" \\(.*\\)$"` (trailing grouping) with `strRemove`.
> Filter to identifier form with `strDetect "^[A-Za-z][A-Za-z0-9.]*$"`.
