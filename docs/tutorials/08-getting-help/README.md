# 08. Getting Help

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.8 "Workflow: getting help"**
> <https://r4ds.hadley.nz/workflow-help>
> Data: None (procedural chapter without diagrams). Demonstration code in [`GettingHelp.hs`](GettingHelp.hs).

Chapter 8 of R4DS is a **diagram-free** procedural chapter closing the "Whole Game" part. It covers
three main topics:

1. **Google is your friend** — Using search (especially error messages) and Stack Overflow.
2. **Making a reprex** — How to create a minimal, **repr**oducible **ex**ample.
3. **Investing in yourself** — Daily learning and following the community.

All are **R / RStudio / tidyverse-specific advice**, so rather than approximation or substitution, we
provide an **honest mapping from R practices to Haskell equivalents**. Only the executable parts
(reprex example) are demonstrated in [`GettingHelp.hs`](GettingHelp.hs).

```sh
cd docs/tutorials/08-getting-help
cabal run tut-08-getting-help
```

**Note**: This file itself is an example **reprex as described in R4DS §8.2** (imports grouped at top,
data embedded inline, only essential code for the problem, written minimally).

---

## §8.1 Google is your friend

R: When stuck, start with Google. Add "R" to your query to narrow to R-related results. Add package
names like "tidyverse" / "ggplot2" to reach familiar code more easily. **Error message search** is
especially effective. Non-English errors can be searched after setting them to English with
`Sys.setenv(LANGUAGE = "en")`. If Google doesn't help, search on **Stack Overflow** with the `[R]`
tag.

Haskell equivalents:

| R / tidyverse advice | Haskell equivalent |
|---|---|
| Add "R" to query | Add "Haskell" to query |
| Further narrow with "ggplot2" / "tidyverse" | Further narrow with "dataframe" / "hgg" / library names |
| Search error message as-is | Search GHC error/warning messages as-is |
| To learn function usage | **Hoogle** (<https://hoogle.haskell.org>) for type/function name search |
| Package documentation | **Hackage** (<https://hackage.haskell.org>) haddock |
| Help with `?function` | GHCi `:doc function`, `:info function`, `:type function` |
| Stack Overflow `[R]` tag | Stack Overflow `[haskell]` tag |

**Hoogle** is a powerful R-equivalent-less search: **reverse-lookup functions from type signatures**
(e.g., searching `(a -> b) -> [a] -> [b]` returns `map`). It's the standard approach when you know
the transformation type but forget the function name. Error message search works as well in Haskell;
GHC defaults to English, so language switching (R's `Sys.setenv` equivalent) isn't needed.

---

## §8.2 Making a reprex

R: When a search doesn't yield results, creating a **reprex** (minimal **repr**oducible **ex**ample)
is good practice. A reprex has two requirements:

1. **Reproducible**: **Include all necessary** `library()` calls and object creation. The tidyverse
   `reprex` package helps catch omissions.
2. **Minimal**: **Remove everything** not directly related to the problem. Replace real data with
   smaller, simpler objects (or built-in data).

R4DS shows `reprex::reprex()`, which formats code **and its output** in `#>`-prefixed Markdown and
copies to clipboard:

```r
# R
y <- 1:4
mean(y)
#> [1] 2.5
```

### Haskell equivalent = self-contained minimal code + its output

Haskell lacks tidyverse's `reprex` package, but **reprex principles apply directly**. The same example
in Haskell:

```haskell
-- Group required imports at top (= make dependencies explicit)
import qualified DataFrame as DF

main :: IO ()
main = do
  let y = [1 .. 4] :: [Double]
  print (sum y / fromIntegral (length y))   -- = R's mean(y)
  -- #> 2.5
```

In GHCi, typing an expression outputs the result below (same as R console), so copy-pasting becomes a
reprex. Running [`GettingHelp.hs`](GettingHelp.hs) with `cabal run` actually reproduces this `#> 2.5`.

Mapping R4DS's three reproducibility elements to Haskell:

| R's 3 elements | Haskell equivalent |
|---|---|
| **Packages**: Include required `library()` at top. Check version (`tidyverse_update()`) | Include required `import` at top. Declare dependencies in `.cabal` `build-depends`. Check version with `cabal outdated` / `ghcup` |
| **Data**: Have `dput(df)` output regeneration code and paste it | Embed data **as literals** in code with `fromNamedColumns` / `fromList` (see below) |
| **Code**: Use whitespace, short readable names, comments marking problem areas, remove unrelated code | Same (this repository's Coding Style in CLAUDE.md). GHC warnings (`-Wall`) catch waste |

### Data embedding (= R's `dput()`)

R's `dput(mtcars)` outputs "code that regenerates the data". Haskell's equivalent is embedding data
**as code literals**. The recipient doesn't need a separate CSV file—they can paste and run:

```haskell
let toy = DF.fromNamedColumns
      [ ("id",    DF.fromList ([1, 2, 3]          :: [Int]))
      , ("group", DF.fromList (["a", "b", "a"]    :: [Text]))
      , ("value", DF.fromList ([10.0, 20.0, 30.0] :: [Double])) ]
-- #> sum(value) = 60.0
```

Following R's advice, use the **minimal subset showing the problem** (not full real data, but the
smallest toy reproducing the bug).

### Finally, verify it runs in a fresh environment

R4DS concludes: "Start a fresh R session, paste, and confirm it actually reproduces." Haskell's
equivalent is **running `cabal run` in a different shell** (rebuilds with dependencies). Same output
confirms the example is truly self-contained and reproducible.

---

## §8.3 Investing in yourself

R: Learn incrementally before problems arise. Follow tidyverse team progress at
[tidyverse blog](https://www.tidyverse.org/blog/); follow the R community at
[R Weekly](https://rweekly.org).

Haskell equivalents (community and information sources):

| R information source | Haskell equivalent |
|---|---|
| tidyverse blog | [GHC blog](https://www.haskell.org/ghc/blog.html) / library CHANGELOGs / GitHub |
| R Weekly (weekly summary) | [Haskell Weekly](https://haskellweekly.news) (weekly newsletter) |
| Stack Overflow / community Q&A | [Haskell Discourse](https://discourse.haskell.org) / r/haskell / Libera `#haskell` |
| CRAN Task Views | [Hackage](https://hackage.haskell.org) categories / [Stackage](https://www.stackage.org) |

For this project specifically, hanalyze / hgg `src/` implementations and `test/` are primary sources
(in the spirit of CLAUDE.md's "distinguish fact from conjecture"—specification basis is confirmed by
grepping source).

---

## §8.4 Summary

R4DS's chapter closes the "Whole Game" part. Through visualization, transformation, tidying, and
import, we've cycled through the entire data science process once. Later parts go deeper into each.

This tutorial series follows the same flow: Ch.1–Ch.8 trace the full picture. Later chapters dive into
grammar of graphics, layers, and exploratory data analysis (EDA).

---

## R↔Haskell correspondence summary (mapping shown this chapter)

| R / RStudio / tidyverse | Haskell equivalent |
|---|---|
| Google "R" + package name | Google "Haskell" + library name |
| `?function` (function help) | GHCi `:doc` / `:info` / `:type`, Hoogle, Hackage haddock |
| No way to reverse-lookup function from type | **Hoogle** (type signature reverse-lookup) |
| `reprex::reprex()` | No dedicated tool. But reprex principles (imports at top, minimal, output included) are the same |
| `dput(df)` (data regeneration code) | Embed data as literals with `fromNamedColumns` / `fromList` |
| Verify in fresh R session | Rebuild with `cabal run` in another shell (with dependencies) |
| `tidyverse_update()` / check version | `cabal outdated` / `ghcup` |
| R Weekly / tidyverse blog | Haskell Weekly / GHC blog / Haskell Discourse |

## Unsupported / Differences recorded without approximation

- **No `reprex` package sugar**: R's `reprex()` formats code and output as `#>`-prefixed Markdown and
  auto-copies to clipboard. Haskell lacks this auto-formatting tool. But the chapter's goal—creating
  "reproducible and minimal" examples—is fully met by grouping imports at top, embedding data, and
  running `cabal run` in a fresh shell (just without auto-formatting).
- **R/RStudio-specific UI**: RStudio Viewer preview, clipboard integration, Server/Cloud selective
  copy are RStudio-specific operations. We note only the Haskell equivalents (GHCi / Hoogle /
  `cabal run`) in the table (R4DS has zero analysis figures in this chapter—it's prose advice).
