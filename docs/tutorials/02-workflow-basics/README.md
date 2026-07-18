# 02. Workflow: Basics (R4DS 2e Ch.2 "Workflow: basics")

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.2 "Workflow: basics"**
> <https://r4ds.hadley.nz/workflow-basics>

R4DS Ch.2 is a **plot-free** chapter on "R coding fundamentals" (object creation, comments,
naming, function calls). This tutorial **faithfully maps R4DS code examples to equivalent Haskell**,
executing them to produce identical results. Where R and Haskell differ in approach, we make
this explicit (no omissions or substitutions). The executable code is in [`WorkflowBasics.hs`](WorkflowBasics.hs).

```sh
cd docs/tutorials/02-workflow-basics
cabal run tut-02-workflow-basics
```

## §2.1 Coding Basics

Basic arithmetic uses the same infix operators and precedence as R.

| R | hgg/Haskell | Result |
|---|---|---|
| `1 / 200 * 30` | `1 / 200 * 30` | `0.15` |
| `(59 + 73 + 2) / 3` | `(59 + 73 + 2) / 3` | `44.6667` |
| `sin(pi / 2)` | `sin (pi / 2)` | `1.0` |

Creating objects. R uses reassignable `<-`; Haskell uses **immutable binding** `let`:

| R | Haskell |
|---|---|
| `x <- 3 * 4` | `let x = 3 * 4` |

In both, just binding doesn't display the value; evaluating the name shows it (`x` → `12`).

Vectors. R uses `c(...)`; Haskell uses lists (or `Data.Vector`):

| R | Haskell |
|---|---|
| `primes <- c(2, 3, 5, 7, 11, 13)` | `let primes = [2,3,5,7,11,13]` |

> **★Difference (broadcasting)**: R automatically applies arithmetic to all vector elements,
> but Haskell requires explicit `map`.
> `primes * 2` → `map (* 2) primes` = `[4,6,10,14,22,26]`,
> `primes - 1` → `map (subtract 1) primes` = `[1,2,4,6,10,12]`.

## §2.2 Comments

Comments: R uses `#`, Haskell uses `--` (or `{- ... -}` for blocks). Purpose is the same:
write the **why** (not how or what). Comments in `WorkflowBasics.hs` are direct examples.

## §2.3 What's in a Name?

Naming conventions. R4DS recommends **snake_case**. Haskell convention is **camelCase**
(functions/bindings) / **PascalCase** (types/constructors), which this project follows (`CLAUDE.md`).

| R (snake_case) | Haskell (camelCase) |
|---|---|
| `this_is_a_really_long_name <- 2.5` | `let thisIsAReallyLongName = 2.5` |
| `r_rocks <- 2^3` | `let rRocks = 2 ^ 3` |

> **★Case and spelling are strict**. In R, `r_rock` / `R_rocks` fail at runtime with
> "object not found". In Haskell, `rRock` / `RRocks` fail at **compile time** with a scope error
> (caught by type checking before shipping—a key difference).

## §2.4 Calling Functions

Function calls. R supports named arguments; Haskell doesn't (uses positional arguments,
pseudo-names via records). Arithmetic sequences like R's `seq()` are most naturally expressed
with Haskell's range syntax.

| R | Haskell |
|---|---|
| `seq(from = 1, to = 10)` | `enumFromTo 1 10` |
| `seq(1, 10)` | `[1 .. 10]` |
| `x <- "hello world"` | `let greeting = "hello world"` |

Quote and parenthesis pairing is required in both languages.

## §2.5 Exercises (Key Points)

1. The `ı` in `my_varıable` is Turkish dotless-i, distinct from `my_variable`.
   In Haskell too, one character difference in an identifier means a different name (scope error).
   "Spelling and character type are strict."
2. Typo fixes: `libary(todyverse)` → `library(tidyverse)`,
   `aes(x = displ y = hwy)` → `aes(x = displ, y = hwy)`,
   `method = "lm` → `method = "lm"`.
   Haskell also enforces strict spelling of imports and argument names, pairing of brackets/quotes.
3–4. RStudio shortcuts and `ggsave` targets are IDE/R-specific topics. Equivalent IDE features
   or `saveSVG` are available, but since they don't affect plot reproduction, we omit them.

## Note

This chapter generates no plots; there are no SVG files. We execute all R4DS code examples
in Haskell equivalents and verify output (`cabal run tut-02-workflow-basics`).
