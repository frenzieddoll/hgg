# 06. Workflow: Scripts and Projects (R4DS 2e Ch.6 "Workflow: scripts and projects")

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.6 "Workflow: scripts and projects"**
> <https://r4ds.hadley.nz/workflow-scripts>
> Data: ggplot2 **diamonds** (full 53,940 rows).

R4DS Ch.6 is an **operations** chapter with **no plots** (all R examples in text are `eval: false`
demonstrations; plots are RStudio screenshots only). Two main topics:

1. **Scripts**: Instead of typing directly into console, write code to a file, edit, and run.
2. **Projects**: Organize all analysis files (input data, scripts, results, plots) in one directory.

Both are **RStudio-specific operations**, so rather than approximate or substitute, we show
a **faithful Haskell equivalent** (cabal / GHCi / cabal project as the R/RStudio counterpart).
Executable code is [`WorkflowScripts.hs`](WorkflowScripts.hs) (run the equivalent workflow).

```sh
cd docs/tutorials/06-workflow-scripts
cabal run tut-06-workflow-scripts
```

★ This file set itself exemplifies R4DS's idea of a "script" and the three naming principles
(imports grouped at top, sections marked with `-- ===` lines, numbered, kebab-case, descriptive names).

---

## §6.1 Scripts

R: Instead of typing into console, write code in script editor, edit, and rerun.
Saving lets you return to earlier versions. Haskell equivalents:

| R / RStudio | Haskell Equivalent |
|---|---|
| Write code in script editor | Write code in `.hs` file (this `WorkflowScripts.hs`) |
| Run one expression (Cmd/Ctrl + Enter) | GHCi `:load WorkflowScripts.hs` → evaluate expressions one by one |
| Run entire script (Cmd/Ctrl + Shift + S) | `cabal run tut-06-workflow-scripts` (run file through) |
| One-off file execution | `runghc WorkflowScripts.hs` |

R4DS's recommendation—"start scripts with required packages"—applies to Haskell too:
**group `import`s at the top** so anyone can see which dependencies are needed. However,
**don't write install commands** (R's `install.packages()` / Haskell's `cabal install`)
in shared scripts—they alter others' environments without consent. In Haskell, dependencies
are declared in `.cabal`'s `build-depends`, and `import` is the usage declaration.

### §6.1.1 Running Code

R4DS's example (typical pipe: filter missing, then aggregate) applied to diamonds:

```haskell
-- R: not_cancelled <- flights |> filter(!is.na(dep_delay), !is.na(arr_delay))
--    not_cancelled |> group_by(...) |> summarize(mean = mean(dep_delay))
smallIdeal =
  diamonds
    |> DF.filterWhere (F.col @Double "carat" .< (1.0 :: DF.Expr Double))
    |> DF.filterWhere (F.col @Text   "cut"   .== F.lit ("Ideal" :: Text))
```

The `filter |> … |> summarize` flow is identical in dataframe (one verb per line, `|>` at end).
Running yields 15,681 rows (carat < 1 and cut = Ideal) with mean price `1546.21`
(verified against `../_data/_raw/diamonds.csv` with `awk`).

### §6.1.2 Diagnostics

R: RStudio flags syntax errors (red squiggles) and potential issues (yellow !) in-editor.

Haskell's equivalent: **GHC type checking + compiler warnings**, in-editor via **HLS
(haskell-language-server)** (red squiggles, hover messages). Many errors R catches only at runtime,
Haskell catches earlier (compile time).

### §6.1.3 Saving and Naming

R4DS's three naming principles apply directly to Haskell files:

1. **Machine-readable**: Avoid spaces, symbols, special chars. Don't rely on case alone.
2. **Human-readable**: Name clearly conveys content.
3. **Sort-friendly**: Start with numbers; alphabetical order = execution order.

R4DS's "bad → good" examples:

```
# Avoid                          # Do this
alternative model.R               01-load-data.R
code for exploratory analysis.r   02-exploratory-analysis.R
finalreport.qmd / FinalReport.qmd 03-model-approach-1.R
run-first.r / temp.txt            04-model-approach-2.R
```

★ **This tutorial series' directory names** (`01-visualize` / `02-workflow-basics` /
… / `06-workflow-scripts`) exemplify all three principles (numbers, kebab-case, descriptive).

---

## §6.2 Projects

R: Collect all analysis files in one directory; RStudio treats it as a project (`.Rproj`).
Haskell's equivalent:

| R / RStudio | Haskell Equivalent |
|---|---|
| RStudio project (`.Rproj`) | cabal **package** (`.cabal`) / **project** (`cabal.project`) |
| project directory = analysis home | This `hgg/` repo (`../../cabal.project` bundles all packages) |
| `File > New Project` | `cabal init` / write `.cabal` |

### §6.2.1 Source of Truth

R: Source of truth is the **R script**, not the environment. From script + data,
environment is reproducible; the reverse is hard. RStudio recommends never saving
workspace between sessions (clean slate).

Haskell's equivalent: Source of truth is **`.hs` source**, not GHCi REPL state.
Restart GHCi (`:reload` / exit and `cabal repl`) and source always regenerates the same result.
**Don't depend on REPL bindings accumulated** —this matches R's clean-slate discipline.
R's "restart R → rerun script" workflow corresponds to Haskell's "`cabal repl` restart → `:reload`"
or "run via `cabal run`."

### §6.2.2 Working Directory

| R | Haskell |
|---|---|
| `getwd()` | `System.Directory.getCurrentDirectory` |
| `setwd("/path")` (**not recommended**) | `System.Directory.setCurrentDirectory` (equally **not recommended**) |

Both R and Haskell advise against hardcoding working directory via `setwd` / `setCurrentDirectory`
in code—it makes scripts location-dependent and hampers sharing. Instead, treat the project's
home as working directory and use relative paths thereafter.

### §6.2.3 Save via Relative Paths (RStudio Projects)

R4DS's toy example (`diamonds.R`, `eval: false`) emphasizes **saving plots and CSVs via code**
(not mouse/clipboard) **to relative paths**:

```r
# R (R4DS example)
ggplot(diamonds, aes(x = carat, y = price)) + geom_hex()
ggsave("diamonds.png")
write_csv(diamonds, "data/diamonds.csv")
```

Haskell's equivalent workflow:

```haskell
let dPlot = diamonds |>> theme ThemeGrey <> layer (scatter "carat" "price" <> alpha 0.05)
saveSVGBound "diamonds-carat-price.svg" dPlot     -- = ggsave("diamonds.png")
createDirectoryIfMissing False "data"             -- = Files pane's New Folder
DF.writeCsv "data/diamonds.csv" diamonds          -- = write_csv(diamonds, "data/diamonds.csv")
```

Running this creates `diamonds-carat-price.svg` and `data/diamonds.csv` in the chapter directory
(the working directory). Following R4DS's principle, **save plots via code**, not mouse clicks,
so you can always trace "which code made this plot?"

> ★ Honest note on differences:
> R's **`geom_hex`** (hexagonal binning to summarize dense scatterplots) is **not implemented** in hgg.
> The rectangular-bin equivalent `bin2d` exists, but it colors cells by **mean z value**,
> not by **count density** (and hexagonal bins aren't implemented). Since this chapter's point is
> the "save via code to relative paths" workflow, not hexagonal binning per se, we use the
> implemented `scatter` (+ `alpha` to reduce overplotting) instead. Hex/count binning and a full
> reproduction belong to later chapters (EDA / Layers) dealing with dense data.
>
> ※Generated files (`diamonds-carat-price.svg`, `data/`) are regenerated each run,
> so [`.gitignore`](.gitignore) excludes them from git (operations chapter with no permanent plots).

### §6.2.4 Relative and Absolute Paths

R: Within a project, **always use relative paths** (absolute paths prevent sharing).
Use `/` (Mac/Linux slash), never Windows backslash.

Haskell's equivalent: `System.FilePath` is `/`-separated and portable across OS.
Write `"data/diamonds.csv"` **relative with `/`**, and the project works anywhere.
Absolute paths (`/home/...` or `C:\...`) only work in your environment—**never write them in
shared code**.

---

## What We Can't Do / Faithfully Recorded Differences

- **△ `geom_hex` (hexagonal binning) not implemented**: §6.2.3's toy plot uses the implemented
  `scatter` (+ `alpha`) instead of unimplemented count-density `geom_hex` (`bin2d` is for cell-mean z,
  a different operation). The chapter's focus is save-to-relative-path workflow, not hex binning,
  so substitution is fine. Hex/count bin implementation and full reproduction belong to EDA / Layers.
- **R/RStudio UI details**: The **4-pane script editor, red diagnostics, `.Rproj` "New Project"
  wizard** are **R4DS-specific screenshots**. Here we show Haskell's equivalent operations
  (GHCi / HLS / cabal project) in tables instead (R4DS itself shows only these screenshots in the text—no analysis plots).
- **`writeCsv` with nullable columns**: This version of `dataframe`'s `writeCsv` can't serialize
  columns with missing values (see Ch.4 Import). Since diamonds has no missing values, §6.2.3 works as-is.
