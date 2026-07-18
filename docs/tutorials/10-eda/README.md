# 10. Exploratory Data Analysis

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.11 "Exploratory data analysis"**
> <https://r4ds.hadley.nz/eda>
> Data: **diamonds** (53,940 rows), **mpg** (234 rows), **nycflights13::flights** (336,776 rows)

This chapter explores **variation** (within a variable) and **covariation** (between variables) through
visualization. Using histogram / freqpoly / boxplot / geom_count / geom_tile / geom_bin2d, we cover
outlier detection through pattern extraction via modeling. Execution code is in [`Eda.hs`](Eda.hs).
Integer-only columns (`price`, `hwy`, etc.) are inferred by dataframe as Int, so we provide a
`numCol` helper to extract as `Double`.

## Running

```sh
cd docs/tutorials/10-eda
cabal run tut-10-eda
```

---

## §1 Variation — Distribution of a single variable

View numerical variable distribution with **histogram**. Binning `diamonds$carat` by width 0.5 reveals
a **distribution with a long right tail**.

```haskell
diamonds |>> theme ThemeGrey <> layer (histogram "carat" <> binWidth 0.5)
```

![carat distribution](01-hist-carat-bw05.svg)

### Typical values — fine bins

Extract only small diamonds (`carat < 3`) and bin finely (width 0.01) to see **peaks at 1 carat and
nice fractions**.

```haskell
let smallerDF = ... filter (carat < 3) ...
smallerDF |>> theme ThemeGrey <> layer (histogram "carat" <> binWidth 0.01)
```

![carat<3 in fine bins](02-hist-carat-bw001.svg)

---

## §2 Unusual values — finding outliers with histogram

View `y` (width in mm) distribution: data clusters near 5, yet **the x-axis spans 0–60**, the only
clue to outliers.

```haskell
diamonds |>> theme ThemeGrey <> layer (histogram "y" <> binWidth 0.5)
```

![y distribution](03-hist-y-bw05.svg)

High-frequency bins tower too high, squashing rare bins. **Zoom y-axis to 0–50** with `coord_cartesian`
to see low bars at 0, ≈30, ≈60.

```haskell
diamonds |>> theme ThemeGrey <> layer (histogram "y" <> binWidth 0.5)
         <> coordCartesianY 0 50
```

![y-axis zoomed](04-hist-y-zoom.svg)

> `coordCartesianY lo hi` **changes display range only, not dropping data** (like ggplot's
> `coord_cartesian(ylim=)`, unlike `ylim()` which discards out-of-range values).

Extract outliers dplyr-style (`y < 3 | y > 20`): `y = 0` appears 7 times—diamonds with 0mm width
are impossible, so **0 encodes missing data**.

```
── Outliers (y<3 | y>20), y ascending ──
  price      x      y      z
5139.00   0.00   0.00   0.00
6381.00   0.00   0.00   0.00
12800.00   0.00   0.00   0.00
15686.00   0.00   0.00   0.00
18034.00   0.00   0.00   0.00
2130.00   0.00   0.00   0.00
2130.00   0.00   0.00   0.00
2075.00   5.15  31.80   5.12
12210.00   8.09  58.90   8.06
```

---

## §3 Missing values — how to handle them

Replace anomalous `y` (`y < 3 | y > 20`) with `NA`, then scatter `x` vs `y`. Rows with `NA` don't
render (here we exclude those rows to reach the same result).

```haskell
let xyKept = [ (x,y) | (x,y) <- zip xv yv, y >= 3 && y <= 20 ]
diamonds2DF |>> theme ThemeGrey <> layer (scatter "x" "y" <> alpha 0.4)
```

![x vs y (outliers as NA)](05-scatter-xy.svg)

Missing values themselves can be meaningful. In `flights`, `dep_time` missing means **cancelled**.
Create groups with `cancelled = is.na(dep_time)` and overlay **frequency polygons** of scheduled
departure times by cancellation status.

```haskell
let cancelled = map isNothing depTime
    schedDec  = [ h + m/60 | ... ]   -- convert sched_dep_time to time (hours)
flightsDF |>> theme ThemeGrey <> layer (freqpoly "sched_dep_time" <> binWidth 0.25 <> colorBy "cancelled")
```

![scheduled departure time by cancellation](06-freqpoly-flights.svg)

> Cancelled flights are far fewer than non-cancelled, so raw counts are hard to compare—leading to
> density normalization in the next section.

---

## §4 Covariation — categorical × numerical

Compare `price` distribution by `cut` (quality) using **frequency polygon**. Raw counts first: cut
itself varies greatly in count, making shape comparison difficult.

```haskell
diamonds |>> theme ThemeGrey <> layer (freqpoly "price" <> binWidth 500 <> colorBy "cut"
                   <> colorCats cutOrder)
```

![price by cut (count)](07-freqpoly-price-count.svg)

Normalize area to 1 with `after_stat(density)` (`histogramDensity True`) to compare shapes. **Fair
alone is flat with higher mean**; others peak sharply near price≈1500—matching R4DS's description.

```haskell
diamonds |>> theme ThemeGrey <> layer (freqpoly "price" <> binWidth 500 <> colorBy "cut"
                   <> colorCats cutOrder <> histogramDensity True)
```

![price by cut (density)](08-freqpoly-price-density.svg)

**Box plots** (`boxplot "y" <> groupBy "g"`) summarize distributions at a glance. Medians are
surprisingly **Ideal lowest, Fair highest** (confounded with carat).

```haskell
diamonds |>> theme ThemeGrey <> layer (boxplot "price" <> groupBy "cut") <> scaleXDiscreteLimits cutOrder
```

![price by cut box plot](09-box-price-cut.svg)

Box plot of `mpg` by `class` for `hwy` (highway fuel economy). `class` is alphabetical order.

```haskell
mpg |>> theme ThemeGrey <> layer (boxplot "hwy" <> groupBy "class")
```

![hwy by class](10-box-hwy-class.svg)

Unordered categories become more readable when **reordered by median** (`fct_reorder`). Pass class
sorted by `hwy` median ascending to `scaleXDiscreteLimits`.

```haskell
let classByMedian = sortOn classMedian (nub mpgClass)
mpg |>> theme ThemeGrey <> layer (boxplot "hwy" <> groupBy "class") <> scaleXDiscreteLimits classByMedian
```

![hwy by class (median order)](11-box-hwy-class-reorder.svg)

Long category names fit better when **flipped horizontal** (`coordFlip`).

```haskell
mpg |>> theme ThemeGrey <> layer (boxplot "hwy" <> groupBy "class") <> scaleXDiscreteLimits classByMedian
    <> coordFlip <> xLabel "hwy" <> yLabel "class"
```

> `coordFlip` reverses data axes, but axis titles stay fixed to physical axes (bottom=x, left=y),
> so we swap `xLabel`/`yLabel` to match the flipped display.

![hwy by class (flipped)](12-box-hwy-class-flip.svg)

---

## §5 Covariation — categorical × categorical

Co-occurrence of two categories is shown with **`geom_count`** (`countXY`): **point area** (area ∝
count) represents each combination's count. Maximum is **Ideal × color G**.

```haskell
diamonds |>> theme ThemeGrey <> layer (countXY "cut" "color") <> scaleXDiscreteLimits cutOrder
```

![cut × color counts](13-count-cut-color.svg)

Same aggregation in a table via `count(color, cut)`, then render with **`geom_tile`** (`heatmap`),
filling with `fill = n`: again, **color G × Ideal is maximum** (brightest).

```haskell
let tileDF = ... color, cut, n = comboCount ...
tileDF |>> theme ThemeGrey <> layer (heatmap "color" "cut" "n") <> scaleYDiscreteLimits cutOrder
```

![color × cut counts (tile)](14-tile-color-cut.svg)

---

## §6 Covariation — numerical × numerical

Scatter plot of `carat` vs `price`: **positive, strong, exponential relationship** (`carat < 3`).

```haskell
smallerDF |>> theme ThemeGrey <> layer (scatter "carat" "price")
```

![carat vs price](15-scatter-carat-price.svg)

When too many points overlap, lower **transparency** (`alpha`). At `alpha = 1/100`, **clusters**
around 1, 1.5, 2 carats emerge.

```haskell
smallerDF |>> theme ThemeGrey <> layer (scatter "carat" "price" <> alpha 0.01)
```

![carat vs price (alpha=1/100)](16-scatter-carat-price-alpha.svg)

Extending 1D binning to 2D: **`geom_bin2d`** (`bin2dCount`). Divide the plane into rectangular bins,
color each cell by **count** (concentrated at low carat, low price).

```haskell
smallerDF |>> theme ThemeGrey <> layer (bin2dCount "carat" "price")
```

![carat vs price (bin2d, fill=count)](17-bin2d-carat-price.svg)

> **Honest constraint**: R4DS also shows `geom_hex` (hexagonal bins), but hgg **lacks hexagonal
> binning**. The figure below **substitutes rectangular bin2d** (same distribution summary, just not
> hexagonal).

![carat vs price (geom_hex replacement = rectangular bin2d)](18-hex-carat-price.svg)

Continuous variables can be **cut into intervals** and treated as categories. Use `cut_width(carat,
0.1)` equivalent to round `carat` to 0.1 steps, then box plot `price` per bin. As carat increases,
median rises and tail skew changes.

```haskell
let caratBin c = 0.1 * fromIntegral (round (c / 0.1) :: Int)
cwDF |>> theme ThemeGrey <> layer (boxplot "price" <> groupBy "carat_bin") <> scaleXDiscreteLimits cwLabels
```

![price by cut_width(carat, 0.1)](19-box-cutwidth.svg)

> **Honest constraint**: hgg's box plots **don't render outliers beyond whiskers as individual
> points** (whiskers cap). The many upper outlier points visible in R4DS figures are omitted.

---

## §7 Patterns and models — seeing past strong relationships

The relationship between `cut` and `price` is obscured by mutual entanglement of `cut`, `carat`, and
`price`. **Remove carat's effect via modeling**. Fit `log(price) ~ log(carat)` by least squares,
then exponentiate residuals back to price scale (price with carat effect removed).

```haskell
let b = sxy / sxx; a = my - b*mx                 -- log-log OLS
    resid = [ exp (y - (a + b*x)) | (x,y) <- zip lcarat lprice ]
residDF |>> theme ThemeGrey <> layer (scatter "carat" "resid" <> alpha 0.2)
```

Scattering residuals against `carat` reveals a **clear curved pattern**: residuals decrease with
increasing carat.

![residuals vs carat](20-resid-carat.svg)

After removing carat's effect, box plot residuals by `cut`: as expected, **higher quality costs
relatively more**—median rises monotonically from Fair → Ideal.

```haskell
residDF |>> theme ThemeGrey <> layer (boxplot "resid" <> groupBy "cut") <> scaleXDiscreteLimits cutOrder
```

![residuals by cut box plot](21-resid-cut.svg)

---

## Summary

| R4DS geom | hgg | Note |
|---|---|---|
| `geom_histogram` | `histogram <> binWidth` | |
| `coord_cartesian(ylim=)` | `coordCartesianY` | Zoom without dropping data |
| `geom_freqpoly` | `freqpoly` (`histogramDensity` for density) | New this chapter |
| `geom_boxplot` | `boxplot` + `groupBy` | Outliers hidden (whisker capped) |
| `fct_reorder` | `scaleXDiscreteLimits` (median order) | Compute order in preprocessing |
| `coord_flip` | `coordFlip` | Axis titles fixed to physical axes |
| `geom_count` | `countXY` | New this chapter (area ∝ count) |
| `geom_tile(fill=n)` | `heatmap` | Pre-aggregate counts |
| `geom_bin2d` | `bin2dCount` | Count mode added this chapter |
| `geom_hex` | (`bin2dCount` substitute) | Hexagonal binning unimplemented |

We've cycled through EDA's twin pillars—variation and covariation—across 1 variable, categorical ×
numerical, categorical × categorical, numerical × numerical, and model residuals.
