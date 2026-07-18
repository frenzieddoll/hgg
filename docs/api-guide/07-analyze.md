# Analyze integration — Plot fitted models directly

> 🌐 **English** | [日本語](07-analyze.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 layers](02-layers.md) | [03 encoding & scale](03-encoding-scale.md) | [04 decoration](04-decoration.md) | [05 backends](05-backends.md) | [06 dataframe](06-dataframe.md) | **07 analyze** | [08 3d](08-3d.md) | [09 appendix](09-appendix.md)

Models fitted with the statistical library **hanalyze** can be layered directly onto hgg plots. **All major model types produce visualizations** — regression, GLM, GP, survival, time series, Bayesian, etc. Uncertainty (CI / credible bands / prediction intervals) uses statistically correct values computed by hanalyze.

Page structure:
**[Fit first](#fit-data)** | **[Route 1: model-out (`toPlot`)](#route1)** |
**[Plot HBM (Bayesian)](#hbm-plotting)** | **[Route 2: stat-in (`statLm`/`statSmooth`)](#route2)** |
**[Example: Overlaying multiple models](#multi-overlay)**

> Setup: On the analyze side, enable `flag plot-integration` to activate `Hanalyze.Plot`
> (`cabal build --project-file=cabal.project.plot`). The stat-in path uses `hgg-analyze-bridge`.

### Fit first — `df |-> spec` (recommended) / raw vectors {#fit-data}

Before plotting, **fit a model**. Data can be supplied two ways:

- **`df |-> spec` (recommended)** — Fit directly by column name from df. Write `df |-> lm "x" "y"`.
  `(|->) :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec`. The left side `df` can be `Map Text ColData`,
  an association list, or `DataFrame` (`class ColumnSource`).
- **Raw vectors** — Pass `LA.Vector` directly, e.g. `lmModel (LA.fromList xs) (LA.fromList ys)`.

**Both return identical model values.** The result type of `df |-> lm "x" "y"` is `LMModel`, just like `lmModel`
(`type Fitted LMSpec = LMModel`). The only difference is **the way data is supplied**. If you have a df, use `df |-> lm "x" "y"` instead of constructing raw vectors (it includes column name validation).

Spec constructor types (`df |-> spec :: Fitted spec`. `(|->) :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec`):

| spec function | Type (what to pass) | `Fitted` (= type passable to `toPlot`) |
|---|---|---|
| `lm` | `Text -> Text -> LMSpec` | `LMModel` |
| `glm` | `Family -> LinkFn -> Text -> Text -> GLMSpec` | `GLMModel` |
| `spline` | `SplineKind -> [Double] -> Text -> Text -> SplineSpec` | `SplineModel` |
| `robust` | `RobustEstimator -> Text -> Text -> RobustSpec` | `RobustModel` |
| `quantile` | `[Double] -> Text -> Text -> QuantileSpec` | `QuantileModel` |
| `lmF` | `Text -> LMFormulaSpec` | `MultiLMModel` |
| `glmF` | `Family -> LinkFn -> Text -> GLMFormulaSpec` | `MultiGLMModel` |
| `glmmF` | `Text -> GLMMFormulaSpec` | `(GLMMResultRE, [Text])` |
| `grouped` | `Text -> spec -> GroupedSpec spec` | `GroupedFit spec` |
| `weighted` | `[Double] -> LMSpec -> WeightedLMSpec` | `WeightedLMModel` |
| `hbm` | `HBMConfig -> ModelP () -> HBMSpec` | `HBMModel` |

> **Formula**: `lmF` / `glmF` / `glmmF` use R-style strings — main effects `y ~ x1 + x2`, interactions `x1:x2`
> (`*` includes main effects), mixed effects `(1|g)` / `(x|g)` — to fit **multivariate models**
> (`df |-> lmF "y ~ x1 + x2"`). Two-column `lm`/`glm` are shortcuts for univariate cases.

All `Fitted` types work with the `toPlot` in **Route 1** below. Examples below primarily use `df |-> spec`.

Integration follows two routes.

### Route 1: model-out — `toPlot fit` (fitted model → plot) {#route1}

Convert fitted models using `toPlot :: m -> VisualSpec` into layers and combine with `<>`. To overlay on the same df as a scatter plot: `df |>> (layer (scatter "x" "y") <> toPlot fit)`. For models without scatter (MCMC / survival / PCA, etc. where the plot is self-contained), use an **empty df**. Below is a complete minimal example (fit without constructing raw vectors):

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector              as V
import qualified Numeric.LinearAlgebra    as LA   -- Used later for survival/PCA etc (lm/glm use df |-> )
import           Data.Text                (Text)
import           Hgg.Plot.Spec        (ColData (..), layer, scatter)
import           Hgg.Plot.Frame       ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hanalyze.Plot            (toPlot, (|->), lm)

-- Empty df for self-contained models (not overlaid on scatter)
noDf :: [(Text, ColData)]
noDf = []

main :: IO ()
main = do
  let df = [ ("x", NumData (V.fromList [1,2,3,4,5,6,7,8]))
           , ("y", NumData (V.fromList [2.1,3.9,6.2,7.8,10.3,11.7,14.1,16.0])) ]
           :: [(Text, ColData)]
  saveSVGBound "lm.svg" (df |>> (layer (scatter "x" "y") <> toPlot (df |-> lm "x" "y")))
```

> Examples below show **key points only** (sharing the imports, `noDf`, and `saveSVGBound` from above).

Models with `Plottable` instance (= plottable via `toPlot`) number **14 types**:

#### Regression & smoothing (scatter + fit + band)

Constructors for fitting directly from raw vectors (`LA.Vector Double`, = path without `df |->` ):

| Model | Constructor type (raw vector path) | Band meaning |
|---|---|---|
| Linear regression LM | `lmModel :: Vector -> Vector -> LMModel` | Wald confidence band |
| GLM | `glmModel :: Family -> LinkFn -> Vector -> Vector -> GLMModel` | **Asymmetric** credible band on μ scale |
| Gaussian process GP | `fitGP :: GPModel -> [Double] -> [Double] -> [Double] -> GPResult` | Credible band |
| Spline | `splineModel :: SplineKind -> [Double] -> Vector -> Vector -> SplineModel` | Wald band in basis space |
| GAM | `gamModel :: Int -> Int -> Double -> Vector -> Vector -> GAMModel` (order, knots, λ, x, y) | Smoothed curve (no band) |
| Robust regression | `robustModel :: RobustEstimator -> Vector -> Vector -> RobustModel` | Line only (weights → diagnostic plot) |
| Quantile regression | `quantileModel :: [Double] -> Vector -> Vector -> QuantileModel` | Multiple quantile lines |

> `Vector` = `Numeric.LinearAlgebra.Vector Double`. `toPlot :: Plottable m => m -> VisualSpec`
> converts these to plots. Using `df |-> lm "x" "y"` (table above) is the natural shortcut returning the same result.

> Note: LM / Spline bands are **regression mean confidence intervals (CI)**, not **prediction intervals for individual observations**.
> GLM / GP use credible bands. Different band types overlaid in one plot appear asymmetric
> (compare by omitting bands and color-coding lines for readability, as in [example (1)](#fit-data)).

```haskell
-- df has "x" "y" columns (assoc / Map / DataFrame). Spec-specific imports:
--   GLM    : import Hanalyze.Model.GLM    (Family (..), LinkFn (..))
--   Spline : import Hanalyze.Model.Spline (SplineKind (..))
--   Robust : import Hanalyze.Model.Robust (RobustEstimator (..), defaultHuberK)
df |>> (layer (scatter "x" "y") <> toPlot (df |-> lm "x" "y"))                              -- LM
df |>> (layer (scatter "x" "y") <> toPlot (df |-> glm Poisson Log "x" "y"))                 -- GLM
df |>> (layer (scatter "x" "y") <> toPlot (df |-> spline (BSpline 3) [0,2,4,6,8] "x" "y"))  -- Spline (knots=internal knots)
df |>> (layer (scatter "x" "y") <> toPlot (df |-> lm "x" "y")                               -- OLS vs robust
                                <> toPlot (df |-> robust (Huber defaultHuberK) "x" "y"))
```

| LM (confidence band) | GLM Poisson (asymmetric band) | GP (credible band) |
|---|---|---|
| ![](images/analyze-integration/lm-scatter-ci.svg) | ![](images/analyze-integration/glm-poisson-ci.svg) | ![](images/analyze-integration/gp-mean-ci.svg) |

| Spline (smooth + band) | GAM (smooth) | Robust vs OLS | Quantile (multiple quantiles) |
|---|---|---|---|
| ![](images/analyze-integration/spline-smooth-ci.svg) | ![](images/analyze-integration/gam-smooth.svg) | ![](images/analyze-integration/robust-vs-ols.svg) | ![](images/analyze-integration/quantile-lines.svg) |

#### Regression options: grid evaluation / `predAt` / effect plots (regression & smoothing only)

> ⚠️ `grid` / `gridRange` / `predAt` / `statLevel` / `holdAt` / `byVar` in this section apply **only to regression & smoothing types**
> (`SingleVarModel` / `MultiVarModel` = LM/GLM/Spline/GAM/Robust/Quantile). They don't affect MCMC chains,
> KM survival, PCA, RandomForest, etc. (these have no "line" to evaluate on a grid).

`toPlot m` evaluates at **training points** and connects the regression line. For sparse or uneven data, curves and bands look jagged. Wrapping with `statModel` to **evaluate on regular grid** smooths them (scatter points still show training data).
Additionally, **effect plots** (`statModelMulti`) and prediction points (`predAt`) compose with `<>`.
These are analyze-side (`Hanalyze.Plot`) features; overriding `grid` doesn't change the **fit itself** (only evaluation points).
`statModel` returns `ModelSpec`, composable with settings below using `<>` (same idiom as [04 enum reference](04-decoration.md#enum-tables)).
**Common options for univariate models** (example: `statModel (df |-> lm "x" "y")`):

All return `ModelSpec` and compose with `<>` (`statModel :: SingleVarModel m => m -> ModelSpec`):

| Setting | Type (what to pass) | Effect (default) |
|---|---|---|
| `grid` | `Int -> ModelSpec` | Evaluation points = smoothness (default 100) |
| `gridRange` | `Double -> Double -> ModelSpec` | Evaluation range lo hi (default = explanatory variable min/max) |
| `bandOn` | `ModelSpec` | Show confidence/credible band (no args, default off) |
| `interval` | `IntervalKind -> ModelSpec` | Band type (`CI` mean confidence / `PI` prediction, needs `bandOn`, LM/GLM only for PI) |
| `statLevel` | `Double -> ModelSpec` | Band coverage (default 0.95, HBM `epred` is 0.94) |
| `predAt` | `Double -> ModelSpec` | Prediction point + CI error bar (stack with `<>` = multiple points) |
| `statColor` / `statFill` | `Color -> ModelSpec` | Regression line fixed color / band fill color (type-safe `Color`, `statColor (fromHex "#…")` / `statColor N.red`) |
| `statLabel` | `Text -> ModelSpec` | Add legend label to line (stacked legends show all labels) |
| `statEquation` / `statR2` | `ModelSpec` | Show regression equation / R² in legend (no args, LM only) |

> **Band defaults**: Off by default (opt-in). Add `<> bandOn` only when needed (ggplot `geom_smooth(se=TRUE)` style opt-in).
> Line type `LtDashed` etc. use `LineType` (`LtSolid` / `LtDashed` / `LtDotted` / `LtDotDash` / `LtLongDash` / `LtTwoDash`).
>
> `along` / `holdAt` / `byVar` are **multivariate `statModelMulti` only** — handled separately in multivariate effect plots below.

```haskell
import           Hanalyze.Plot ( toPlot, statModel, grid, gridRange, bandOn, statLevel
                               , interval, IntervalKind (..), statColor, statLabel, predAt
                               , statModelMulti, along, holdAt, byVar, HoldAgg (..)
                               , lmF, (|->) )
import           Hgg.Plot.Color   (fromHex)   -- statColor takes Color

-- (1) Grid evaluation: Eliminate jag from sparse data. Default grid=100 points, range=explanatory min/max. Default no band.
df |>> (layer (scatter "x" "y") <> toPlot (statModel smod <> grid 200))    -- Smooth at 200 points (no band)
df |>> (layer (scatter "x" "y") <> toPlot (statModel smod <> grid 200 <> gridRange 0 10))
df |>> (layer (scatter "x" "y") <> toPlot (statModel smod <> grid 200 <> bandOn))    -- Band on (opt-in)
df |>> (layer (scatter "x" "y") <> toPlot (statModel smod <> bandOn <> interval PI)) -- Prediction interval (LM/GLM)
df |>> (layer (scatter "x" "y") <> toPlot (statModel smod <> bandOn <> statLevel 0.99)) -- 99% band
df |>> (layer (scatter "x" "y") <> toPlot (statModel smod <> statColor (fromHex "#e41a1c") <> statLabel "OLS")) -- Color + legend

-- (2) predAt: Prediction point + CI error bar. Stack with <> = multiple points.
df |>> ( layer (scatter "x" "y")
       <> toPlot (statModel smod <> grid 200 <> predAt 1 <> predAt 4 <> predAt 7) )
```

| Training point evaluation (jagged) | grid 200 (smooth) | predAt (prediction points + CI) |
|---|---|---|
| ![](images/analyze-integration/grid-before-training-points.svg) | ![](images/analyze-integration/grid-after-200.svg) | ![](images/analyze-integration/predat-points.svg) |

**Multivariate effect plots** = For models with multiple explanatory variables, vary one (`along`) while fixing others to visualize effects
(R's `ggpredict` / `effects` equivalent). Model is **fit with formula** (`df |-> lmF "y ~ x1 + x2"` / `glmF`).
Unlike univariate `statModel`, `statModelMulti m (along "x1")` makes `along` a **required argument**, preventing compile-time mistakes from forgetting to specify the varying variable.

`statModelMulti :: MultiVarModel m => m -> AlongSpec -> ModelSpec` (second argument `along` required):

| Argument | Type (what to pass) | Role (default) |
|---|---|---|
| **`along`** (required) | `Text -> AlongSpec` | Varying explanatory variable = x-axis (second arg to `statModelMulti m (along "v")`) |
| `holdAt` | `HoldAgg -> ModelSpec` | How to fix other variables (`HoldAgg` 6 types, table below, default `Mean`) |
| `byVar` | `Text -> [Double] -> ModelSpec` | Fix second variable at specific values, plot stratified curves color-coded |

> `along` is the only `statModelMulti` argument (type-required). `holdAt` / `byVar` compose with `<>`.
> Univariate options `grid` / `gridRange` / `statLevel` / `bandOn` / `predAt` also work.

```haskell
-- Fit y ~ x1 + x2 with formula. df has "y" "x1" "x2" columns (assoc / Map / DataFrame).
let effMod = df |-> lmF "y ~ x1 + x2"

-- Vary x1, fix other variable (x2) with holdAt (default Mean).
df |>> (layer (scatter "x1" "y") <> toPlot (statModelMulti effMod (along "x1") <> holdAt Median))

-- byVar = Fix second variable at multiple values → plot one curve per value, color-coded.
df |>> ( layer (scatter "x1" "y")
       <> toPlot (statModelMulti effMod (along "x1") <> grid 100 <> byVar "x2" [1, 5]) )
```

`holdAt` fixing methods (`HoldAgg`) are **6 types**:

| HoldAgg | How to fix other variables |
|---|---|
| `Mean` (default) / `Median` | Mean / median of continuous; factor → most frequent level |
| `Mode` | Mode (continuous=rounded mode, factor=most frequent level) |
| `Reference` | Factor reference level (first in ascending order; continuous → `Mean`) |
| `Marginalize` | Don't fix; marginalize over observed distribution (PDP/AME, all rows × grid, heavy, no band = curve only) |
| `Fixed [(name, v)]` | Explicit specification (partial OK; unspecified → `Mean`) |

| effect: byVar (x2=1,5 color-coded) | effect: holdAt Median (x2 fixed at median) |
|---|---|
| ![](images/analyze-integration/effect-byvar.svg) | ![](images/analyze-integration/effect-holdat-median.svg) |

Key points:
- **`statModel` (univariate) / `statModelMulti` (multivariate) separated by type classes `SingleVarModel` / `MultiVarModel`**.
  `along` required only for multivariate; unnecessary for univariate (misuse caught at compile time).
- **GLM also supports multivariate** (`df |-> glmF family link "y ~ x1 + x2"`). Band is asymmetric on μ scale.
- grid / predAt / holdAt / byVar all compose with `ModelSpec` via `<>` (Monoid). `toPlot (statModel m)` uses traditional training point evaluation (adding `grid` switches to grid evaluation = backwards compatible).

#### Grouped fitting: `grouped` (one line per group)

To **fit separately per group and overlay N color-coded lines**, use **`df |-> grouped "g" spec`**
(ggplot `geom_smooth(aes(color=g))` equivalent). Result is **`GroupedFit spec`** holding each group's `Fitted spec`,
and `toPlot` draws N curves + group legend (`ColorByCol` + `scaleColorManual`) in one shot.

```haskell
import Hanalyze.Plot ( grouped, groupModels, groupLabels, groupedFullrange
                     , lmDiag, groupedLmDiag, CoefStats (..), (|->), toPlot )

-- Split by group "g", fit each with lm "x" "y". Spec can be lm/glm/spline/robust/quantile.
let gf = df |-> grouped "g" (lm "x" "y")     -- :: GroupedFit LMSpec

df |>> (layer (scatter "x" "y" <> colorBy "g") <> toPlot gf)   -- Group scatter + group regression lines

-- Extend each group line to full x range of all groups (ggplot fullrange=TRUE). Group-specific extender.
df |>> (layer (scatter "x" "y" <> colorBy "g") <> groupedFullrange gf)
```

![grouped: Fit lm per group A/B and color-code](images/analyze-integration/grouped-lm.svg)

`GroupedFit` is not just a drawing spec but a **result type**, enabling diagnosis of whether slopes truly differ across groups:

| Accessor | Return type | Purpose |
|---|---|---|
| `groupModels gf` | `[(Text, Fitted spec)]` | Extract each group's fitted model (e.g. `LMModel`) |
| `groupLabels gf` | `[Text]` | Group label list |
| `lmDiag m` | `[CoefStats]` | Single `LMModel` coefficient diagnostics (`csSE` / `csTValue` / `csPValue`) |
| `groupedLmDiag gf` | `[(Text, [CoefStats])]` | All groups' coefficient diagnostics at once (`Fitted spec ~ LMModel` only) |

> SE / t / p from `lmDiag` match statsmodels OLS to 1e-6 precision. Unlike HBM's `forestOf` for visual group-coefficient comparison, `groupedLmDiag` extracts numeric group coefficients.

#### Weighted regression: `weighted` (WLS)

**Weighted least squares (WLS)** minimizes `Σ wᵢ(yᵢ − ŷᵢ)²` with observation-specific weights `wᵢ`.
Fit with **`df |-> weighted ws (lm "x" "y")`** (ggplot `geom_smooth(method=lm, aes(weight=w))` equivalent).
Use for unequal variance or differing observation reliability. Result is `WeightedLMModel`; `toPlot` draws
regression line + **WLS confidence band** (reflecting weights). Overlay element data scatter normally.

```haskell
import Hanalyze.Plot (weighted, lm, (|->), toPlot, statModel, bandOn, statEquation)

let wm = df |-> weighted ws (lm "x" "y")        -- ws :: [Double] (row order, all ≥ 0)

df |>> (layer (scatter "x" "y") <> toPlot wm)                       -- WLS line (overlay on raw data scatter)
df |>> (layer (scatter "x" "y") <> toPlot (statModel wm <> bandOn)) -- WLS with confidence band
```

![weighted: WLS regression line + confidence band](images/analyze-integration/weighted-wls.svg)

Key points:
- **CI correct for WLS**: Grid evaluation computes non-scaled evaluation points × √w-scaled design matrix,
  `se = t·√(s²·x₀ᵀ(XᵀWX)⁻¹x₀)`. Matches statsmodels `WLS().fit()` for β̂ / `mean_ci` / `rsquared` to 1e-6 precision.
  `toPlot` is fixed to grid path and aligns with raw data scatter.
- **`statEquation` / `statR2`** work (R² is **weighted R²** like statsmodels = centered on weighted mean).
- All weights = 1 matches OLS (`lm`). Currently **LM-only** (`weighted :: [Double] -> LMSpec -> …`, WLS is LM-specific).

#### Mixed effects: caterpillar plot of random effects

Mixed-effects models (fit with `glmmF "y ~ x + (1|group)"`, `GLMMResultRE` with random intercept + slope)
are drawn as **caterpillar plots**: **random effects (BLUPs) sorted by value**, forest marks (horizontal bars) lined up,
reference line at 0 (= deviation from fixed effect is zero). Group spread and outliers visible at a glance — a canonical GLMM plot.

```haskell
import Hanalyze.Plot (glmmF, (|->), toPlot, diagnosticPlots)

let (re, _) = df |-> glmmF "y ~ x + (1|group)"   -- (GLMMResultRE, fixed effect coefficient names)

noDf |>> toPlot re                                -- Caterpillar for first column (usually random intercept)
noDf |>> subplots (diagnosticPlots re)            -- All r columns (intercept + slopes) in separate plots
```

Key points:
- `toPlot` = caterpillar for random-effect **first column only** (usually intercept). `diagnosticPlots` = all r columns (intercept + slopes).
- **No CI band currently (points only)**: Only point estimates (BLUPs) drawn. Group-level uncertainty bands not output in current version (forest marks support symmetric CI display).

| Mixed-effects caterpillar (8 groups, random intercept, BLUPs sorted) |
|---|
| ![](images/analyze-integration/glmm-caterpillar.svg) |

#### Bayesian diagnostics (MCMC)

```haskell
import qualified Data.Map.Strict    as Map
import           Hanalyze.MCMC.Core  (Chain (..))
import           Hanalyze.Plot       (chainModel, toPlot, diagnosticPlots)

-- Normally use sampler output (Chain) directly. Below constructs draw column manually:
let draws = [ 5 + sin (fromIntegral i * 0.31) | i <- [1 .. 200 :: Int] ] :: [Double]
    chain = Chain { chainSamples     = [ Map.singleton "mu" v | v <- draws ]
                  , chainAccepted    = 200, chainTotal = 240
                  , chainEnergy      = [], chainDivergences = [] }
    cmod  = chainModel "mu" chain                                   -- param name + Chain
saveSVGBound "trace.svg"   (noDf |>> toPlot cmod)                   -- trace plot
saveSVGBound "density.svg" (noDf |>> (diagnosticPlots cmod !! 1))   -- marginal posterior (2nd of diagnostics)
```

| MCMC trace | MCMC density |
|---|---|
| ![](images/analyze-integration/mcmc-trace.svg) | ![](images/analyze-integration/mcmc-density.svg) |

#### Survival & time series

```haskell
import Hanalyze.Model.Survival       (Event (..), SurvSample (..), kaplanMeier)
import Hanalyze.Model.CompetingRisks (CRSample (..), fitCompetingRisks)
import Hanalyze.Plot                 (forecastModel, toPlot)

-- KM survival: (time, Observed | Censored) columns
let kmSamples = [ SurvSample t e
                | (t, e) <- [ (2,Observed),(3,Observed),(5,Censored),(6,Observed)
                            , (8,Observed),(9,Censored),(11,Observed),(12,Observed) ] ]
saveSVGBound "km.svg" (noDf |>> toPlot (kaplanMeier kmSamples))

-- Competing risks CIF: (time, cause). Cause 0 = censoring, 1/2 = event types
let crSamples = [ CRSample t c
                | (t, c) <- [ (1,1),(2,2),(3,1),(4,0),(5,2),(6,1),(7,2),(8,0) ] ]
saveSVGBound "cif.svg" (noDf |>> toPlot (fitCompetingRisks crSamples))

-- Time series forecast: forecastModel order horizon series (AR(order) forward horizon steps)
let series = LA.fromList (drop 1 (scanl (\y e -> 10 + 0.6*(y-10) + e) 10
               [ 1.5 * sin (fromIntegral i * 1.3) | i <- [1 .. 60 :: Int] ]))
saveSVGBound "forecast.svg" (noDf |>> toPlot (forecastModel 2 12 series))
```

| KM survival | Competing risks CIF | Time series forecast |
|---|---|---|
| ![](images/analyze-integration/km-survival.svg) | ![](images/analyze-integration/cif-competing.svg) | ![](images/analyze-integration/ts-forecast.svg) |

#### Multivariate & trees

```haskell
import           Hanalyze.Model.MultiLM      (fitMultiLM)
import           Hanalyze.Model.PCA          (PCAStandardize (..), pca)
import           Hanalyze.Model.RandomForest (defaultRFConfig, fitRF)
import qualified System.Random.MWC           as MWC

-- Multi-output linear regression → residual correlation heatmap (xmat: n×p design, ymat: n×q outputs)
let n    = 30
    xcol = [ fromIntegral i | i <- [1 .. n] ] :: [Double]
    wig  = [ sin (0.7 * fromIntegral i) | i <- [1 .. n] ]
    xmat = LA.fromColumns [LA.konst 1 n, LA.fromList xcol]                  -- intercept + x
    ymat = LA.fromColumns [ LA.fromList (zipWith (\x w -> 2*x + w)   xcol wig)
                          , LA.fromList (zipWith (\x w -> x + 0.8*w) xcol wig)
                          , LA.fromList (zipWith (\x w -> -x - w)    xcol wig) ]
    mfit = fitMultiLM xmat ymat
saveSVGBound "multilm.svg" (noDf |>> toPlot mfit)

-- PCA → scree plot (rows: observations × variables. Center = covariance PCA)
let rows   = [ [ 5*sin (fromIntegral i*0.3), 1.2*cos (fromIntegral i*0.5)
               , 0.4*sin (fromIntegral i) ] | i <- [1 .. 50 :: Int] ]
    pcaRes = pca Center Nothing (LA.fromLists rows)
saveSVGBound "pca.svg" (noDf |>> toPlot pcaRes)

-- RandomForest → feature importance bar (fit is IO, needs random gen)
let xss = [ [ fromIntegral i, sin (fromIntegral i*3.1)
            , fromIntegral ((i*7) `mod` 11) ] | i <- [1 .. 80 :: Int] ]   -- Features [[Double]]
    ys  = [ 3 * fromIntegral i | i <- [1 .. 80 :: Int] ]                  -- Target [Double]
gen <- MWC.createSystemRandom
rf  <- fitRF defaultRFConfig xss ys gen
saveSVGBound "rf.svg" (noDf |>> toPlot rf)
```

| MultiLM residual correlation | PCA scree | RandomForest importance |
|---|---|---|
| ![](images/analyze-integration/multilm-resid-corr.svg) | ![](images/analyze-integration/pca-scree.svg) | ![](images/analyze-integration/rf-importance.svg) |

#### Route 1 Bayesian: HBM (probabilistic programs) {#hbm-plotting}

Just as Route 1's `toPlot` overlays frequentist models (LM/GLM/…), **Bayesian hierarchical models (HBM)** also plot the same way
(HBM Route 1 only, not stat-in). Probabilistic programs (`Hanalyze.Model.HBM`'s `ModelP`) are fit with `hbmModelPure` using NUTS,
yielding "trained HBM model" `HBMModel`. Equivalent to PyMC's `pm.sample`. `hbmModelPure` is
**pure & deterministic with seed**, taking IO → same seed → same result. IO version `hbmModel` (system random) also available.

```haskell
import Hanalyze.Plot
import Hanalyze.Model.HBM

-- y ~ Normal(a + b·x, s). PyMC-equivalent DAG (a,b → mu → obs, s → obs):
-- Using deterministic "mu" in observation loop relabels dependencies to det name,
-- making obs parents {mu, s}; same-name observe "obs" merges to one node.
-- epred evaluates muName="mu" at grid points (x=[xi]) under O1 convention.
-- Wrapping observation loop in plate shows repeated nodes (mu/obs) in DAG as "obs (N)" box with count
--   (PyMC plate equivalent, N = data points). plateForM_ collapses plate+length+forM_ to one line.
model :: ModelP ()
model = do
  x <- dataNamed "x" []          -- Placeholder; hbmModelPure auto-binds by column name
  y <- dataNamed "y" []
  a <- sample "a" (Normal 0 10)
  b <- sample "b" (Normal 0 10)
  s <- sample "s" (HalfNormal 1)
  plateForM_ "obs" (zip x y) $ \(xi, yi) -> do   -- Wrap in plate, repeat → DAG box+count
    mu <- deterministic "mu" (a + b * realToFrac xi)
    observe "obs" (Normal mu s) [yi]

-- Pure: specify seed in hbmSeed (or use defaultHBM = default seed 42) → fixed HBMModel
fit :: HBMModel
fit = hbmModelPure defaultHBM model [("x", xs), ("y", ys)]
```

`HBMModel` can't uniquely map to a single plot (probabilistic program). So **extractors** are used explicitly
(`df |>> toPlot (extractor fit)`). Default credible interval is **94% HDI** (ArviZ convention, differs from frequentist 95% Wald; adjustable with `statLevel`).

All accept `HBMModel` (`fit :: HBMModel`). Note return types (`epred` is `ModelSpec` composable with `<>`; list returns used with `subplots`; single types with `toPlot`):

| Extractor | Type (what to pass → what returns) | What to draw (ArviZ equivalent) |
|---|---|---|
| `epred` | `HBMModel -> Text -> Text -> ModelSpec` (data name, mean det name) | Posterior predictive mean + 94% HDI (`az.plot_lm`) |
| `traceOf` | `HBMModel -> [ChainModel]` | Each latent's trace (`az.plot_trace`) |
| `marginalsOf` | `HBMModel -> [VisualSpec]` | Each param's marginal posterior density (`az.plot_posterior`) |
| `forestOf` | `HBMModel -> ForestSpec` | Posterior mean + 94% HDI forest (`az.plot_forest`) |
| `ppcOf` | `HBMModel -> Text -> PPCSpec` (observed node name) | Observation vs posterior predictive (`az.plot_ppc`) |
| `dagOf` | `HBMModel -> DagSpec` | Model structure DAG (`pm.model_to_graphviz`) |
| `tracesByChainOf` / `marginalsByChainOf` | `HBMModel -> [VisualSpec]` | Trace / marginal posterior with chains color-coded |

```haskell
-- epred is ModelSpec so composes with grid/statLevel using <> (same as Route 1).
-- epred's HDI band is core, always shown (unlike Route 1's bandOn opt-in).
df |>> (layer (scatter "x" "y") <> toPlot (epred fit "x" "mu" <> grid 100 <> statLevel 0.9))

noDf |>> foldMap toPlot (traceOf fit)     -- Overlay all parameter traces
noDf |>> subplots (map toPlot (marginalsOf fit)) <> subplotCols 2  -- Marginal posteriors per-param in grid
noDf |>> toPlot (forestOf fit)            -- Coefficient forest (94% HDI)
noDf |>> toPlot (dagOf fit)               -- Model DAG
noDf |>> toPlot (ppcOf fit "obs")         -- ppc is pure (y_rep sampled via runST)
```

**Parameter selection (= ArviZ `var_names` equivalent)**: Extractors output all parameters, but multivariate hierarchical models often show only key parameters.
Per-param grid uses [`selectPanels`](04-decoration.md#subplots) (panel title = param name); forest uses `scaleYDiscreteLimits` (cat rows = param name). Both support **selection + order**:

```haskell
-- Show only 3 variables from trace grid (in this order, vertically)
noDf |>> subplots (tracesByChainOf fit) <> selectPanels ["b1_0", "b1_1", "sigma"]
       <> subplotCols 1
-- Show only group coefficients from forest (top to bottom: b1_0, b1_1, b1_2)
noDf |>> toPlot (forestOf fit) <> scaleYDiscreteLimits ["b1_0", "b1_1", "b1_2"]
```

| epred | forest | ppc | dag |
|---|---|---|---|
| ![](images/analyze-integration/hbm-epred.svg) | ![](images/analyze-integration/hbm-forest.svg) | ![](images/analyze-integration/hbm-ppc.svg) | ![](images/analyze-integration/hbm-dag.svg) |

> **Reading ppc's 3 colors** (like `az.plot_ppc`): **Black line = observed data** density; **thin blue lines (many) = each draw's posterior predictive replicate y_rep** density (model-generated "plausible data" spread); **red dashed = pooled y_rep density** (posterior predictive distribution). Observed (black) fitting within blue bundle and overlapping red = good fit (model reproduces data well). Black outside blue = mis-fit signal. `ppcOf` is pure (`ppcSeed` default 42 for reproducibility). Use IO version `ppcOfIO` only when system random needed.

> **DAG node shapes** (PyMC `model_to_graphviz` convention): **White oval = probabilistic latent** (`sample`, distribution name included); **gray oval = observed** (`observe`); **white box = deterministic** (`deterministic`, no distribution since derived); **rounded box frame = plate** (`plate "name" N` wraps repeats, frame label shows "name (N)" with **count**). Nodes scale to label width (long distribution names fit in frame). Repeated nodes (per-observation mu/obs etc.) collapse to one node, but plate frame count reveals data size.

**HBM diagnostic dashboard (one-sheet plot) — nested `subplots`**: Extractors are normal `VisualSpec`, so [nested subplots](04-decoration.md#subplots) bundle PyMC + ArviZ `az.plot_trace` groups into one diagnostic dashboard. **`traceOf` / `marginalsOf` return lists per variable**, stacking vertically with `subplots … <> subplotCols 1` creates "variable-wise vertical columns".

Define nested **column layers separately**, then combine into 1-row-5-column layout (DAG squeezed in tall column, so **dedicate left column to DAG**):

```haskell
let ppc = ppcOf fit "obs"   -- Pure. Use ppcOfIO if system random needed.

-- Column: DAG-only (gets crushed in tall column, so give it one full column).
let dagCol = toPlot (dagOf fit) <> title "Structure (DAG)"
-- Column: Posterior (marginal posterior densities) stacked per-variable (n×1). Multi-chain color-coded
    postCol  = subplots (marginalsByChainOf fit) <> subplotCols 1 <> title "Posterior (per-variable, chains)"
-- Column: Trace stacked per-variable (n×1). Similarly chain color-coded
    traceCol = subplots (tracesByChainOf fit)    <> subplotCols 1 <> title "Trace (per-variable, chains)"
-- Column: HDI (forest) and PPC stacked (2×1)
    hdiPpcCol = subplots [ toPlot (forestOf fit) <> title "HDI (forest 94%)"
                         , toPlot ppc            <> title "PPC" ] <> subplotCols 1 <> title "HDI / PPC"
-- Column: Posterior predictive (epred x/y resolved by outer df)
    epredCol = layer (scatter "x" "y") <> toPlot (epred fit "x" "mu") <> title "Posterior predictive"

-- Combine 5 columns in 1-row-5-column. DAG left. Widen to 2000px for DAG column
-- (wide-screen dashboard, pixel spec. Needs import Hgg.Plot.Unit (px, (*~)))
df |>> ( subplots [ dagCol, postCol, traceCol, hdiPpcCol, epredCol ] <> subplotCols 5
       <> widthUnit (2000 *~ px) <> heightUnit (600 *~ px)
       <> title "HBM diagnostic dashboard (structure / posterior / trace / HDI·PPC / posterior predictive)" )
```

> **Horizontal whitespace**: `subplots` has **independent, complete plots per panel** (each with y-axis ticks, labels, title). Panel spacing follows ggplot `panel.spacing` default = `half_line` (5.5pt), but most horizontal gap is from **neighboring panel's own y-axis strip** (patchwork-style independent plots). More columns show more gap; widening makes relative compression. > **Multi-chain**: `traceOf` / `marginalsOf` return **all chains concatenated/pooled as one**. To **overlay chains in same plot color-coded** (= ArviZ `plot_trace` default), use `tracesByChainOf` / `marginalsByChainOf` (each per-param plots colored chain layers). Visual mixing/convergence differences readable across chains.

Individual columns (separate plots):

| Posterior (per-variable, vertical) | Trace (per-variable, vertical) | HDI / PPC (vertical) |
|---|---|---|
| ![](images/analyze-integration/hbm-col-posterior.svg) | ![](images/analyze-integration/hbm-col-trace.svg) | ![](images/analyze-integration/hbm-col-hdippc.svg) |

Combined one-sheet plot (= DAG column + above columns + posterior predictive in 1×5. DAG fits in nested cell left):

![HBM diagnostic dashboard (structure DAG / posterior / trace / HDI·PPC / posterior predictive)](images/analyze-integration/hbm-dashboard.svg)

> **Note**: HBM extractors from analyze `Hanalyze.Plot`. DAG (`dagOf`) also **fits in nested cells** (DAG column above fits in nested cell example; tall columns squeeze it, so dedicate one full column). `epred` / `traceOf` / `forestOf` / `dagOf` / `ppcOf` all pure; `ppcOf` reproducible via `ppcSeed`. Use IO versions `ppcOfIO` / `ppcOfWithIO` only when system random needed. `PPCConfig { ppcReps, ppcSeed, ppcCumulative }` controls overlay count / seed / cumulative version (ecdf).

### Route 2: stat-in — `statLm` / `statSmooth` (ggplot-style stat-in) {#route2}

Like ggplot2's `geom_smooth(method="lm")`, **overlay stat as `layer (…)` like normal marks**, just compose with `<>`.
Regression computed by bridge (`hgg-analyze-bridge`) delegates to hanalyze. **df referenced once, decoration like normal marks `<>`**:

> **Scope**: stat-in offers **6 stats** — `statLm` / `statLmLevel` / `statSmooth` / `statSmoothCI` / `statPoly` / `statResid`.
> For diverse models (GLM / GP / survival / Bayesian, etc.), **use Route 1 (`toPlot`, 14 models)**.

```haskell
import           Hgg.Plot.Spec        ( statLm, statLmLevel, statSmooth, statSmoothCI
                                          , statPoly, statResid, colorBy, color, stroke )
import           Hgg.Plot.Color       (fromHex)
import           Hgg.Plot.Bridge.Stat (saveSVGBoundStats)

-- Equivalent to ggplot(df, aes(x,y)) + geom_point() + geom_smooth(method="lm", color="red")
saveSVGBoundStats "out.svg" $
  df |>> ( layer (scatter "x" "y")
         <> layer (statLm "x" "y" <> color (fromHex "#d62728") <> stroke 2)   -- Regression + 95% CI
         <> title "fit" )

-- smooth = B-spline smoothing curve (knot count, no band)
saveSVGBoundStats "out2.svg" $
  df |>> ( layer (scatter "x" "y") <> layer (statSmooth "x" "y" 8) )
```

**Stat list** (all `Layer`, wrap in `layer (…)`, decorate with `<>`):

| stat | Type (what to pass) | Meaning (ggplot equivalent) |
|---|---|---|
| `statLm` | `ColRef -> ColRef -> Layer` | Linear regression + 95% CI (`geom_smooth(method="lm")`) |
| `statLmLevel` | `ColRef -> ColRef -> Double -> Layer` | Confidence level specified (`level=0.99`) |
| `statSmooth` | `ColRef -> ColRef -> Int -> Layer` | B-spline smoothing (knot count, no band) |
| `statSmoothCI` | `ColRef -> ColRef -> Int -> Layer` | B-spline smoothing + CI band |
| `statPoly` | `ColRef -> ColRef -> Int -> Layer` | Polynomial regression (degree deg) + band |
| `statResid` | `ColRef -> ColRef -> Layer` | Residual vs fitted diagnostic scatter (`plot(lm)` #1) |

```haskell
-- Level 0.99 / B-spline with band / Quadratic / Residual diagnostics
df |>> layer (statLmLevel  "x" "y" 0.99)
df |>> layer (statSmoothCI "x" "y" 6)
df |>> layer (statPoly     "x" "y" 2)
df |>> layer (statResid    "x" "y")            -- Scatter (fitted, residual)

-- Group-wise fit: color names group column → fit per group, overlay with ggplot hue colors
--   (= geom_smooth(aes(color=g))). Align scatter color with same colorBy "g".
df |>> ( layer (scatter "x" "y" <> colorBy "g")
       <> layer (statLm  "x" "y" <> colorBy "g") )
```

Key points:
- **Stat is `Layer`** (`Hgg.Plot.Spec`, pure tags `MStatLM` / `MStatSmooth` / `MStatPoly` / `MStatResid`).
  Wrap in `layer (…)`, overlay with `<>`. Decorations (`color` / `stroke` / `alpha`) work via Layer `<>`,
  applying to resulting regression lines / bands / scatter.
- **`colorBy "g"` for group-wise fit**: When lyColor names group column, bridge splits per group and fits,
  overlay with ggplot hue colors. Scatter's same `colorBy "g"` color-matches.
- **Draw via `saveSVGBoundStats` / `renderBoundStats`** (bridge). `BoundPlot`'s resolver fits regression (`resolveStats`) before drawing → **df just `df |>>` once**.
- `saveSVGBound` (not bridge) leaves stat unresolved, warning printed (no regression drawn), so stat-in uses `saveSVGBoundStats` / `renderBoundStats`.

| lm (band, red line) | smooth (B-spline) | statSmoothCI (smooth + band) |
|---|---|---|
| ![](images/analyze-integration/lm-stat-in.svg) | ![](images/analyze-integration/smooth-stat-in.svg) | ![](images/analyze-integration/smooth-ci-stat-in.svg) |

| statLmLevel 0.99 (wide band) | statPoly deg=2 (quadratic + band) | statResid (residual diagnostics) | group-wise lm |
|---|---|---|---|
| ![](images/analyze-integration/lm-level99-stat-in.svg) | ![](images/analyze-integration/poly-stat-in.svg) | ![](images/analyze-integration/resid-stat-in.svg) | ![](images/analyze-integration/group-lm-stat-in.svg) |

### Example: Overlay multiple models on one plot {#multi-overlay}

Extractors are normal layers, so freely combine with [04 decoration](04-decoration.md) ornaments and [`subplots`](04-decoration.md#subplots).

**Model comparison — LM / GLM / spline in one plot**

Learning: `df |-> spec`, drawing: `toPlot`. Wrap each fit with **`statModel`, add `statColor`** (line color)
and **`statLabel`** (legend label), models color-code and legends stack:

```haskell
import Hanalyze.Plot      (toPlot, statModel, statColor, statLabel, (|->), lm, glm, spline)
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hanalyze.Model.Spline (SplineKind (..))
import Hgg.Plot.Color        (fromHex)   -- statColor takes Color

df |>> ( layer (scatter "x" "y")
       <> toPlot (statModel (df |-> lm "x" "y")              <> statColor (fromHex "#1f77b4") <> statLabel "LM")      -- Blue
       <> toPlot (statModel (df |-> glm Poisson Log "x" "y") <> statColor (fromHex "#ff7f0e") <> statLabel "GLM")     -- Orange
       <> toPlot (statModel (df |-> spline (BSpline 3) [4, 8] "x" "y") <> statColor (fromHex "#2ca02c") <> statLabel "spline") )  -- Green
```

![Model comparison: LM/GLM/spline color-coded and legend-labeled with statColor + statLabel](images/analyze-integration/model-comparison.svg)

> **Color & legend on overlay**: Different models in one plot stack each `statLabel`'s color and legend
> (all categories merge) — example above shows **3 colors + 3 legend entries**. Grouped by **column**?
> [`grouped`](#grouped-fitting-grouped-one-line-per-group) also outputs single color scale + legend.

**HBM diagnostic dashboard** — Probabilistic models can't uniquely map to one plot, so extractors
(`forestOf` / `traceOf` / `epred` / `dagOf` …) bundle via [`subplots`](04-decoration.md#subplots) into one sheet.
Complete example of structure DAG + posterior + trace + HDI·PPC + posterior predictive in one row is in
[HBM section dashboard](#hbm-plotting).
