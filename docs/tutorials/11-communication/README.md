# 11. Communication

> 🌐 **English** | [日本語](README.ja.md)

> Primary source: **R for Data Science 2e, Ch.11 "Communication"**
> <https://r4ds.hadley.nz/communication>
> Data: **mpg** (ggplot2)

Polish exploratory plots into "presentation-ready" figures. We arrange **non-data elements**: labels,
annotations, legend position, color palette, theme, zoom, and axis ticks. Execution code is in
[`Communication.hs`](Communication.hs).

## Running

```sh
cd docs/tutorials/11-communication
cabal run tut-11-communication
```

---

## 1. Labels (= `labs(...)`)

Add title, subtitle, caption, axis names, and legend name.

| R | hgg |
|---|---|
| `labs(title=, subtitle=, caption=)` | `title` / `subtitle` / `caption` |
| `labs(x=, y=, color=)` | `xLabel` / `yLabel` / `legendTitle` |

![labels](01-labels.svg)

## 2. Annotations (= `annotate()` / `geom_text`)

Place text or arrows at data coordinates to highlight key points.

| R | hgg |
|---|---|
| `annotate("label", x, y, label=)` | `annotText x y "…"` |
| `annotate("segment", …, arrow=)` | `annotArrow x1 y1 x2 y2` |

![annotate](02-annotate.svg)

## 3. Legend position (= `theme(legend.position=)`)

| R | hgg |
|---|---|
| `theme(legend.position = "bottom")` | `legendPos LegendBottom` |
| `theme(legend.position = "none")` | `legendOff` |

![legend bottom](03-legend-bottom.svg)

`LegendPosition` includes `LegendRight` (default) / `LegendBottom` / `LegendNone`, plus plot-internal
positions like `LegendInsideTopRight`.

## 4. Color palette + shape redundant encoding (color-blind accessibility)

Assigning both color and shape to the same variable makes it intelligible to those with color
blindness (R4DS's `scale_color_brewer` + `aes(shape=)`).

| R | hgg |
|---|---|
| `scale_color_brewer(palette = "Set1")` | `palette tolBright` (color-blind safe) |
| `aes(color = drv, shape = drv)` | `colorBy "drv" <> shapeBy "drv"` |

![palette shape](04-palette-shape.svg)

Available palettes: `okabeIto` / `tolBright` / `brewerSet2` / `brewerDark2` (all leaning toward
color-blind friendly).

## 5. Themes (= `theme_bw()` / `theme_minimal()` …)

Switch non-data appearance (background, grid, border) wholesale.

| R | hgg |
|---|---|
| `theme_bw()` | `theme ThemeBW` |
| `theme_minimal()` | `theme ThemeMinimal` |

![theme bw](05-theme-bw.svg)
![theme minimal](06-theme-minimal.svg)

`ThemeName` options include `ThemeDefault` / `ThemeBW` / `ThemeMinimal` / `ThemeClassic` /
`ThemeGrey` / `ThemeLight` / `ThemeDark` / `ThemeVoid` / `ThemeLinedraw`, etc.

## 6. Zoom (= `coord_cartesian()`)

Narrow display range. **Data aren't dropped**, so smooth lines remain computed from all data and just
get magnified (unlike `filter`, which changes smooths themselves—R4DS's point).

| R | hgg |
|---|---|
| `coord_cartesian(xlim = c(5,7), ylim = c(10,25))` | `coordCartesianX 5 7 <> coordCartesianY 10 25` |

![zoom](07-zoom.svg)

## 7. Axis tick specification (= `scale_y_continuous(breaks=)`)

| R | hgg |
|---|---|
| `scale_y_continuous(breaks = seq(15, 40, by = 5))` | `yAxis (axisBreaksAt [15,20,25,30,35,40])` |

![axis breaks](08-axis-breaks.svg)

---

## Correspondence table this chapter (Summary)

| ggplot2 | hgg |
|---|---|
| `labs(title/subtitle/caption=)` | `title` / `subtitle` / `caption` |
| `labs(x/y/color=)` | `xLabel` / `yLabel` / `legendTitle` |
| `annotate("label"/"segment")` | `annotText` / `annotArrow` |
| `theme(legend.position=)` | `legendPos LegendBottom` / `legendOff` |
| `scale_color_brewer()` | `palette tolBright`, etc. |
| `aes(shape=)` (redundant encoding) | `shapeBy "g"` |
| `theme_bw()` / `theme_minimal()` | `theme ThemeBW` / `theme ThemeMinimal` |
| `coord_cartesian(xlim/ylim=)` | `coordCartesianX` / `coordCartesianY` |
| `scale_y_continuous(breaks=)` | `yAxis (axisBreaksAt […])` |

Previous chapter → [`10-eda`](../10-eda/).
Next chapter → [`17-datetimes`](../17-datetimes/) (Ch.17 Dates and times, flights).
