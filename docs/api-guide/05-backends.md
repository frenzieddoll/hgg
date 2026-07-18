# backends — SVG / PDF / PNG / Jupyter

> 🌐 **English** | [日本語](05-backends.ja.md)

> [📚 Index](README.md) | [01 quickstart](01-quickstart.md) | [02 layers](02-layers.md) | [03 encoding & scale](03-encoding-scale.md) | [04 decoration](04-decoration.md) | **05 backends** | [06 dataframe](06-dataframe.md) | [07 analyze](07-analyze.md) | [08 3d](08-3d.md) | [09 appendix](09-appendix.md)

Output the same `VisualSpec` / `BoundPlot` to different format packages by use case. Backends work independently from the plot core (no extra dependencies like df needed).

Structure of this page (by backend):
**[SVG](#be-svg)** (primary use) | **[Jupyter inline](#be-jupyter)** | **[PDF](#be-pdf)** |
**[PNG](#be-png)** | **[Low-level output / other](#be-lowlevel)**

### SVG — `hgg-svg` (primary, practical use) {#be-svg}

File-saving variants (`IO`, writes files) and rendering variants (pure, returns `Text`), further split by **Resolver requirement** into 3 forms. Naming convention: **"no suffix = normal (Resolver not needed), `With` = Resolver included, `Bound` = DataFrame"**.

| Function | Output | How to pass data | Use case |
|---|---|---|---|
| `saveSVG path spec` | `IO ()` | `inline` embedded in spec | **Normal**. Figures without column names |
| `saveSVGWith path r spec` | `IO ()` | Pass `Resolver` manually | Figures with `ColByName` (low-level) |
| `saveSVGBound path bound` | `IO ()` | Bundle with `df |>> spec` | **DataFrame**. Write with column names ([06 dataframe](06-dataframe.md)) |
| `renderSVG spec` | `Text` | inline | Want string (not file) |
| `renderSVGWith r spec` | `Text` | `Resolver` | Same + Resolver |
| `renderBound bound` | `Text` | `BoundPlot` | Same + DataFrame |
| `saveSVGInteractive path r spec` | `IO ()` | `Resolver` | SVG with inline JS for hover + drag pan + wheel zoom |
| `plot path spec` | `IO ()` | inline | Alias for `saveSVG` (matplotlib `savefig` style) |

```haskell
import Graphics.Hgg.Backend.SVG (saveSVG, saveSVGInteractive)

main = do
  saveSVG            "static.svg"      spec
  saveSVGInteractive "interactive.svg" emptyResolver spec   -- pan/zoom in browser
```

#### Differences between `saveSVG` / `saveSVGWith` / `saveSVGBound` / `|>>`

Organize save function choices (4 operator roles are primary reference at [operator quick reference](README.md#operator-quick-reference)).
**`|>>` is not a save function but "pure bundle function" — one level up** (`BoundPlot` value created only, no file written). `df |>>` detailed in [06 dataframe](06-dataframe.md).

| | Type | Data | Column validation | When to use |
|---|---|---|---|---|
| `saveSVG path spec` | `IO`(output) | `inline` embedded in spec | ─ | Figures with directly written values |
| `saveSVGWith path r spec` | `IO`(output) | Pass `Resolver` manually | ─ | When you hold Resolver yourself |
| `(\|>>)` | **Pure bundle** | df passed, resolved internally | **yes** (`bpDiagnostics`) | Pre-step for df + column names ([06 dataframe](06-dataframe.md)) |
| `saveSVGBound path bound` | `IO`(output) | Bundle created by `\|>>` | (included in bundle) | Output from `\|>>` |

```haskell
-- DataFrame route (recommended): bundle with |>>, output with saveSVGBound ([06 dataframe](06-dataframe.md))
saveSVGBound "out.svg" (df |>> layer (scatter "weight" "mpg"))

-- Low-level: prepare Resolver yourself and use saveSVGWith
saveSVGWith "out.svg" myResolver (layer (scatter "weight" "mpg"))

-- inline only: no column names, use saveSVG
saveSVG "out.svg" (layer (scatter (inline ws) (inline ms)))
```

Implementation is thin wrappers: `saveSVG path = saveSVGWith path emptyResolver`,
`renderBound (BoundPlot r spec _) = renderSVGWith r spec`. To skip validation completely, extract `(Resolver, VisualSpec)` with `unBound bound` and pass to `saveSVGWith`.

Rendering settings like size and margins are **on the spec side**, not backend arguments
(`width n` / `height n` / `aspectRatio r` / `theme …` · [04 decoration](04-decoration.md)). Backend only handles output format choice.

### Jupyter inline — `hgg-ihaskell` (Experimental) {#be-jupyter}

Simply import the `IHaskellDisplay` instance and cell evaluation values render inline.

```haskell
import Graphics.Hgg.Easy
import Graphics.Hgg.IHaskell ()    -- just expose the instance

layer (points [0,1,2,3] [0,1,4,9]) <> title "demo"   -- cell evaluation → inline SVG
```

Figures with `ColByName` (= Resolver needed) pass the `BoundPlot` from `df |>> spec` directly as cell value (`instance IHaskellDisplay BoundPlot`).

### PDF — `savePDF` (HPDF) {#be-pdf}

Output vector PDF with `hgg-pdf` (for papers, reports). API symmetric with SVG backend: `savePDF path spec` (inline columns only) / `savePDFWith path r spec` (figures with `ColByName`) / `savePDFBound path bp` (`BoundPlot` from `df |>> spec`).

```haskell
import Graphics.Hgg.Backend.PDF (savePDF)

savePDF "fig1.pdf" (layer (scatter (inline xs) (inline ys)) <> title "Figure 1")
```

> ⚠️ **v1 limitation: No Japanese labels**. Only PDF standard 14 fonts (Helvetica / Times / Courier = Latin), so non-Latin-1 characters replaced with `?` and warning. For Japanese labels, use PNG backend (`hgg-rasterific` · TrueType reading · next section). Weight/italic maps to 4 variants; `"serif"`/`"monospace"` families map to Times/Courier.

### PNG — `savePNG` (Rasterific · Japanese supported) {#be-png}

Output raster PNG with `hgg-rasterific` (pure Haskell · no external binary dependency). API symmetric with SVG/PDF backend: `savePNG path spec` (inline columns only) / `savePNGWith path r spec` (figures with `ColByName`) / `savePNGBound path bp` (`BoundPlot` from `df |>> spec`).

```haskell
import Graphics.Hgg.Backend.Rasterific (savePNG)

savePNG "fig1.png" (layer (scatter (inline xs) (inline ys)) <> title "Figure 1: scatter")
```

Fonts use TrueType (.ttf) reading = **Japanese label support** (PNG backend as fallback for PDF v1 limitation). Search is fontconfig-independent: ① explicit `pngFontPath` → ② known directories (`~/.fonts`, `~/.local/share/fonts`, `/usr/share/fonts`, `/usr/local/share/fonts`, `/mnt/c/Windows/Fonts`) × known filenames (HackGen / Noto Sans CJK JP / IPA / Takao / DejaVu .ttf) in order. Not found lists paths and errors.

```haskell
import Graphics.Hgg.Backend.Rasterific

-- Explicit font + Hi-DPI 2× (dimensions, line width, text scale 2×)
savePNGConfigured defaultPNGConfig { pngFontPath = Just "/path/to/font.ttf"
                                   , pngScale    = 2.0 }
                  "fig1@2x.png" emptyResolver spec
```

> ⚠️ **v1 limitations**: ① `.ttc` (TrueType Collection · Windows meiryo/msgothic etc.) and CFF OTF unreadable (.ttf only). ② Font family not distinguished; regular/bold 2 faces only (weight ≠ "bold" and italic use regular fallback).

### Low-level output / other {#be-lowlevel}

Above `save*` suffices normally, but lower-level outlets exist:

```haskell
-- Get interactive SVG as "string" (Text version of saveSVGInteractive)
renderSVGInteractive emptyResolver spec :: Text

-- Emit Primitive list directly to PDF / PNG (backend authoring, special rendering, see Appendix C)
savePrimitivesPDF "out.pdf" 640 480 prims              -- :: FilePath -> Int -> Int -> [Primitive] -> IO ()
savePrimitivesPNG defaultPNGConfig "out.png" 640 480 prims

-- Directly pass Hackage dataframe to save (shortcut bypassing df |>> spec)
plotDF "out.svg" df (layer (scatter "weight" "mpg"))   -- :: FilePath -> DataFrame -> VisualSpec -> IO ()
```

> `savePrimitives*` is the output port folding `[Primitive]` (from `renderToPrimitives`) to format, symmetric with SVG version `savePrimitivesSVG` (backend authoring in [Appendix C](09-appendix.md#appendix-extend)). Config types `PNGConfig` / `PNGFonts` set PNG backend. Bundle result typeclass for `df |>>` is `BindableSpec` ([06 dataframe](06-dataframe.md)).
