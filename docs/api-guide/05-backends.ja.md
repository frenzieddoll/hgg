# backend ─ SVG / PDF / PNG / Jupyter

> [📚 索引](README.ja.md) ｜ [01 quickstart](01-quickstart.ja.md) ｜ [02 layers](02-layers.ja.md) ｜ [03 encoding & scale](03-encoding-scale.ja.md) ｜ [04 decoration](04-decoration.ja.md) ｜ **05 backends** ｜ [06 dataframe](06-dataframe.ja.md) ｜ [07 analyze](07-analyze.ja.md) ｜ [08 3d](08-3d.ja.md) ｜ [09 appendix](09-appendix.ja.md)

同じ `VisualSpec` / `BoundPlot` を、 用途別の backend package で出力する。 backend は
plot 本体だけで完結する (df 等の追加依存は不要)。

このページの構成 (backend 別):
**[SVG](#be-svg)** (主用途) ｜ **[Jupyter inline](#be-jupyter)** ｜ **[PDF](#be-pdf)** ｜
**[PNG](#be-png)** ｜ **[低レベル出力 / その他](#be-lowlevel)**

### SVG ─ `hgg-svg` (主用途・実用) {#be-svg}

保存系 (`IO`、 ファイルを書く) と描画系 (純粋、 `Text` を返す) があり、 さらに
**Resolver の要否**で 3 通り。 名前の規則は **「無印 = 通常 (Resolver 不要)、
`With` = Resolver 同伴、 `Bound` = DataFrame」**。

| 関数 | 出力 | データの渡し方 | 用途 |
|---|---|---|---|
| `saveSVG path spec` | `IO ()` | `inline` で spec に直接 | **通常**。 列名を使わない図 |
| `saveSVGWith path r spec` | `IO ()` | `Resolver` を手で渡す | `ColByName` を含む図 (低レベル) |
| `saveSVGBound path bound` | `IO ()` | `df \|>> spec` で束ねる | **DataFrame**。 列名で書く ([06 dataframe](06-dataframe.ja.md)) |
| `renderSVG spec` | `Text` | inline | 文字列が欲しい (ファイルにしない) |
| `renderSVGWith r spec` | `Text` | `Resolver` | 同上 + Resolver |
| `renderBound bound` | `Text` | `BoundPlot` | 同上 + DataFrame |
| `saveSVGInteractive path r spec` | `IO ()` | `Resolver` | hover + drag pan + wheel zoom の inline JS 付き SVG |
| `plot path spec` | `IO ()` | inline | `saveSVG` の別名 (matplotlib `savefig` 感) |

```haskell
import Graphics.Hgg.Backend.SVG (saveSVG, saveSVGInteractive)

main = do
  saveSVG            "static.svg"      spec
  saveSVGInteractive "interactive.svg" emptyResolver spec   -- ブラウザで pan/zoom
```

#### `saveSVG` / `saveSVGWith` / `saveSVGBound` / `|>>` の違い

保存関数の選び方を整理する (4 演算子の役割は [演算子早見表](README.ja.md#演算子早見表) が一次根拠)。
**`|>>` だけは保存関数ではなく「データを図に束ねる純関数」**で、 階層が 1 つ上 (`BoundPlot` と
いう値を作るだけ・ファイルは書かない)。 `df |>>` は [06 dataframe](06-dataframe.ja.md) で詳説する。

| | 種類 | データ | 列名検証 | いつ使う |
|---|---|---|---|---|
| `saveSVG path spec` | `IO`(出力) | `inline` を spec に埋込済 | ─ | 値を直接書いた図 |
| `saveSVGWith path r spec` | `IO`(出力) | `Resolver` を手渡し | ─ | 自分で Resolver を持っている |
| `(\|>>)` | **純関数(束ね)** | df を渡すと中で解決 | **する** (`bpDiagnostics`) | df + 列名で書く前段 ([06 dataframe](06-dataframe.ja.md)) |
| `saveSVGBound path bound` | `IO`(出力) | `\|>>` が作った束を消費 | (束に同梱済を報告) | `\|>>` の出力 |

```haskell
-- DataFrame ルート (推奨): |>> で束ねて saveSVGBound で出す (06 dataframe)
saveSVGBound "out.svg" (df |>> layer (scatter "weight" "mpg"))

-- 低レベル: Resolver を自分で用意して saveSVGWith
saveSVGWith "out.svg" myResolver (layer (scatter "weight" "mpg"))

-- inline だけ: 列名を使わないなら saveSVG
saveSVG "out.svg" (layer (scatter (inline ws) (inline ms)))
```

実体は薄いラッパで、 `saveSVG path = saveSVGWith path emptyResolver`、
`renderBound (BoundPlot r spec _) = renderSVGWith r spec`。 検証を完全に外したい
ときは `unBound bound` で `(Resolver, VisualSpec)` を取り出し `saveSVGWith` に渡す。

サイズ・余白などの「描画設定」 は backend 引数ではなく **spec 側**で持つ
(`width n` / `height n` / `aspectRatio r` / `theme …`、 [04 decoration](04-decoration.ja.md))。 backend は
出力フォーマットの選択だけを担う。

### Jupyter inline ─ `hgg-ihaskell` (Experimental) {#be-jupyter}

`IHaskellDisplay` instance を import するだけで、 セル評価値がインライン描画される。

```haskell
import Graphics.Hgg.Easy
import Graphics.Hgg.IHaskell ()    -- instance を見せるだけ

layer (points [0,1,2,3] [0,1,4,9]) <> title "demo"   -- セル評価でインライン SVG
```

`ColByName` を含む図 (= Resolver 必須) は `df |>> spec` の `BoundPlot` をそのまま
セル値にする (`instance IHaskellDisplay BoundPlot`)。

### PDF ─ `savePDF` (HPDF) {#be-pdf}

`hgg-pdf` でベクタ PDF を出力できる (論文・レポート向け)。 API は
SVG backend と対称: `savePDF path spec` (inline 列のみ) /
`savePDFWith path r spec` (`ColByName` 含む図) / `savePDFBound path bp`
(`df |>> spec` の `BoundPlot`)。

```haskell
import Graphics.Hgg.Backend.PDF (savePDF)

savePDF "fig1.pdf" (layer (scatter (inline xs) (inline ys)) <> title "Figure 1")
```

> ⚠️ **v1 制約: 日本語ラベルは出ない**。 PDF 標準 14 フォント (Helvetica/Times/
> Courier 系 = Latin) のみのため、 非 Latin-1 文字は警告つきで `?` に置換される。
> 日本語ラベルの図は PNG backend (`hgg-rasterific`、 TrueType 読込、
> 次節) を使う。 weight/italic は 4 変種に、 `"serif"`/`"monospace"` 系
> family は Times/Courier にマップされる。

### PNG ─ `savePNG` (Rasterific・日本語可) {#be-png}

`hgg-rasterific` で raster PNG を出力できる (純 Haskell・外部バイナリ
依存なし)。 API は SVG/PDF backend と対称: `savePNG path spec` (inline 列のみ) /
`savePNGWith path r spec` (`ColByName` 含む図) / `savePNGBound path bp`
(`df |>> spec` の `BoundPlot`)。

```haskell
import Graphics.Hgg.Backend.Rasterific (savePNG)

savePNG "fig1.png" (layer (scatter (inline xs) (inline ys)) <> title "図 1: 散布図")
```

フォントは TrueType (.ttf) 読込 = **日本語ラベル対応** (PDF v1 制約の受け皿)。
探索は fontconfig 非依存で、 ①`pngFontPath` 明示指定 → ②既知ディレクトリ
(`~/.fonts`, `~/.local/share/fonts`, `/usr/share/fonts`, `/usr/local/share/fonts`,
`/mnt/c/Windows/Fonts`) × 既知ファイル名候補 (HackGen / Noto Sans CJK JP /
IPA / Takao / DejaVu の .ttf) の順。 見つからない場合は探索パスを列挙して
エラーになる。

```haskell
import Graphics.Hgg.Backend.Rasterific

-- フォント明示 + Hi-DPI 2 倍 (寸法・線幅・文字が比例して 2x)
savePNGConfigured defaultPNGConfig { pngFontPath = Just "/path/to/font.ttf"
                                   , pngScale    = 2.0 }
                  "fig1@2x.png" emptyResolver spec
```

> ⚠️ **v1 制約**: ① `.ttc` (TrueType Collection・Windows の meiryo/msgothic 等) と
> CFF 系 OTF は読めない (.ttf のみ)。 ② フォント family は区別せず regular/bold
> の 2 face のみ (`"bold"` 以外の weight と italic は regular で代替)。

### 低レベル出力 / その他 {#be-lowlevel}

通常は上記の `save*` で足りるが、 より低レベルな出力口もある:

```haskell
-- インタラクティブ SVG を「文字列」で得る (saveSVGInteractive の Text 版)
renderSVGInteractive emptyResolver spec :: Text

-- Primitive 列を直接 PDF / PNG へ (backend 自作・特殊描画で使う。付録 C 参照)
savePrimitivesPDF "out.pdf" 640 480 prims              -- :: FilePath -> Int -> Int -> [Primitive] -> IO ()
savePrimitivesPNG defaultPNGConfig "out.png" 640 480 prims

-- Hackage dataframe を直接渡して保存 (df |>> spec を経ない近道)
plotDF "out.svg" df (layer (scatter "weight" "mpg"))   -- :: FilePath -> DataFrame -> VisualSpec -> IO ()
```

> `savePrimitives*` は `renderToPrimitives` で得た `[Primitive]` をフォーマットへ畳む口で、
> SVG 版 `savePrimitivesSVG` と対称 ([付録 C](09-appendix.ja.md#appendix-extend) の backend 自作)。
> 設定型 `PNGConfig` / `PNGFonts` は PNG backend の構成。 `df |>>` の束ね結果の型クラスは
> `BindableSpec` ([06 dataframe](06-dataframe.ja.md))。
