# モジュールとできること

> 🌐 [English](modules.ja.md) | **日本語**

## パッケージ構成

core は描画先非依存 (base / vector / text / containers のみ)。 出力先ごとに backend package を選ぶ。

| Package | 役割 | 言語 | 状態 |
|---|---|---|---|
| `hgg-core` | Spec / Layout / Render 抽象 / Palette / DAG | Haskell (純) | ✅ |
| `hgg-svg` | SVG backend (`saveSVG` 等) | Haskell | ✅ 実用 |
| `hgg-3d` | 3D plot (CPU 投影、 mplot3d 同等) | Haskell | ✅ |
| `hgg-pdf` | PDF backend (HPDF・Latin のみ) | Haskell | ✅ (Phase 17) |
| `hgg-rasterific` | PNG backend (Rasterific + FontyFruity・日本語可) | Haskell | ✅ (Phase 22) |
| `hgg-dataframe` | Hackage dataframe → Resolver bridge | Haskell | ✅ |
| `hgg-analyze-bridge` | hanalyze 直 plot bridge | Haskell | ✅ |
| `hgg-doe` | DoE 専用 helper (MainEffects/Interaction/ResponseSurface) | Haskell | ✅ |
| `hgg-canvas` | Halogen / web-canvas + WebGL2 3D backend | PureScript | ✅ 実戦投入中 |
| `hgg-doe-canvas` | PS DoE 専用 helper | PureScript | ✅ |

### core の主要モジュール

| モジュール | 責務 |
|---|---|
| `Hgg.Plot.Spec` | 図の宣言型仕様 (Grammar API)。 geom / channel / scale / theme / facet |
| `Hgg.Plot.Easy` | 入門 API。 Spec を再 export + `[Double]` 直渡しヘルパ + `overlay` |
| `Hgg.Plot.Layout` | domain 計算・track 配置・facet レイアウト |
| `Hgg.Plot.Render` | `Primitive` (幾何プリミティブ) と backend 抽象 |
| `Hgg.Plot.Palette` | Hgg ブランド + 学術 palette、 sequential/diverging/cyclical |
| `Hgg.Plot.DAG` | グラフ図 (graphviz dot 相当、 Sugiyama 配置) |
| `Hgg.Plot.Validate` | spec の妥当性検査 |

PureScript (`hgg-canvas`) は **同一 ADT** を持ち、 aeson ↔ Argonaut の JSON
round-trip を保証する。 backend を入れ替えても spec は同じ。

---

## 描けるもの

> 各 chart の 1 行コード例 + 出力 SVG は **../design/gallery/README.md** に視覚カタログがある。

### 基本 chart
scatter / line / bar / histogram / box / heatmap / pie / waterfall / contour / step / stem
/ parallel coords / band (CI ribbon)

### 分布
violin / strip / swarm / raincloud / ridge (joyplot)
→ 群比較は `cabal run tutorial-04-distribution`

### 統計特化
density (KDE) / trace (MCMC) / pairs plot / regression line + CI / stat line (mean/median)
/ forest / ESS / Q-Q plot / **DAG** (graphviz 相当)

### 3D (mplot3d 同等 + interactive)
- CPU 投影 (SVG/PDF/PNG): scatter3D / line3D / wireframe3D / surface3D
- WebGL interactive (browser): orbit / zoom / pan
- `showBrowser` で 1 行起動

### DoE (`hgg-doe`)
mainEffects / interaction / responseSurface

### 座標系 (Phase 10/11)
`coordFlip` (xy 入替) / `coordCartesian` (データ非破棄 zoom) / `coordPolar` (rose 等)

### 軸
linear / log / format (Integer/Decimal/Exponent) ✅、 sqrt / time は spec 定義済 (Layout wip)

### 装飾
facet (Trellis、 free scales 対応) / subplots (1D/2D grid) / pairs / inset axes
/ annotation (text/arrow/rect/line) / dual Y axis / marginal hist+density / legend chip (4 position)

### position adjustment
`PosDodge` (横並び) / `PosStack` (積み上げ) / `PosFill` (100%) / identity

### テーマ・配色
- テーマ: `ThemeDefault` / `ThemeMinimal` / `ThemeDark` / `ThemeLight`
  / `ThemeCanvas` (羊皮紙・明) / `ThemeCanvasDark`
- series palette: ggplot 標準 hue_pal + 学術 palette + Hgg ブランド (キャラ別 7 系統)
- `ThemeOverride` で element 単位の上書き
→ 明/暗の出し分け: `cabal run tutorial-05-theme`

### フォント
`titleFont` / `axisLabelFont` / `tickFont` / `legendFont` + `fontSize` / `fontFamily`
/ `fontWeight` / `fontColor` (`<>` 合成可。 現状 title + axis label が Render 完了)

### scale
`scaleColorManual` (カテゴリ→色辞書) / `scaleColorGradient2` (発散) / `scaleSize` (size range)
/ `colorContinuous` / `sizeBy` / `*_reverse` 相当 (range 入替)

---

## 機能の安定度

| 区分 | 内容 |
|---|---|
| **Stable** | SVG backend の基本 chart・分布・統計・座標系・テーマ・facet (= ギャラリー掲載分) |
| **Experimental** | sqrt/time 軸 (Layout wip)、 tick/legend フォント wire、 真の stacked-pie、 PDF backend (Phase 17・Latin のみ)、 PNG backend (Phase 22・日本語可) |
| **Planned** | Hackage / npm 公開 (正式版 リリース後) |
