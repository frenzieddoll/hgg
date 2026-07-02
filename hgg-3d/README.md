# hgg-3d

3D plot library for hgg。 matplotlib mplot3d 同等の API + 2D 同型の Monoid 構文 + WebGL interactive 表示。

## 機能

| 機能 | 状態 |
|---|---|
| 3D MarkKind (Scatter / Line / Wireframe / Surface) | ✅ Phase 3 |
| CPU projection → SVG / PDF / Rasterific (既存 2D backend) | ✅ Phase 3 |
| 2D 同型 Monoid API (`purePlot3D <> layer3D (scatter3D ...) <> ...`) | ✅ Phase 5 |
| Browser interactive 表示 (`showBrowser`、 WebGL2 backend) | ✅ Phase 5 |
| Self-contained HTML 出力 (`saveHTML3D`) | ✅ Phase 5 |
| Camera control (= orbit / zoom / pan) | ✅ Phase 4 (WebGL) |

## 使い方

### 1. Browser interactive 表示 (= showBrowser)

```haskell
import Hgg.Plot.ThreeD
import Hgg.Plot.ThreeD.Spec
import Hgg.Plot.ThreeD.Browser  (showBrowser)

main :: IO ()
main = showBrowser $
     purePlot3D
  <> layer3D (scatter3D helixPts <> color3D "#56B4E9" <> size3D 6 <> alpha3D 0.9)
  <> layer3D (line3D    linePts  <> color3D "#009E73" <> width3D 1.5)
  <> camera     (defaultCameraZUp 3)
  <> projection defaultPerspective
  <> axes3D     defaultAxes3D
  <> title3D    "demo"
```

操作: 左ドラッグ orbit / 右ドラッグ pan / wheel zoom。

### 2. 静的 SVG 出力 (= saveSVG3D)

```haskell
import Hgg.Plot.ThreeD.Easy (saveSVG3D)

main = saveSVG3D "out.svg" spec  -- 同じ spec を使い回し
```

Phase 3 CPU projection で SVG 生成。 印刷 / doc 埋込向け。

### 3. Self-contained HTML 配布 (= saveHTML3D)

```haskell
import Hgg.Plot.ThreeD.Browser (saveHTML3D)

main = saveHTML3D "out.html" spec
```

bundle JS + spec JSON を inline 埋込した単一ファイル。 外部依存無しで配布可能。

## API 概要

### 2D 同型構文

```haskell
-- 2D (hgg-core)
purePlot <> layer (scatter (ColByName "x") (ColByName "y") <> color "red")

-- 3D (本 package)、 構文 100% 同型
purePlot3D <> layer3D (scatter3D pts <> color3D "red")
```

| 用途 | 2D | 3D |
|---|---|---|
| 純粋起点 | `purePlot` | `purePlot3D` |
| layer lift | `layer` | `layer3D` |
| Mark | `scatter / line / bar / ...` | `scatter3D / line3D / wireframe3D / surface3D` |
| 色 | `color` | `color3D` |
| 数値属性 | `size / alpha` | `size3D / alpha3D / width3D` |
| 全体属性 | `title / xLabel / theme` | `title3D / camera / projection / axes3D` |
| 出力 | `saveSVG / savePDF` | `saveSVG3D / saveHTML3D / showBrowser` |

### Layer3D 属性一覧

| 属性 | 関数 | 対象 Mark |
|---|---|---|
| 色 | `color3D` | 全 Mark |
| 線色 | `edgeColor3D` | Surface |
| 点 size | `size3D` | Scatter |
| alpha | `alpha3D` | Scatter |
| 線幅 | `width3D` | Line / Wireframe |
| 陰影 on/off | `shaded3D` | Surface |
| x 範囲 | `xRange3D` | Surface |
| y 範囲 | `yRange3D` | Surface |

### VisualSpec3D 属性

| 属性 | 関数 |
|---|---|
| Camera | `camera :: Camera3D -> VisualSpec3D` |
| Projection | `projection :: Projection3D -> VisualSpec3D` |
| Axes | `axes3D :: Axes3D -> VisualSpec3D` |
| Title | `title3D :: Text -> VisualSpec3D` |
| Width / Height | `width3DV / height3DV :: Int -> VisualSpec3D` |

## アーキテクチャ

```
ユーザコード (HS)
  ↓ purePlot3D <> layer3D (...) <> ...
VisualSpec3D
  ↓ saveSVG3D (= Hgg.Plot.ThreeD.Easy)
  Phase 3 CPU projection → [Primitive] → savePrimitivesSVG (hgg-svg)
  ↓ saveHTML3D / showBrowser (= Hgg.Plot.ThreeD.Browser)
  aeson encode → JSON inline 埋込 + bundle JS (= data-files) inline → HTML
                  ↓
              ブラウザ (= PS hgg-canvas、 WebGL2 backend)
```

WebGL bundle (= `data/webgl-spec.js`、 ~114KB) は cabal data-files で同梱。
PS 側 (`hgg-canvas`) を更新した場合の再 bundle 手順:

```bash
cd ../hgg-canvas
spago bundle --module Examples.WebGLPlot --outfile ../hgg-3d/data/webgl-spec.js --platform browser
# HS 側は再 build 不要 (= data-files なので)
```

bundle embed 方式の選択理由は [`../design/bundle-embed-choice.md`](../design/bundle-embed-choice.md)。

## demo 実行

```bash
cabal run browser-3d-demo  # tmp HTML + xdg-open でブラウザ起動 (Linux/macOS/Windows)
```

WSL 環境で xdg-open 失敗時は、 メッセージに表示される tmp path (`/tmp/hgg-3d-tmp.html`) を
Windows 側で手動で開く (= file:// or scp 後 browser で)。

## 関連

- Phase 3 plan: `../specification/phases/phase-3-3d-cpu-projection.md` (= CPU projection)
- Phase 4 plan: `../specification/phases/phase-4-3d-webgl.md` (= PS WebGL backend)
- Phase 5 plan: `../specification/phases/phase-5-browser-display.md` (= 本 package の HS-side wrap)
- Architecture: `../specification/spec/hgg-architecture-spec.md` §6.3
