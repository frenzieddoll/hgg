# Vega-Lite との比較

> 🌐 [English](comparison-vega-lite.ja.md) | **日本語**

[matplotlib / ggplot2 との比較](comparison.ja.md) は命令型/宣言型の軸だった。 ここでは
**hgg に最も近い類縁である Vega-Lite** と比較する。 両者とも

> 「データ + マーク + エンコーディングを **宣言的な spec (AST)** として組み立て、
>  レンダラがそれを描画する」

という grammar of graphics を、 **spec 中心** (matplotlib のような状態機械でなく) で
実装している。 hgg の `VisualSpec` は Vega-Lite の JSON spec に対応する。

本書はとくに、 連携が強化された 2 軸 — **DataFrame 連携** と **統計エンジン (hanalyze)
連携** — に焦点を当てて評価する。

> ⚠ 本書の hgg 側 API は実装を確認した事実 (`hgg-frame` /
> `hgg-analyze-bridge` / analyze `Hanalyze.Plot`)。 Vega-Lite 側は v5 の
> 公式文法に基づく。 細部はバージョンで変わり得る。

## ひとことで

- **同じ「宣言的 spec」 思想** ─ Vega-Lite=JSON、 hgg=`VisualSpec` (Haskell ADT)。
  Vega-Lite の `layer` 配列は hgg の `<>` (Monoid 合成) に対応。
- **決定的な違いは「統計をどこで計算するか」** ─ Vega-Lite は viz ランタイム (JS) 内蔵の
  `transform` で限られた統計 (regression/loess/density 等) を計算する。 hgg は
  **本物の統計ライブラリ hanalyze** が計算し、 結果を spec に流す。 GLMM / GP credible band /
  生存曲線 / ベイズなど、 viz ランタイムには無いモデルがそのまま図になる。
- **Vega-Lite の強みは対話性とエコシステム** (selection / tooltip / pan-zoom、 Altair、 ブラウザ)。
  hgg は現状 **静的出力** (SVG/PNG/WebGL/PDF) で対話性は持たない。

## 同じ図を 2 つの流儀で

散布図 + 線形回帰直線。

### Vega-Lite (JSON spec、 回帰は内蔵 transform)

```json
{
  "data": {"values": [{"x": 1, "y": 2.1}, {"x": 2, "y": 3.9}]},
  "layer": [
    {"mark": "point", "encoding": {
      "x": {"field": "x", "type": "quantitative"},
      "y": {"field": "y", "type": "quantitative"}}},
    {"mark": "line", "transform": [{"regression": "y", "on": "x"}],
     "encoding": {"x": {"field": "x"}, "y": {"field": "y"}}}
  ]
}
```

回帰直線は Vega ランタイム (JS) が `transform.regression` で計算する。

### hgg (VisualSpec、 回帰は hanalyze が計算)

```haskell
-- (a) stat-in: stat も通常 geom と同じ layer。 bridge が hanalyze に fit を委譲 (df 1 回参照)
saveSVGBoundStats "out.svg" $ df |>> (layer (scatter "x" "y") <> layer (statLm "x" "y"))

-- (b) model-out: 先に hanalyze で fit し、 toPlot で図に変換
let fit = lmModel xs ys           -- Hanalyze.Model.* の本物の fit
df |>> (layer (scatter "x" "y") <> toPlot fit)   -- 回帰線 + 信頼帯
```

`lm`/`toPlot` の数値は **hanalyze の `fitLM` が計算** (Vega の JS regression でなく)。
信頼帯 (CI band) も hanalyze の `confidenceBand` 由来。

## 軸 1: DataFrame 連携

| 観点 | Vega-Lite | hgg |
|---|---|---|
| データ供給 | `data.values` (inline JSON) / `data.url` (CSV/JSON/TSV) | `class PlotData` + `(|>>)` バインド (`BoundPlot`) |
| 列参照 | `encoding.x.field: "x"` (文字列) | `scatter "x" "y"` (列名で記述、 `Resolver` 解決) |
| df 直結 | Python は **Altair** が pandas → spec 変換 (別レイヤ) | `instance PlotData DataFrame` で **Hackage `dataframe` を直接バインド** |
| 検証 | spec compile 時 (vega) | バインド時 `validatePlotWith` で **列名検証 → 診断を値同梱** (`bpDiagnostics`) |
| 型安全 | JSON ゆえ実行時 | `PlotData` 型クラス・Haskell の型検査 (列名は文字列だが構造は型) |

要点: Vega-Lite は「JSON にデータを載せる」 か Altair で pandas を変換する。 hgg は
`dataframe` を `PlotData` instance として直接受け、 **バインド時に列名を検証**して診断を
`BoundPlot` の値として運ぶ (例外を投げない純粋な経路)。 → `|>` が Hackage `dataframe` に
占有されているため bind 演算子は **`|>>`**。

## 軸 2: 統計エンジン (hanalyze) 連携 ★

ここが最大の差別化点。

| 観点 | Vega-Lite (`transform`) | hgg |
|---|---|---|
| 回帰 | `regression` (linear/log/exp/pow/quad/poly) | `lm`/`statLm` / `LMModel` `toPlot` (CI band 付き) |
| 多項式回帰 | `regression`(method=poly) | `statPoly` / `SplineSpec` (任意次数) |
| 平滑 | `loess` | `statSmooth` (B-spline) / `SplineModel` `toPlot` |
| 密度 | `density` (KDE) | histogram/density layer (core) |
| 分位 | `quantile` | `QuantileModel` `toPlot` (複数 τ) |
| 集約 | `aggregate`/`bin`/`window` | df 側 + core の `bin2d` 等 |
| **重み付き回帰 (WLS)** | ✗ (回帰に重み指定なし) | **`weighted ws (lm "x" "y")`** (√w スケール・CI も WLS で正しい・weighted R²) |
| **GLM** | ✗ (内蔵 transform に無い) | **`GLMModel` `toPlot`** (μ 曲線 + 非対称 credible band) |
| **GP** | ✗ | **`GPResult` `toPlot`** (平均 + credible band) |
| **生存** | ✗ | **`KMResult`/`CRFit` `toPlot`** (KM 階段 / 競合 CIF) |
| **時系列予測** | ✗ | **`ForecastModel` `toPlot`** (AR 予測区間) |
| **ベイズ (HBM)** | ✗ | **HBM 抽出子族** (`epred`/`traceOf`/`forestOf`/`ppcOf`/`dagOf`/`marginalsOf`)・NUTS 事後 |
| **混合効果** | ✗ | hanalyze `GLMM` (Phase 48: random intercept+slope)・`glmmF` で fit。 `toPlot` 化は今後 |
| **木/多変量** | ✗ | `RandomForest` (重要度 bar) / `PCAResult` (scree) `toPlot` |
| 計算主体 | **Vega ランタイム (JS)** | **hanalyze (本物の統計ライブラリ)** |

2 つの連携経路:

1. **model-out (`toPlot`)**: hanalyze でモデルを fit → `toPlot :: m -> VisualSpec` で図に変換。
   `class Plottable` instance は **14 モデル** (LM/GLM/GP/Spline/GAM/Robust/MultiFit/Quantile/
   MCMCChain/KM/CRFit/Forecast/PCA/RandomForest) に加え、 **WLS (`WeightedLMModel`)・群別フィット
   (`GroupedFit`・N 曲線を群色で重畳)・HBM 抽出子** (`ForestSpec`/`PPCSpec`/`DagSpec` ほか)。 帯の
   統計的意味はモデルごとに正しく区別 (Wald band / GP credible band / AR 予測区間 / ベイズ 94% HDI /
   分位は線自体が区間)。 統一動詞 `df |-> spec` (任意データ源から任意モデル学習) で fit する。
2. **stat-in (`statLm`/`statSmooth`/`statPoly`/`statResid`)**: ggplot 風に
   `layer (scatter "x" "y") <> layer (statLm "x" "y")` (stat も通常 geom と同じ layer・df 1 回)。
   描画は bridge の `saveSVGBoundStats` が `resolveStats` で計算を hanalyze に委譲。 逆エッジ
   (plot→analyze) は bridge package に隔離し core の非依存を保つ (core は純タグ `MStatLM` 等のみ)。
   ★さらに **geom_smooth 風オプション**を完備: `statColor`/`statFill`/`statLinetype`/`statLinewidth`/
   `statAlpha` (見た目)、 `statLabel` (凡例)、 `statEquation`/`statR2` (式・決定係数の注釈)、
   `statLevel` + `interval CI|PI` (信頼区間 / 予測区間の切替)。 = Vega-Lite の `regression` には無い
   不確実性・群色分け・式注釈まで宣言的に指定できる。

★ Vega-Lite の `regression` は曲線の **当てはめ式** を JS で計算するだけで、 信頼区間・予測区間・
標準誤差・モデル診断・重み付けは持たない。 hgg は hanalyze の
`confidenceBand`/`predictionBandAt`/`predictGlmMuWithCI` 等が返す**統計的に正しい不確実性**を帯として
描き、 CI/PI の切替・群別重畳・式/R² 注釈・WLS まで宣言的に組める。

## 軸 3: 基本プロットのカバレッジ

凡例: ✓ = ネイティブ対応 / △ = transform・workaround で可能 / ✗ = 実用的でない。
hgg 側は実装の `MarkKind` (`hgg-core` Spec.hs、 47 種) を根拠とする。

| 基本プロット | Vega-Lite | hgg (`MarkKind`) |
|---|---|---|
| 散光図 | ✓ `point` | ✓ `MScatter` |
| 折れ線 | ✓ `line` | ✓ `MLine` |
| 棒 (群/積/100%) | ✓ `bar` | ✓ `MBar` (dodge/stack/fill = position adjustment) |
| 面 / 帯 | ✓ `area` | ✓ `MBand` |
| ヒストグラム | ✓ `bar`+`bin` | ✓ `MHistogram` |
| 円 / ドーナツ | ✓ `arc` | ✓ `MPie` |
| 階段 | ✓ `line(step)` | ✓ `MStep` |
| ヒートマップ | ✓ `rect` | ✓ `MHeatmap` |
| 2D ビン (geom_bin2d) | ✓ `rect`+`bin` | ✓ `MBin2d` |
| テキスト / ラベル | ✓ `text` | ✓ `MText` / `MLabel` |
| 箱ひげ | ✓ `boxplot` (合成) | ✓ `MBox` |
| エラーバー / 区間 | ✓ `errorbar`/`errorband` | ✓ `MLineRange` / `MPointRange` / `MCrossbar` |
| ストリップ (tick) | ✓ `tick` | ✓ `MStrip` |
| 密度 (KDE) | △ `density` transform | ✓ `MDensity` |
| ECDF | △ `window` transform | ✓ `MEcdf` |
| 棒付き棒 (stem) | △ (`rule`+`point`) | ✓ `MStem` |
| QQ プロット | ✗ | ✓ `MQQ` |
| 等高線 (contour) | ✗ (Vega なら可) | ✓ `MContour` (marching squares) |

要点: **基本プロットはほぼ同等にカバー**。 Vega-Lite は `boxplot`/`errorbar` を合成マークで
持ち、 density/ECDF は transform 経由。 hgg は QQ・contour・stem を **ネイティブマーク**
として持つ (Vega-Lite では workaround か不可)。

## 軸 4: 応用プロットのカバレッジ

| 応用プロット | Vega-Lite | hgg (`MarkKind`) |
|---|---|---|
| バイオリン | △ (`density`+`area` の手組み) | ✓ `MViolin` |
| リッジライン (joyplot) | △ (facet + 手組み) | ✓ `MRidge` |
| レインクラウド | ✗ | ✓ `MRaincloud` |
| スウォーム (beeswarm) | ✗ | ✓ `MSwarm` |
| 平行座標 | △ (`fold`+`repeat`) | ✓ `MParallel` |
| ウォーターフォール | △ (`window` 手組み) | ✓ `MWaterfall` |
| 散布図行列 (SPLOM/pairs) | △ (`repeat`) | ✓ `pairs` |
| 回帰 + 信頼帯 | △ (`regression` transform、 **CI 無し**) | ✓ `MRegression` (hanalyze の CI band) |
| フォレストプロット | ✗ | ✓ `MForest` |
| ファンネルプロット | ✗ | ✓ `MFunnel` |
| 自己相関 (ACF) | ✗ | ✓ `MAutocorr` |
| MCMC trace / density | ✗ | ✓ `MTrace` / `MCMC` (ベイズ診断) |
| ESS (有効サンプルサイズ) | ✗ | ✓ `MEss` |
| モデル DAG (グラフ) | ✗ | ✓ `MDAG` (HBM 構造) |
| ウェハマップ | ✗ | ✓ `MWaferMap` (半導体) |
| 3D 散布 | ✗ | ✓ `MScatter3D` (`hgg-3d`、 CPU 投影) |
| カスタム図形 (トランプ等) | ✗ (図形は限定) | ✓ `MShClub`/`MShSpade`/`MShHeart`/… |

要点: **応用・統計・ドメイン特化プロットは hgg が大きく優る**。 Vega-Lite は
高レベル文法ゆえバイオリン/平行座標/ウォーターフォールは transform の手組みが要り、 フォレスト・
ACF・MCMC 診断・DAG・ウェハマップ・3D はそもそも守備範囲外。 hgg はこれらを
**統計/工学ワークフローの定番**としてマークに持つ (とくにベイズ診断・DOE・半導体)。

## 軸 5: 文法 (grammar) の比較

| 文法要素 | Vega-Lite | hgg |
|---|---|---|
| spec の形 | JSON object | `VisualSpec` (Haskell ADT、 値) |
| レイヤ合成 | `"layer": [...]` (配列) | `<>` (Monoid) |
| エンコーディング | チャネル record `encoding: {x, y, color, size, shape, opacity, text, tooltip, order, detail, …}` | コンビネータ関数 `scatter "x" "y"` + `colorBy`/`sizeBy`/… |
| データ型注釈 | `type: quantitative/nominal/ordinal/temporal` 明示 | 列の値から推論 (数値/カテゴリ)、 `temporal` 専用型は限定 |
| 集約 | encoding 内に inline (`aggregate: "mean"`, `bin`, `timeUnit`) | stat マーク (`MStatMean`/`MStatMedian`) or df 前処理 |
| 変換 | `transform: [...]` (filter/calculate/aggregate/window/fold/pivot/regression/loess/density/quantile) | df 側前処理 + stat マーク + hanalyze 連携 (`statLm`/`statSmooth`/`toPlot`) |
| 多パネル合成 | `facet` / `repeat` / `concat` / `hconcat` / `vconcat` (汎用) | `facet` (free/fixed scale) + **`hconcat`/`vconcat` + 演算子 `<->`/`<:>` (= concat 相当・同方向チェーンを平坦化)** + `subplots`/`subplotCols` (任意グリッド) + 入れ子 subplots (列の中にセル) + **`repeatFields` (フィールド反復 = repeat 相当・明示形)** |
| スケール / 軸 / 凡例 | `scale`/`axis`/`legend` + `resolve` | scale + `coord_flip` + `scale_*_reverse` + polar (coord) |
| 条件付きエンコード | `condition` (selection 連動) | 無し (静的) |
| 対話 | `params` / `selection` (点選択・範囲・pan-zoom) | 無し |
| テーマ | `config` | Theme (明暗 + 学術 palette + ggplot preset + element 単位 override) |
| 合成の代数 | JSON ネスト (代数則は暗黙) | **Monoid 則** (`<>` 結合律・単位元) を型で保証 |

文法の思想差:
- **多パネル合成**: かつては hgg は `facet` のみで concat/repeat に劣ったが、 `subplots`/
  `subplotCols` (異種 spec を任意グリッドに・**入れ子可**) と、 その薄ラッパ **`hconcat`/`vconcat` +
  中置演算子 `<->`(横)/`<:>`(縦)** で **concat/hconcat/vconcat 相当を獲得**し差は大きく縮んだ。 演算子は
  同方向チェーンを平坦化するので `(a <-> b <-> c) <:> d` で「1 行目 3 列 + 2 行目全幅」 のような非対称
  レイアウトが 1 行で書ける (例: HBM 診断ダッシュボードは「列の中にセル (forest/PPC/DAG)」 の二重入れ子)。
  `repeat` 相当も **`repeatFields`** (フィールドリスト→各 view を `subplots` に展開する明示形) で獲得し、
  多パネル合成のギャップはほぼ解消した (差は Vega の encoding 内 `{repeat: …}` 糖衣を持たない点のみ)。
- **エンコーディングの与え方**: Vega-Lite は全チャネルを **一様な record** で書く (発見性が高い)。
  hgg は **コンビネータ + Monoid** (Haskell らしく合成的・型安全だが、 利用可能チャネルは
  関数として個別)。
- **変換の所在**: Vega-Lite は集約・回帰・KDE を **spec 内 transform** で完結 (自己完結・再現性高)。
  hgg は単純整形は df 側、 統計は **hanalyze に委譲** (本物の統計エンジン。 軸 2 参照)。
- **対話の文法**: Vega-Lite は `selection`/`params` を文法の第一級市民として持つ (これが最大の強み)。
  hgg は静的出力で対話文法を持たない。
- **代数的保証**: hgg の `<>` は Monoid 則を型で持つため、 部分図を安全に組み替え・再利用
  できる (JSON のネストにはこの保証が無い)。

## 概念対応表

| やりたいこと | Vega-Lite | hgg |
|---|---|---|
| 図の spec | JSON object | `VisualSpec` |
| レイヤ重畳 | `"layer": [...]` | `<>` (Monoid) |
| マーク | `"mark": "point"` | `scatter` / `line` / `bar` / … (`MarkKind`) |
| エンコーディング | `"encoding": {"x": …}` | `scatter "x" "y"` 等の列名引数 |
| ファセット | `"facet"`/`"repeat"` | `facet` (free/fixed scale) / `repeatFields` (repeat 相当) |
| 異種パネル合成 | `"concat"`/`"hconcat"`/`"vconcat"` | `hconcat [..]` / `vconcat [..]` / 演算子 `a <-> b` (横) / `a <:> b` (縦) / `subplots [..] <> subplotCols n` (入れ子可) |
| スケール | `"scale": {…}` | scale / `coord_flip` / `scale_*_reverse` |
| テーマ | config / theme | Theme (明暗 + 学術 palette + ggplot preset) |
| 回帰 | `transform: regression` | `lm` / `toPlot (lmModel …)` |
| データ | `data.values`/`url` | `df |>> …` (`PlotData`) |
| 出力 | SVG / Canvas / PNG (vega) | SVG / WebGL / Canvas(PS) / PDF(計画) / PNG(Rasterific) |
| 対話 | selection / tooltip / zoom | (なし・静的) |

## どこが強み / どこが未整備か

### hgg の強み
- **本物の統計エンジン連携**: GLM/GP/生存/予測/混合効果など、 viz ランタイムの内蔵 transform に
  無いモデルがそのまま図になる。 不確実性 (CI/credible band/予測区間) が統計的に正しい。
- **型と純粋性**: `VisualSpec` は Haskell の値・Monoid。 バインド検証が値として伝播 (例外なし)。
- **df を型クラスで直結**: `dataframe` を `PlotData` instance で受ける (中間 JSON 変換が不要)。
- **複数バックエンド**: SVG/WebGL/Canvas/PDF/PNG を 1 spec から。 HS/PS の parity を golden test で担保。

### Vega-Lite が優る点 — 2 種に分けて扱う

実際に [Vega-Lite ギャラリー](https://vega.github.io/vega-lite/examples/) を走査して描けない例を
分類した (再現コードと一覧は [vega-lite-gallery.md](vega-lite-gallery.ja.md))。

**A. 仕様として載せない (意図的に対象外・実装予定なし)** — 利用者が使わないため spec に持たない:
- **地理 / 地図** (geoshape / projection): Maps セクション全部。
- **対話** (selection / hover / brush / pan-zoom / crossfilter / widgets): Interactive セクション全部。
- **画像 / アイソタイプ** (image mark): Image-based Scatter / Isotype。

**B. 今後の課題 (真に描けない・実装余地あり)** — A を除いて現状ネイティブに無いもの:
- **多パネル合成は対応済**: `concat`/`hconcat`/`vconcat` 相当は `subplots` (異種 spec の任意グリッド +
  入れ子)、 `repeat` 相当は **`repeatFields`** (フィールドリストを反復し各 view を `subplots` に展開)。
  残差は Vega の encoding 内 `{repeat: …}` 糖衣 (フィールドを encoding に直接差し込む形) のみ。
- **Streamgraph は対応済** (Phase 52.D2): `stream x y <> color "series"` で中心化 (silhouette、
  baseline=-Σy/2) 積層 area を描く。 wiggle 最小化 (ThemeRiver) は未対応。
- **Horizon Graph / Trail (可変幅線) / Mosaic / Ternary / Candlestick / Bullet**: 専用マークが無い。
- **宣言的 transform の網羅** (`fold`/`pivot`/`window` 等): hgg は df 側で前処理する想定。
- **混合効果の `toPlot`**: Phase 52.D3 で **対応済** — `GLMMResultRE` (`glmmF` で fit) の `Plottable`
  instance が random effect の **caterpillar plot** (BLUP を群ごと昇順ソート) を描く。 ★CI 帯は現状
  なし (点のみ): conditional variance / `n_j` 未格納のため (将来格納で帯化可能)。
- **エコシステム** (Altair/ブラウザ等の周辺) も Vega-Lite が優るが、 これは設計思想の違い。

## まとめ

Vega-Lite と hgg は **「宣言的 spec を組んでレンダラに渡す」 という設計思想を共有**する
最も近い類縁。 5 軸の評価をまとめると:

- **基本プロット (軸 3)**: ほぼ同等。 hgg は QQ・contour・stem をネイティブに持つ。
- **応用プロット (軸 4)**: **hgg が大きく優る** (バイオリン・平行座標・フォレスト・ACF・
  MCMC 診断・DAG・ウェハマップ・3D)。 Vega-Lite は高レベル文法ゆえ手組みか守備範囲外。
- **文法 (軸 5)**: Vega-Lite は **対話 (`selection`)** と **inline transform** が第一級で強い。
  hgg は **Monoid 合成の代数的保証**と **型安全**が強い。 多パネル合成は `subplots` (入れ子可)
  で concat/hconcat/vconcat 相当を獲得し差が縮んだ (残差は `repeat` の自動反復のみ)。
- **DataFrame 連携 (軸 1)**: hgg は `dataframe` を型クラスで直結 + バインド時検証。
- **統計連携 (軸 2)**: hgg は hanalyze (本物の統計エンジン) が計算 ⇔ Vega-Lite は JS transform。

棲み分けは明快:

- **対話的な探索 / Web 配信 / pandas エコシステム** → Vega-Lite (Altair)。
- **本格的な統計モデル・応用/工学プロットの可視化 / 型安全 / Haskell データパイプライン / 静的高品質出力** → hgg。

とくに hanalyze 連携 (`toPlot` 14 モデル + stat-in bridge) により、 **「fit したモデルを
そのまま統計的に正しい不確実性つきで描く」** ワークフローは hgg に固有の強み。
