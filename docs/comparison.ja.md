# matplotlib / ggplot2 との比較

> 🌐 [English](comparison.ja.md) | **日本語**

> 宣言的 spec の類縁である **Vega-Lite** との比較 (とくに DataFrame 連携・hanalyze 統計
> エンジン連携の軸) は [comparison-vega-lite.ja.md](comparison-vega-lite.ja.md) を参照。

## ひとことで

- **思想は ggplot2 寄り** ─ 「データ + aesthetic mapping + geom + scale」 の文法 (grammar of graphics)。
  `<>` での合成は ggplot2 の `+` に対応する。
- **matplotlib との違いは「状態を持たない」 こと** ─ matplotlib pyplot のような暗黙の
  「アクティブな figure」 は無い。 図は純粋値 (`VisualSpec`) で、 副作用は最後の保存だけ。
- **カバー範囲** ─ matplotlib 全機能の再現を狙うものではなく、 **統計プロットの定番ワークフロー**
  (scatter/line/bar/dist/regression/facet/3D 等) のカバーを目標とする。

---

## 同じ図を 3 つの流儀で

「群で色分けした散布図 + 軸ラベル + タイトル」 を書く。

### matplotlib (命令型・状態機械)

```python
import matplotlib.pyplot as plt
for g, sub in df.groupby("group"):
    plt.scatter(sub.x, sub.y, label=g)
plt.legend(); plt.title("by group")
plt.xlabel("x"); plt.ylabel("y")
plt.savefig("out.svg")
```

### ggplot2 (宣言型・文法)

```r
ggplot(df, aes(x, y, color=group)) +
  geom_point(size=6) +
  scale_color_manual(values=c(alpha="#1B9E77", beta="#D95F02")) +
  labs(title="by group", x="x", y="y")
```

### hgg (宣言型・純関数)

```haskell
purePlot
  <> layer (scatter (inline xs) (inline ys) <> color (inlineCat gs) <> size 6)
  <> scaleColorManual [("alpha","#1B9E77"), ("beta","#D95F02")]
  <> legend
  <> title "by group" <> xLabel "x" <> yLabel "y"
```

→ `cabal run tutorial-02-grammar`

---

## 概念対応表

| やりたいこと | matplotlib | ggplot2 | hgg |
|---|---|---|---|
| 図の土台 | `plt.figure()` (暗黙) | `ggplot(d, aes())` | `purePlot` |
| 散布図 | `plt.scatter` | `geom_point` | `scatter` / `points` |
| 折れ線 | `plt.plot` | `geom_line` | `line` / `lineXY` |
| 棒 | `plt.bar` | `geom_col` | `bar` / `bars` |
| ヒストグラム | `plt.hist` | `geom_histogram` | `histogram` / `hist` |
| 群で色分け | `c=`, ループ | `aes(color=g)` | `<> color (inlineCat gs)` |
| 重畳 | 連続 `plt.*` 呼び出し | `+ geom_*()` | `<> layer (...)` |
| 色 scale | `cmap=` | `scale_color_*` | `scaleColorManual` / `scaleColorGradient2` |
| 小分割 | `plt.subplots` | `facet_wrap` / `facet_grid` | `facet*` |
| テーマ | `plt.style.use` | `theme_*` | `theme Theme*` |
| 軸ラベル | `plt.xlabel` | `labs(x=)` | `xLabel` |
| 座標反転/極座標 | 個別 API | `coord_flip` / `coord_polar` | `coordFlip` / `coordPolar` |
| 保存 | `plt.savefig` | `ggsave` | `saveSVG` |
| 3D | `mplot3d` | (限定) | `hgg-3d` + `showBrowser` |

---

## どこが強み / どこが未整備か

### 強み

- **純粋・合成可能** ─ 部分 spec (テーマだけ、 軸設定だけ) を値として使い回せる。 テストしやすい。
- **HS / PS 同一 ADT** ─ backend (Haskell) と frontend (PureScript) が同じ spec を共有し、
  JSON で round-trip。 サーバ生成図とブラウザ対話図が一致する。
- **3D が browser interactive** ─ WebGL2 で orbit/zoom/pan。 mplot3d の静的投影を超える。
- **統計プロットの幅** ─ violin / raincloud / ridge / trace / ESS / forest / DAG など、
  matplotlib では追加ライブラリが要る図を core で持つ。

### 未整備 (Planned / Experimental)

- PDF / PNG backend は placeholder (SVG / Canvas / WebGL が実用)。
- sqrt / time 軸は spec 定義済だが Layout 完全対応は wip。
- matplotlib のような微細な低レベル artist 操作は Layer 4 (`Primitive`) 直書きが必要。

> backend / chart 別の実装状況の単一情報源は `design/parity-table.md`。
