# 06. スクリプトとプロジェクト (R4DS 2e Ch.6 "Workflow: scripts and projects")

> 🌐 [English](README.md) | **日本語**

> 一次情報: **R for Data Science 2e, Ch.6 "Workflow: scripts and projects"**
> <https://r4ds.hadley.nz/workflow-scripts>
> データ: ggplot2 の **diamonds**(全量 53,940 行)。

R4DS 第 6 章は **図を描かない** 運用章です(本文の R 例はすべて `eval: false` の見本、
図は RStudio のスクリーンショットのみ)。 主題は次の 2 つ:

1. **スクリプト (script)**: コードをコンソール直打ちでなくファイルにまとめ、 編集し、 実行する。
2. **プロジェクト (project)**: 解析に関わるファイル一式(入力データ・スクリプト・結果・図)を
   1 つのディレクトリにまとめる。

どちらも **RStudio 固有の運用** なので、 ここでは近似や置換ではなく
「**R/RStudio の概念 → Haskell(cabal / GHCi / cabal project)の等価運用**」という
honest な対応づけで示します。 実行コードは [`WorkflowScripts.hs`](WorkflowScripts.hs)
(等価ワークフローを実際に動かして確認)。

```sh
cd docs/tutorials/06-workflow-scripts
cabal run tut-06-workflow-scripts
```

★このファイル群自体が R4DS のいう「スクリプト」と「命名 3 原則」の見本です
(先頭に import をまとめ、 セクションを `-- ===` 罫線で区切り、 連番・kebab-case・
中身を表す名前を付けています)。

---

## §6.1 スクリプト(Scripts)

R: コンソールに直打ちする代わりにスクリプトエディタにコードを書き、 編集して再実行する。
保存すれば後から戻れる。 Haskell の等価運用:

| R / RStudio | Haskell の等価運用 |
|---|---|
| スクリプトエディタにコードを書く | `.hs` ファイルにコードを書く(この `WorkflowScripts.hs`) |
| 1 式ずつ実行(Cmd/Ctrl + Enter) | GHCi で `:load WorkflowScripts.hs` → 式を 1 つずつ評価 |
| スクリプト全体を実行(Cmd/Ctrl + Shift + S) | `cabal run tut-06-workflow-scripts`(= このファイルを通し実行) |
| 即席の 1 ファイル実行 | `runghc WorkflowScripts.hs` |

R4DS の推奨「スクリプトは必要なパッケージから始める」は Haskell でも同じで、
**先頭に `import` をまとめる** と相手がどの依存が要るか一目で分かります。 ただし
**共有スクリプトに install 系(R の `install.packages()` / `cabal install`)は書かない**
— 相手の環境を勝手に変えてしまうからです。 Haskell では依存は `.cabal` の
`build-depends` で宣言し、 `import` はその利用宣言にあたります。

### §6.1.1 コードの実行(Running code)

R4DS の例(欠損を除いてから集計する典型パイプ)を、 本章のデータ diamonds で同型に示します。

```haskell
-- R: not_cancelled <- flights |> filter(!is.na(dep_delay), !is.na(arr_delay))
--    not_cancelled |> group_by(...) |> summarize(mean = mean(dep_delay))
smallIdeal =
  diamonds
    |> DF.filterWhere (F.col @Double "carat" .< (1.0 :: DF.Expr Double))
    |> DF.filterWhere (F.col @Text   "cut"   .== F.lit ("Ideal" :: Text))
```

`filter |> … |> summarize` の流れは dataframe でも同じです(1 行 1 動詞・`|>` 行末)。
実行すると carat < 1 かつ Ideal の 15,681 行・平均価格 `1546.21` が出ます
(`../_data/_raw/diamonds.csv` を `awk` で突合済の実測値)。

### §6.1.2 診断(Diagnostics)

R: RStudio はエディタ上で構文エラー(赤波線)や潜在的問題(黄!)をその場で指摘する。

Haskell の等価は **GHC の型検査 + コンパイラ警告**、 エディタ上では **HLS
(haskell-language-server)** が赤波線・hover でメッセージを出します。 R は実行して
初めて気づく誤りも、 Haskell ではより早い段階(コンパイル時)に多くが捕まります。

### §6.1.3 保存と命名(Saving and naming)

R4DS の命名 3 原則は、 そのまま Haskell のファイルにも当てはまります:

1. **機械可読**: 空白・記号・特殊文字を避ける。 大文字小文字だけの区別に頼らない。
2. **人間可読**: 中身が分かる名前を付ける。
3. **既定の並び順と相性良く**: 連番で始め、 アルファベット順 = 実行順 にする。

R4DS の「悪い例 → 良い例」 と同じ整理:

```
# 避ける                          # こう付ける
alternative model.R               01-load-data.R
code for exploratory analysis.r   02-exploratory-analysis.R
finalreport.qmd / FinalReport.qmd 03-model-approach-1.R
run-first.r / temp.txt            04-model-approach-2.R
```

★**このチュートリアル群のディレクトリ名**(`01-visualize` / `02-workflow-basics` /
… / `06-workflow-scripts`)がまさにこの 3 原則の実例です(連番・kebab-case・中身を表す)。

---

## §6.2 プロジェクト(Projects)

R: 解析に関わるファイル一式を 1 ディレクトリにまとめ、 RStudio はそれを project
(`.Rproj`)として支援する。 Haskell の等価:

| R / RStudio | Haskell の等価運用 |
|---|---|
| RStudio project(`.Rproj`) | cabal の **package**(`.cabal`)/ **project**(`cabal.project`) |
| project ディレクトリ = 解析の home | この `hgg/` リポジトリ(`../../cabal.project` が全 package を束ねる) |
| `File > New Project` | `cabal init` / `.cabal` を書く |

### §6.2.1 真実の源(source of truth)

R: 真実の源は環境(Environment)ではなく **R スクリプト**。 スクリプト + データから
環境は再生成できるが、 逆は難しい。 RStudio はセッション間でワークスペースを保存しない
設定(clean slate)を推奨。

Haskell の等価: 真実の源は GHCi の REPL 状態ではなく **`.hs` ソース**。 GHCi を
再起動(`:reload` / 終了して `cabal repl`)しても、 ソースから常に同じ結果を
再生成できます。 **REPL に積み上げた束縛に依存しない** — これが R の clean slate と
同じ規律です。 R の「R 再起動 → スクリプト再実行」のショートカット運用は、 Haskell では
「`cabal repl` 再起動 → `:reload`」 あるいは「`cabal run` で通し実行」 に対応します。

### §6.2.2 作業ディレクトリ(working directory)

| R | Haskell |
|---|---|
| `getwd()` | `System.Directory.getCurrentDirectory` |
| `setwd("/path")`(**非推奨**) | `System.Directory.setCurrentDirectory`(同様に**非推奨**) |

R も Haskell も、 作業ディレクトリを **コード中で `setwd` / `setCurrentDirectory`
して固定するのは非推奨**(スクリプトが場所依存になり共有を妨げる)。 代わりに project の
home を作業ディレクトリにして、 以降は相対パスで書きます。

### §6.2.3 相対パスで保存する(RStudio projects)

R4DS の toy 例(`diamonds.R`, `eval: false`)の眼目は **図と CSV をコードで
(マウス/クリップボードでなく)相対パスに保存する** ことです:

```r
# R (R4DS の見本)
ggplot(diamonds, aes(x = carat, y = price)) + geom_hex()
ggsave("diamonds.png")
write_csv(diamonds, "data/diamonds.csv")
```

Haskell での等価ワークフロー:

```haskell
let dPlot = diamonds |>> theme ThemeGrey <> layer (scatter "carat" "price" <> alpha 0.05)
saveSVGBound "diamonds-carat-price.svg" dPlot     -- = ggsave("diamonds.png")
createDirectoryIfMissing False "data"             -- = Files ペインの New Folder
DF.writeCsv "data/diamonds.csv" diamonds          -- = write_csv(diamonds, "data/diamonds.csv")
```

実行すると、 作業ディレクトリ(= この章ディレクトリ)に `diamonds-carat-price.svg` と
`data/diamonds.csv` が生成されます。 R4DS の教えどおり、 **図はマウスでなくコードで保存**
すれば、 後から「この図はどのコードが作ったか」 を必ず辿れます。

> ★相違(honest に記録):
> R の **`geom_hex`**(六角ビニングで過密散布を要約)は hgg に **未実装** です。
> 矩形ビンの `geom_bin2d` 相当(`bin2d`)はありますが、 これは **各セルの z 平均を色にする**
> もので、 **件数密度**を塗る `geom_hex` とは別物です(六角ビンも未実装)。 本章の眼目は
> 六角ビン自体ではなく「コードで相対パスに保存する」 ワークフローなので、 図は実装済の
> `scatter`(過密は `alpha` で緩和)で代替しました。 六角/件数ビンの実装と本格再現は
> 過密データを扱う EDA / Layers 章に回します。
>
> ※生成物(`diamonds-carat-price.svg`・`data/`)は実行のたびに再生成されるため
> [`.gitignore`](.gitignore) で git 管理から外しています(図なし運用章)。

### §6.2.4 相対パスと絶対パス(Relative and absolute paths)

R: project の中では **常に相対パス** を使う(絶対パスは共有を妨げる)。 区切りは
Mac/Linux のスラッシュ `/` を使う(Windows のバックスラッシュは避ける)。

Haskell の等価: `System.FilePath` は `/` 区切りで全 OS 可搬です。 上で書いた
`"data/diamonds.csv"` のように **相対・スラッシュ区切り** で書けば、 project を
どこに置いても動きます。 絶対パス(`/home/...` や `C:\...`)は self の環境にしか
通じないので、 共有するコードには **絶対に書かない** のが鉄則です。

---

## できないこと / 近似せず記録した相違

- **△ `geom_hex`(六角ビニング)未実装**: §6.2.3 の toy 例の図マークは、 件数密度を塗る
  `geom_hex` が未実装のため `scatter`(+ `alpha`)で代替しました(`bin2d` はセル平均 z 用で
  別物)。 本章の眼目は保存ワークフローなので図は代替で問題なし。 六角/件数ビンの実装は
  EDA / Layers 章へ。
- **R/RStudio 固有の UI**: スクリプトエディタの 4 ペイン・診断の赤波線・`.Rproj` の
  「New Project」 ウィザード等の **スクリーンショットは R4DS 固有** で、 本章では
  対応する Haskell の運用(GHCi / HLS / cabal project)を表で示すにとどめます
  (R4DS でも本文に出る図はこれらのスクショだけ = 解析図はゼロ)。
- **`writeCsv` の欠損列制限**: この版の `dataframe` の `writeCsv` は欠損(`Nothing`)を
  含む列を直列化できません(Ch4 import 章に既記)。 diamonds は欠損なしのため §6.2.3 は
  そのまま書けています。
