-- | チュートリアル 06: スクリプトとプロジェクト
--   (R4DS 2e Ch6 "Workflow: scripts and projects")
--   https://r4ds.hadley.nz/workflow-scripts
--
--   R4DS 第 6 章は **図を描かない** 運用章 (本文の R 例は eval=false の見本、 図は
--   RStudio のスクリーンショットのみ)。 主題は次の 2 つ:
--     ・スクリプト (script): コードをファイルにまとめ、 編集し、 まとめて実行する
--     ・プロジェクト (project): 解析に関わるファイル一式を 1 ディレクトリにまとめる
--   いずれも RStudio 固有の運用なので、 ここでは **R/RStudio の概念 → Haskell
--   (cabal / GHCi / cabal project) の等価運用** に honest に対応づける (近似ではなく
--   「R の概念 → Haskell の等価」 の写像)。
--
--   このファイル自体が R4DS のいう「スクリプト」の見本: 先頭に import をまとめ、
--   セクションは `-- ===` 罫線で区切り、 機械可読・人間可読・連番で名前を付ける。
--   ・データは ggplot2 の diamonds 全量 (R4DS と同じ)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

-- ★R4DS §6.1: スクリプトは「必要なパッケージ」から始める。 そうすれば共有相手が
--   どの依存が要るか一目で分かる。 ただし共有スクリプトに install 系
--   (R の install.packages / cabal install) は **絶対に書かない** (相手の環境を
--   勝手に変えてしまう)。 Haskell では依存は .cabal の build-depends で宣言し、
--   import はその利用宣言にあたる。
import           Data.Text              (Text)
import qualified DataFrame              as DF
import qualified DataFrame.Functions    as F
import           DataFrame.Operators    ((|>), (.<), (.==))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame     ((|>>))
import           Hgg.Plot.Backend.SVG (saveSVGBound)
import           Hgg.Plot.DataFrame ()
import           System.Directory       (getCurrentDirectory, createDirectoryIfMissing)

main :: IO ()
main = do

  -- =========================================================================
  -- §6.1 Scripts — スクリプト
  -- =========================================================================
  -- R: コンソールに直接打つ代わりに、 スクリプトエディタにコードを書き、 編集し、
  --    まとめて実行する。 1 式ずつ実行 (Cmd/Ctrl+Enter)、 全体実行 (Cmd/Ctrl+Shift+S)。
  -- Haskell の等価運用:
  --    ・1 式ずつ      = GHCi で `:load WorkflowScripts.hs` → 式を 1 つずつ評価
  --    ・全体実行      = `cabal run tut-06-workflow-scripts` (= このファイルを通し実行)
  --    ・即席 1 ファイル = `runghc WorkflowScripts.hs`
  -- このプログラムは「全体実行」 にあたる (= R の Cmd/Ctrl+Shift+S)。
  putStrLn "# §6.1 Scripts: このファイルが R4DS のいう script (= cabal run で通し実行)"

  -- R4DS §6.1.1 の例: パッケージ読み込み → 欠損を除く → 集計、 という典型パイプ。
  --   not_cancelled <- flights |> filter(!is.na(dep_delay), !is.na(arr_delay))
  --   not_cancelled |> group_by(...) |> summarize(mean = mean(dep_delay))
  -- ここでは本章のデータ diamonds で同型のパイプを示す (理想カット・1 カラット未満の平均価格)。
  diamonds <- DF.readCsv "../_data/_raw/diamonds.csv"
  let smallIdeal =
        diamonds
          |> DF.filterWhere (F.col @Double "carat" .< (1.0 :: DF.Expr Double))
          |> DF.filterWhere (F.col @Text "cut" .== F.lit ("Ideal" :: Text))
  let prices    = DF.columnAsList (F.col @Int "price") smallIdeal :: [Int]
      meanPrice = fromIntegral (sum prices) / fromIntegral (length prices) :: Double
  putStrLn "\n# §6.1.1 Running code: filter |> summarize 型のパイプ (carat<1 かつ Ideal)"
  print (DF.dimensions smallIdeal)
  putStrLn ("  mean_price = " ++ show meanPrice ++ ", n = " ++ show (length prices))

  -- -------------------------------------------------------------------------
  -- §6.1.2 Diagnostics — 診断
  -- -------------------------------------------------------------------------
  -- R: RStudio はエディタ上で構文エラー (赤波線) や潜在的問題 (黄!) をその場で指摘する。
  -- Haskell の等価: GHC の型検査 + コンパイラ警告。 エディタ上では HLS (haskell-language-server)
  --   が赤波線 / hover でメッセージを出す。 R の「実行して初めて気づく」より早い段階
  --   (コンパイル時) に多くの誤りが捕まる。 (実行コードなし)
  putStrLn "\n# §6.1.2 Diagnostics: R=RStudio の波線 → Haskell=GHC 型検査 + HLS の波線"

  -- -------------------------------------------------------------------------
  -- §6.1.3 Saving and naming — 保存と命名
  -- -------------------------------------------------------------------------
  -- R4DS の命名 3 原則 (そのまま Haskell のファイルにも当てはまる):
  --   1. 機械可読: 空白・記号・特殊文字を避ける。 大文字小文字だけの区別に頼らない。
  --   2. 人間可読: 中身が分かる名前を付ける。
  --   3. 既定の並び順と相性良く: 連番で始めて、 アルファベット順 = 実行順 にする。
  -- ★このチュートリアル群のディレクトリ名 (01-visualize, 02-workflow-basics, …,
  --   06-workflow-scripts) がまさにこの 3 原則の実例 (連番・kebab-case・中身を表す)。
  putStrLn "\n# §6.1.3 Naming: 01-visualize .. 06-workflow-scripts が命名 3 原則の実例"

  -- =========================================================================
  -- §6.2 Projects — プロジェクト
  -- =========================================================================
  -- R: 解析に関わるファイル一式 (入力データ・スクリプト・結果・図) を 1 ディレクトリに
  --    まとめる。 RStudio はこれを「project」 (.Rproj) として支援する。
  -- Haskell の等価: cabal の **package** (.cabal) / **project** (cabal.project)。
  --    この hgg リポジトリ自体が 1 つの cabal project で、 ../../cabal.project が
  --    全 package を束ねる。

  -- -------------------------------------------------------------------------
  -- §6.2.1 source of truth — 真実の源
  -- -------------------------------------------------------------------------
  -- R: 真実の源は環境 (Environment) ではなく **R スクリプト**。 スクリプト + データから
  --    環境を再生成できるが、 逆は難しい。 RStudio はセッション間でワークスペースを
  --    保存しない設定 (clean slate) を推奨。
  -- Haskell の等価: 真実の源は GHCi の REPL 状態ではなく **.hs ソース**。 GHCi を再起動
  --    (`:reload` / 終了→再 `cabal repl`) しても、 ソースから常に同じ結果を再生成できる。
  --    REPL に積み上げた束縛に依存しない = R の clean slate と同じ規律。
  putStrLn "\n# §6.2.1 source of truth: REPL 状態でなく .hs ソースが真実の源 (再現可能)"

  -- -------------------------------------------------------------------------
  -- §6.2.2 working directory — 作業ディレクトリ
  -- -------------------------------------------------------------------------
  -- R: getwd() で現在の作業ディレクトリを表示。 setwd() で変更できるが **非推奨**。
  -- Haskell の等価: System.Directory.getCurrentDirectory (= getwd())。
  --    setCurrentDirectory (= setwd()) は同様に非推奨 (スクリプトを場所依存にする)。
  cwd <- getCurrentDirectory
  putStrLn "\n# §6.2.2 working directory: getCurrentDirectory (= R の getwd())"
  putStrLn ("  cwd = " ++ cwd)

  -- -------------------------------------------------------------------------
  -- §6.2.3 RStudio projects + relative paths — 相対パスで保存する
  -- -------------------------------------------------------------------------
  -- R4DS の toy 例 (diamonds.R, eval=false) の眼目:
  --   ・図と CSV を **コードで** (マウス/クリップボードでなく) ファイルに保存する
  --   ・パスは **相対パス** (data/diamonds.csv) で書く → どこに project を置いても動く
  --     ggplot(diamonds, aes(carat, price)) + geom_hex(); ggsave("diamonds.png")
  --     write_csv(diamonds, "data/diamonds.csv")
  --
  -- ★相違 (honest): geom_hex (六角ビニングによる過密散布の要約) は hgg に
  --   **未実装** (矩形ビンの geom_bin2d 相当 `bin2d` はあるが、 これはセル平均 z を
  --   色にするもので、 件数密度の geom_hex とは別物)。 本章の眼目は六角ビン自体では
  --   なく「コードで相対パスに保存する」 ワークフローなので、 図は実装済の scatter で
  --   代替する (過密は alpha で緩和)。 六角/件数ビンの実装と本格再現は EDA/Layers 章へ。
  let dPlot = diamonds |>> layer (scatter "carat" "price" <> size 2 <> alpha 0.05)
  saveSVGBound "diamonds-carat-price.svg" dPlot   -- = ggsave("diamonds.png") の等価 (相対パス)
  createDirectoryIfMissing False "data"           -- = Files ペインの New Folder
  DF.writeCsv "data/diamonds.csv" diamonds        -- = write_csv(diamonds, "data/diamonds.csv")
  putStrLn "\n# §6.2.3 relative paths: saveSVGBound + writeCsv を相対パスへ (生成物は gitignore)"
  putStrLn "  -> diamonds-carat-price.svg, data/diamonds.csv (CWD = この章ディレクトリ)"

  -- §6.2.4 Relative and absolute paths:
  -- R: project の中では **常に相対パス** を使う (絶対パスは共有を妨げる)。 区切りは
  --    Mac/Linux のスラッシュ "/" を使う (Windows のバックスラッシュは避ける)。
  -- Haskell の等価: System.FilePath は "/" 区切りで全 OS 可搬。 上で書いた
  --    "data/diamonds.csv" のように相対・スラッシュで書けば project の置き場所に依らない。
  putStrLn "\n# §6.2.4 paths: 相対・スラッシュ区切りで書く (絶対パスは共有を妨げるので避ける)"

  putStrLn "\nworkflow examples ran OK"
