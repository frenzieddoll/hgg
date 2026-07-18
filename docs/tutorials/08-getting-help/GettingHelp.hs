-- | チュートリアル 08: 助けの求め方 (R4DS 2e Ch8 "Workflow: getting help")
--   https://r4ds.hadley.nz/workflow-help
--
--   R4DS 第 8 章は **図を描かない** 散文の運用章。 主題は次の 3 つ:
--     ・Google is your friend  — 検索 (エラーメッセージ含む) と Stack Overflow
--     ・Making a reprex        — 最小・再現可能な例 (reproducible example) の作り方
--     ・Investing in yourself  — 日々の学習・コミュニティの追い方
--   いずれも R/RStudio/tidyverse 固有の助言なので、 ここでは近似や置換ではなく
--   「R の運用 → Haskell の等価運用」 に honest に対応づける (詳細は README)。
--
--   このファイル自体が R4DS §8.2 のいう **reprex (reproducible example)** の見本:
--     1. 必要な import を先頭にまとめる         (= 必要パッケージを明示)
--     2. データを **インラインで埋め込む**       (= R の dput() の等価)
--     3. 問題に直接関係するコードだけを最小に書く (= minimal)
--   別シェルで `cabal run tut-08-getting-help` (= まっさらな環境) すれば、 誰の手元でも
--   同じ出力が再生成される。 これが「再現可能」 の検証 (R4DS §8.2 末尾の助言と同じ)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

-- ★R4DS §8.2-1: reprex は **必要パッケージを先頭に** まとめる。 そうすれば相手が
--   どの依存を入れれば動くか一目で分かる。 Haskell では .cabal の build-depends で
--   依存を宣言し、 この import 群がその利用宣言にあたる。
import           Data.Text           (Text)
import qualified DataFrame.Internal.Column            as DF
import qualified DataFrame.Internal.DataFrame         as DF
import qualified DataFrame.Operations.Core            as DF
import qualified DataFrame.Functions as F

main :: IO ()
main = do

  -- =========================================================================
  -- §8.2 Making a reprex — 最小・再現可能な例
  -- =========================================================================
  -- R4DS の reprex 例: クリップボードのコードを reprex() に通すと、 コードと **その
  --   出力** が #> 付きの Markdown になって、 そのまま貼って動かせる:
  --     y <- 1:4
  --     mean(y)
  --     #> [1] 2.5
  -- Haskell の等価 = 自己完結した最小コード + その出力。 GHCi なら式を打てば下に
  --   結果が出る。 ここでは通し実行 (cabal run) で「コード + #> 出力」 を再現する。
  let y = [1 .. 4] :: [Double]
      meanY = sum y / fromIntegral (length y)   -- = R の mean(y)
  putStrLn "# §8.2 reprex: y <- 1:4; mean(y)"
  putStrLn "y <- [1..4]"
  putStrLn "mean y"
  putStrLn ("#> " ++ show meanY)                -- #> 2.5  (R は #> [1] 2.5)

  -- -------------------------------------------------------------------------
  -- §8.2-2 データの埋め込み — R の dput() の等価
  -- -------------------------------------------------------------------------
  -- R: 再現例にデータを含める一番簡単な方法は dput(df) でデータを **再生成する R コード**
  --    を吐かせ、 それを貼ること。 問題を示す **最小の部分集合** を使う。
  -- Haskell の等価: データを fromNamedColumns / fromList で **リテラルとして埋め込む**。
  --    これなら相手は CSV ファイルを別途用意しなくても、 貼って即実行できる。
  --    (= R の `mtcars <- dput の出力` と同じ役割。 ここでは問題再現に十分な小さな toy。)
  let toy = DF.fromNamedColumns
        [ ("id",    DF.fromList ([1, 2, 3]            :: [Int]))
        , ("group", DF.fromList (["a", "b", "a"]      :: [Text]))
        , ("value", DF.fromList ([10.0, 20.0, 30.0]   :: [Double])) ]
  putStrLn "\n# §8.2-2 データ埋め込み (= R の dput()): fromNamedColumns でリテラル化"
  print toy

  -- 埋め込んだ toy に対する最小の計算 (= 問題に直接関係する部分だけ)。
  let vals  = DF.columnAsList (F.col @Double "value") toy :: [Double]
      total = sum vals
  putStrLn ("#> sum(value) = " ++ show total)   -- #> 60.0

  -- R4DS §8.2 末尾: 「まっさらな R セッションで貼って動くか」 を最後に確認する。
  -- Haskell の等価: 別シェルで `cabal run tut-08-getting-help` (依存も含めビルドし直し)
  --   して同じ出力が出れば、 この例は本当に再現可能。
  putStrLn "\ngetting-help reprex ran OK (別シェルで cabal run しても同じ出力 = 再現可能)"
