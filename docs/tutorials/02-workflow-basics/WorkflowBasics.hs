-- | チュートリアル 02: ワークフローの基礎 (R4DS 2e Ch2 "Workflow: basics")
--   https://r4ds.hadley.nz/workflow-basics
--
--   R4DS 第 2 章は図を描かない「R コーディングの基礎」 章。 ここでは R の各例を
--   Haskell の等価コードに**忠実に対応づけて**実行し、 同じ結果を出力する。
--   R と Haskell で考え方が違う箇所 (再代入でなく束縛・ベクトル演算は map・
--   名前付き引数が無い・命名は camelCase 等) は README とコメントで明示する。
--
--   実行: cabal run tut-02-workflow-basics
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Text.Printf (printf)

main :: IO ()
main = do
  -- =========================================================================
  -- §2.1 Coding basics: 基本的な計算 (R: 1 / 200 * 30 など)
  -- =========================================================================
  -- R と同じ中置演算子・優先順位。 Haskell の / は分数なので Double で評価する。
  putStrLn "# §2.1 基本計算"
  print (1 / 200 * 30 :: Double)          -- R: 1 / 200 * 30      => 0.15
  print ((59 + 73 + 2) / 3 :: Double)     -- R: (59 + 73 + 2) / 3 => 44.66667
  print (sin (pi / 2) :: Double)          -- R: sin(pi / 2)       => 1

  -- オブジェクトの作成。 R は `x <- 3 * 4` (再代入可)、 Haskell は `let` 束縛 (不変)。
  let x = 3 * 4 :: Int
  -- R 同様、 束縛しただけでは表示されない。 値を見るには名前を評価 (= print)。
  print x                                  -- R: x                => 12

  -- ベクトル: R は c(2,3,5,7,11,13)。 Haskell はリスト (または Data.Vector)。
  let primes = [2, 3, 5, 7, 11, 13] :: [Int]
  -- ★相違: R はベクトルに算術を「全要素へ自動適用 (broadcast)」 するが、
  --   Haskell は自動 broadcast しないので map で各要素に適用する。
  print (map (* 2) primes)                 -- R: primes * 2  => 4 6 10 14 22 26
  print (map (subtract 1) primes)          -- R: primes - 1  => 1 2 4 6 10 12

  -- =========================================================================
  -- §2.2 Comments: R は `#`、 Haskell は `--` (このファイル自体が例)
  -- =========================================================================
  putStrLn "\n# §2.2 コメント"
  -- create vector of primes  (R の # コメントに相当する Haskell の -- コメント)
  let primes2 = [2, 3, 5, 7, 11, 13] :: [Int]
  -- multiply primes by 2
  print (map (* 2) primes2)                -- => 4 6 10 14 22 26

  -- =========================================================================
  -- §2.3 What's in a name?: 命名規約
  -- =========================================================================
  -- R は snake_case 推奨。 Haskell の慣例は camelCase (関数・束縛) /
  -- PascalCase (型・コンストラクタ)。 本プロジェクトも camelCase ([[CLAUDE.md]])。
  putStrLn "\n# §2.3 命名"
  let thisIsAReallyLongName = 3.5 :: Double   -- R: this_is_a_really_long_name <- 3.5
  print thisIsAReallyLongName              -- => 3.5
  let rRocks = 2 ^ (3 :: Int) :: Int       -- R: r_rocks <- 2^3
  print rRocks                             -- => 8
  -- ★大文字小文字・綴りは厳密。 rRock や RRocks と書けばコンパイルエラー
  --   (R では実行時 "object not found")。 Haskell は型検査で出荷前に捕まえる。

  -- =========================================================================
  -- §2.4 Calling functions
  -- =========================================================================
  -- R: seq(from = 1, to = 10) / seq(1, 10)。 Haskell に名前付き引数は無いので
  --   位置引数 or レンジ記法。 等差列は [from..to] が最も Haskell 的。
  putStrLn "\n# §2.4 関数呼び出し"
  print (enumFromTo 1 10 :: [Int])         -- R: seq(1, 10)  => 1 2 .. 10
  print ([1 .. 10] :: [Int])               -- 同じ (レンジ記法)
  let greeting = "hello world" :: String   -- R: x <- "hello world"
  putStrLn greeting                        -- => hello world

  -- =========================================================================
  -- §2.5 Exercises (演習の要点)
  -- =========================================================================
  -- 1. R4DS の `my_varıable` は i がトルコ語の点なし i (ı) で別字。 Haskell でも
  --    識別子が 1 文字でも違えば別物 = スコープエラー。 「綴り・字種は厳密」。
  -- 2. R 例 `libary(todyverse)` 等のタイポ修正は library(tidyverse) /
  --    aes(x = displ, y = hwy) / method = "lm"。 Haskell でも import 名・引数の
  --    綴りと閉じ括弧/引用符の対は厳密。
  putStrLn "\n# done (図は無い章・出力で確認)"
