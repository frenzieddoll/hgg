-- | チュートリアル 04: コードスタイル (R4DS 2e Ch4 "Workflow: code style")
--   https://r4ds.hadley.nz/workflow-style
--
--   R4DS 第 4 章は図を描かない「コードスタイル」 章 (本文の R 例はすべて
--   eval=false のスタイル見本・図は RStudio のスクリーンショットのみ)。 ここでは
--   R の各スタイル規則を **Haskell / 本プロジェクト規約 ([[CLAUDE.md]]) に対応づけ**、
--   「Strive for (こう書く) / Avoid (避ける)」 を示す。 Strive 版は実際に動かして
--   結果を出力し、 スタイルどおりのコードがちゃんと動くことを確認する。
--
--   ・このファイル自体が本プロジェクトのスタイル見本: 2 スペース字下げ・camelCase・
--     セクション境界の `-- ===` 罫線・mutate 相当の `=` 揃え・pipe は 1 行 1 動詞。
--   ・データは nycflights13 の flights 全量 (R4DS と同じ)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import qualified DataFrame           as DF
import qualified DataFrame.Functions as F
import           DataFrame.Operators ((|>), (.<))

main :: IO ()
main = do
  flights <- DF.readCsv "../_data/_raw/flights.csv"

  -- =========================================================================
  -- §4.2 Names — 変数名
  -- =========================================================================
  -- R: 小文字 + 数字 + `_` の snake_case (short_flights)。
  -- 本プロジェクト規約: 関数・束縛は **camelCase** (shortFlights)。 長く説明的な名前を
  -- 短い略語より優先する点は R と同じ。
  --
  --   Strive for:  shortFlights = ...      (説明的・camelCase)
  --   Avoid:       sf / SHORTFLIGHTS = ... (略語・全大文字)
  let shortFlights = flights |> DF.filterJust "air_time"
                             |> DF.filterWhere (F.col @Int "air_time" .< (60 :: DF.Expr Int))
  putStrLn "# §4.2 Names: shortFlights = flights |> filter(air_time < 60)"
  print (DF.dimensions shortFlights)

  -- =========================================================================
  -- §4.3 Spaces — 空白
  -- =========================================================================
  -- R: 二項演算子 (`+ - == <` …) の両側に空白 (`^` は例外)。 関数呼び出しの括弧の
  -- 内外に空白は入れない。 カンマの後に空白。 Haskell も同じ流儀。
  --
  --   Strive for:  z = (a + b) ^ 2 / d
  --   Avoid:       z=( a+b )^2/d
  let a = 3 :: Double
      b = 4 :: Double
      d = 2 :: Double
      z = (a + b) ^ (2 :: Int) / d
  putStrLn "\n# §4.3 Spaces: z = (a + b) ^ 2 / d"
  print z                                     -- => 24.5

  -- `=` の揃え (mutate で複数列を作るとき、 R は `=` を縦に揃えて読みやすくする)。
  -- dataframe でも NamedExpr を揃えて書ける (dep_time は HHMM 形式なので %/% 100 で
  -- 時、 %% 100 で分。 dataframe では div / mod)。
  --
  --   Strive for (= を揃える):
  let withCols = flights |> DF.deriveMany
        [ F.nullLift2 (\dist t -> fromIntegral dist / fromIntegral t :: Double)
            (F.col @Int "distance") (F.col @(Maybe Int) "air_time") `F.as` "speed"
        , F.nullLift (\t -> t `div` 100) (F.col @(Maybe Int) "dep_time") `F.as` "dep_hour"
        , F.nullLift (\t -> t `mod` 100) (F.col @(Maybe Int) "dep_time") `F.as` "dep_minute" ]
  putStrLn "\n# §4.3 Spaces: 揃えた mutate (speed / dep_hour / dep_minute)"
  print (withCols |> DF.select ["air_time", "distance", "speed", "dep_hour", "dep_minute"]
                  |> DF.take 5)

  -- =========================================================================
  -- §4.4 Pipes — パイプの整形
  -- =========================================================================
  -- R: `|>` の前に空白・行末に置き、 1 行 1 動詞。 名前付き引数を持つ関数
  -- (mutate / summarize) は引数を 1 行ずつ・2 スペース追加字下げ。 dataframe の
  -- `|>` も同じ流儀で書く。
  --
  --   Strive for:
  --     flights
  --       |> DF.groupBy ["tailnum"]
  --       |> DF.aggregate [ ... ]
  --   Avoid:
  --     flights|>DF.groupBy["tailnum"]|>DF.aggregate[...]
  --
  -- R4DS §4.4 の最初のパイプ例: 欠損を除いて目的地ごとに件数を数える。
  --   flights |> filter(!is.na(arr_delay), !is.na(tailnum)) |> count(dest)
  -- dest は非 null なので素直に groupBy できる。 1 行 1 動詞・|> 行末の整形が要点。
  let byDest = flights
        |> DF.filterJust "arr_delay"
        |> DF.filterJust "tailnum"
        |> DF.groupBy ["dest"]
        |> DF.aggregate [ F.countAll `F.as` "n" ]
        |> DF.sortBy [ DF.Desc (F.col @Int "n") ]
  putStrLn "\n# §4.4 Pipes: filter(!is.na ...) |> count(dest)"
  print (DF.dimensions byDest)
  print (byDest |> DF.take 5)
  -- ★もう一つの例 group_by(tailnum) |> summarize(delay, n) は「名前付き引数は 1 行ずつ」
  --   の整形見本 (README 参照)。 tailnum は元が Maybe Text で、 この版の dataframe は
  --   元欠損列での groupBy がクラッシュするため実行デモは count(dest) で代用した。

  -- =========================================================================
  -- §4.5 ggplot2 — `+` は `|>` と同じ流儀で
  -- =========================================================================
  -- R: ggplot の `+` もパイプと同じ整形 (1 行 1 レイヤ・引数が長ければ 1 行ずつ)。
  -- hgg では plot は `|>>` でデータを束ね、 レイヤは `<>` で重ねる
  -- (= ggplot の `+`)。 整形は同じ流儀:
  --
  --   summary
  --     |>> layer (line "month" "delay")
  --      <> layer (scatter "month" "delay")
  --
  -- ★R4DS はこの章で図を描かない (例は eval=false のスタイル見本) ので、 ここでも
  --   図は生成せず、 プロット元の集計だけ出す (月別平均遅延)。
  let delayByMonth = flights
        |> DF.filterJust "arr_delay"
        |> DF.groupBy ["month"]
        |> DF.aggregate [ F.mean (F.col @Int "arr_delay") `F.as` "delay" ]
        |> DF.sortBy [ DF.Asc (F.col @Int "month") ]
  putStrLn "\n# §4.5 ggplot2: group_by(month) |> summarize(delay) (プロット元)"
  print delayByMonth

  -- =========================================================================
  -- §4.6 Sectioning comments — セクション罫線
  -- =========================================================================
  -- R: `# Load data ----------` のような区切りコメントでスクリプトを分割。
  -- 本プロジェクト規約は `-- ===` 罫線 (このファイルが見本)。
  putStrLn "\n# §4.6 Sectioning: このファイルの -- === 罫線が R の # ---- に相当"

  putStrLn "\nstyle examples ran OK"
