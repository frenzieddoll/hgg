-- | チュートリアル 07: データの読み込み (R4DS 2e Ch7 "Data import")
--   https://r4ds.hadley.nz/data-import
--
--   readr (read_csv 系) で平文の矩形ファイル (CSV 等) を読む方法を学ぶ章。 R4DS の
--   students.csv (列名に空白/大文字/ドット、 "N/A" 文字列、 AGE に空欄と "five") を
--   題材に、 読み込み → 欠損/列名/型の調整 → 複数ファイルの結合 → 書き出し →
--   手組み (data entry) までを通す。 図はテーブル 1 枚のみ (プロット図なし)。
--
--   ★R/readr の概念 → dataframe の API への忠実対応。 dataframe に無い機能 (skip /
--   comment / factor 型 / parquet 書込) は近似でごまかさず、 honest に実装で埋めるか
--   相違として明記する。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main (main) where

import           Data.List                 (isPrefixOf, isSuffixOf, sort)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Text.Read                 (readMaybe)
import           System.Directory          (listDirectory, removeFile, doesFileExist)
import qualified DataFrame                 as DF
import qualified DataFrame.Functions        as F
import           DataFrame.Operators       ((|>))

-- ===========================================================================
-- ヘルパ
-- ===========================================================================

-- | AGE 列の値を整える: "five" → 5、 数字文字列 → その数、 欠損はそのまま。
--   (= R4DS の parse_number(if_else(age == "five", "5", age)))。
fixAge :: Maybe Text -> Maybe Int
fixAge Nothing       = Nothing
fixAge (Just "five") = Just 5
fixAge (Just t)      = readMaybe (T.unpack t)

-- | インライン CSV 文字列を一時ファイル経由で読む (= R の read_csv("a,b\n1,2"))。
--   dataframe は FilePath からのみ読むので、 文字列を temp に書いて opts つきで読む。
--   生成 temp は読み終えたら片付ける。
readInline :: DF.ReadOptions -> String -> IO DF.DataFrame
readInline opts content = do
  let tmp = "_inline.csv"
  writeFile tmp content
  df <- DF.readCsvWithOpts opts tmp
  removeFile tmp
  return df

-- | R の read_csv(skip = n): 先頭 n 行を捨ててから読む。 dataframe に skip 引数は
--   無いので、 行を前処理で落としてから読む (honest な穴埋め)。
readSkip :: Int -> String -> IO DF.DataFrame
readSkip n content = readInline DF.defaultReadOptions
  (unlines (drop n (lines content)))

-- | R の read_csv(comment = "#"): 先頭が "#" の行を捨ててから読む。 dataframe に
--   comment 引数は無いので、 同様に前処理で落とす。
readComment :: Char -> String -> IO DF.DataFrame
readComment c content = readInline DF.defaultReadOptions
  (unlines (filter (not . ([c] `isPrefixOf`) . dropWhile (== ' ')) (lines content)))

main :: IO ()
main = do
  putStrLn "##### §7.2 Reading data from a file #####"

  -- =========================================================================
  -- §7.2 read_csv の基本 (students.csv)
  -- =========================================================================
  -- R: students <- read_csv("data/students.csv")
  students0 <- DF.readCsv "data/students.csv"
  putStrLn "\n# read_csv の col spec (推論した列名・型)"
  print (DF.columnNames students0)        -- BOM は自動除去される
  print students0

  -- -------------------------------------------------------------------------
  -- §7.2.1 Practical advice — na の指定
  -- -------------------------------------------------------------------------
  -- R: read_csv("data/students.csv", na = c("N/A", ""))
  -- dataframe: ReadOptions の missingIndicators に渡す。 既定は空文字のみ欠損扱い。
  students <- DF.readCsvWithOpts
                DF.defaultReadOptions { DF.missingIndicators = ["N/A", ""] }
                "data/students.csv"
  putStrLn "\n# na = c(\"N/A\",\"\") 指定後 (favourite.food の N/A が欠損に)"
  print students

  -- -------------------------------------------------------------------------
  -- §7.2.1 非構文名 → snake_case (= rename / janitor::clean_names())
  -- -------------------------------------------------------------------------
  -- R: rename(student_id = `Student ID`, full_name = `Full Name`) / janitor::clean_names()
  let renamed = students |> DF.renameMany
        [ ("Student ID",     "student_id")
        , ("Full Name",      "full_name")
        , ("favourite.food", "favourite_food")
        , ("mealPlan",       "meal_plan")
        , ("AGE",            "age") ]
  putStrLn "\n# 列名を snake_case に (= clean_names)"
  print (DF.columnNames renamed)

  -- §7.2.1 factor + age 修正
  -- R: mutate(meal_plan = factor(meal_plan),
  --           age = parse_number(if_else(age == "five", "5", age)))
  -- ★相違 (honest): この版の dataframe には R の factor (<fct>) に当たる独立の型は無い。
  --   meal_plan は Text のまま (順序つき水準は後の章のトピック)。 ここでは age の修正のみ行う。
  let cleaned = renamed |> DF.apply fixAge "age"
  putStrLn "\n# age を修正 (five → 5・数値列に。 factor は後述の理由で Text のまま)"
  print cleaned

  -- =========================================================================
  -- §7.2.3 Other arguments — その他の引数
  -- =========================================================================
  putStrLn "\n##### §7.2.3 Other arguments #####"

  -- インライン CSV 文字列を読む (= read_csv("a,b,c\n1,2,3\n4,5,6"))
  abc <- readInline DF.defaultReadOptions "a,b,c\n1,2,3\n4,5,6\n"
  putStrLn "\n# インライン CSV (a,b,c / 1,2,3 / 4,5,6)"
  print abc

  -- skip = 2 (先頭 2 行のメタデータを飛ばす)
  skipped <- readSkip 2 "The first line of metadata\nThe second line of metadata\nx,y,z\n1,2,3\n"
  putStrLn "\n# skip = 2 (メタデータ 2 行を飛ばす。 dataframe に skip 無 → 前処理で実装)"
  print skipped

  -- comment = "#" (# で始まる行を捨てる)
  commented <- readComment '#' "# A comment I want to skip\nx,y,z\n1,2,3\n"
  putStrLn "\n# comment = \"#\" (# 行を捨てる。 dataframe に comment 無 → 前処理で実装)"
  print commented

  -- col_names = FALSE (見出し無し → 連番列名)
  noHdr <- readInline DF.defaultReadOptions { DF.headerSpec = DF.NoHeader } "1,2,3\n4,5,6\n"
  putStrLn "\n# col_names = FALSE (見出し無し → dataframe が付ける連番列名)"
  print (DF.columnNames noHdr)
  print noHdr

  -- col_names = c("x","y","z") (列名を明示)
  named <- readInline DF.defaultReadOptions { DF.headerSpec = DF.ProvideNames ["x","y","z"] } "1,2,3\n4,5,6\n"
  putStrLn "\n# col_names = c(\"x\",\"y\",\"z\") (列名を明示)"
  print named

  -- =========================================================================
  -- §7.2.4 Other file types — 他のファイル形式 (区切り文字)
  -- =========================================================================
  putStrLn "\n##### §7.2.4 Other file types #####"
  -- R: read_csv2(;) / read_tsv(\t) / read_delim(任意) は区切り文字違い。
  -- dataframe: ReadOptions の columnSeparator を変えるだけ。
  semi <- readInline DF.defaultReadOptions { DF.columnSeparator = ';' } "a;b;c\n1;2;3\n"
  putStrLn "\n# read_csv2 相当 (columnSeparator = ';')"
  print semi
  tsv <- readInline DF.defaultReadOptions { DF.columnSeparator = '\t' } "a\tb\tc\n1\t2\t3\n"
  putStrLn "\n# read_tsv 相当 (columnSeparator = '\\t')"
  print tsv

  -- =========================================================================
  -- §7.3 Controlling column types — 列型の制御
  -- =========================================================================
  putStrLn "\n##### §7.3 Controlling column types #####"

  -- §7.3.1 型推論の例
  guessed <- readInline DF.defaultReadOptions
               "logical,numeric,date,string\nTRUE,1,2021-01-15,abc\nfalse,4.5,2021-02-15,def\nT,Inf,2021-02-16,ghi\n"
  putStrLn "\n# §7.3.1 型推論 (logical/numeric/date/string の各列を推論)"
  print guessed

  -- §7.3.2 欠損が型推論を壊す例
  -- simple_csv の "." は既定だと欠損と見なされず、 x は文字列列になる。
  let simpleCsv = "x\n10\n.\n20\n30\n"
  simpleDefault <- readInline DF.defaultReadOptions simpleCsv
  putStrLn "\n# §7.3.2 simple_csv を既定で読む (\".\" のため x が文字列列に)"
  print simpleDefault
  -- na = "." を指定すると "." が欠損になり、 x が数値列として推論される。
  simpleNa <- readInline DF.defaultReadOptions { DF.missingIndicators = ["."] } simpleCsv
  putStrLn "\n# na = \".\" 指定 (\".\" を欠損扱い → x が数値列に)"
  print simpleNa

  -- §7.3.3 列型の明示 (= col_types) / .default / cols_only
  -- R: read_csv(another_csv, col_types = cols(.default = col_character()))
  -- dataframe: typeSpec = NoInference で全列を文字列のまま読む (= .default = col_character)。
  let anotherCsv = "x,y,z\n1,2,3\n"
  allChar <- readInline DF.defaultReadOptions { DF.typeSpec = DF.NoInference } anotherCsv
  putStrLn "\n# §7.3.3 cols(.default = col_character()) 相当 (typeSpec = NoInference)"
  print allChar
  -- R: cols_only(x = col_character()) は指定列だけ読む → 読んでから select で代替。
  colsOnly <- readInline DF.defaultReadOptions anotherCsv
  putStrLn "\n# cols_only(x = ...) 相当 (読んでから select [\"x\"])"
  print (colsOnly |> DF.select ["x"])

  -- =========================================================================
  -- §7.4 Reading data from multiple files — 複数ファイルの結合
  -- =========================================================================
  putStrLn "\n##### §7.4 Reading data from multiple files #####"
  -- R: list.files("data", pattern = "sales\\.csv$", full.names = TRUE)
  entries <- listDirectory "data"
  let salesFiles = sort [ "data/" ++ f | f <- entries, "sales.csv" `isSuffixOf` f ]
  putStrLn "\n# list.files(pattern = \"sales\\\\.csv$\") 相当"
  mapM_ putStrLn salesFiles
  -- R: read_csv(sales_files, id = "file") — 3 ファイルを縦に積み、 file 列で出所を残す。
  -- dataframe に縦結合 1 関数は無いので、 各列をリスト化して連結し再構築する (honest)。
  stacked <- stackSalesFiles salesFiles
  putStrLn "\n# read_csv(sales_files, id = \"file\") 相当 (縦積み + file 列)"
  print (DF.dimensions stacked)
  print stacked

  -- =========================================================================
  -- §7.5 Writing to a file — 書き出し
  -- =========================================================================
  putStrLn "\n##### §7.5 Writing to a file #####"
  -- R: write_csv(students, "students.csv")
  -- ★この版の writeCsv は欠損 (Nothing) 列を直列化できないので、 欠損なし列のみ書く。
  let writable = cleaned |> DF.select ["student_id","full_name","meal_plan"]
  DF.writeCsv "students-clean.csv" writable
  putStrLn "\n# write_csv 相当 → students-clean.csv (欠損なし列のみ)"
  -- 書いた CSV を読み戻すと型情報は失われる (= R4DS の指摘。 CSV は中間キャッシュに不向き)。
  roundTrip <- DF.readCsv "students-clean.csv"
  putStrLn "# 読み戻し (型情報は CSV では保持されない)"
  print roundTrip
  removeFileIfExists "students-clean.csv"

  -- =========================================================================
  -- §7.6 Data entry — 手組み (tibble / tribble)
  -- =========================================================================
  putStrLn "\n##### §7.6 Data entry #####"
  -- R: tibble(x = c(1,2,5), y = c("h","m","g"), z = c(0.08,0.83,0.60))  (列ごと)
  let byCol = DF.fromNamedColumns
        [ ("x", DF.fromList ([1,2,5]          :: [Int]))
        , ("y", DF.fromList (["h","m","g"]    :: [Text]))
        , ("z", DF.fromList ([0.08,0.83,0.60] :: [Double])) ]
  putStrLn "\n# tibble(...) 相当 (列ごとに組む = fromNamedColumns)"
  print byCol

  -- R: tribble(~x, ~y, ~z, 1,"h",0.08, 2,"m",0.83, 5,"g",0.60)  (行ごと)
  -- ★相違 (honest): Haskell には tribble の専用糖衣は無い。 行レイアウトのタプルリストを
  --   書いて unzip3 で列に組めば、 同じ「行ごとに読みやすく並べる」 意図を表せる。
  let rows = [ (1 :: Int, "h" :: Text, 0.08 :: Double)
             , (2,        "m",         0.83)
             , (5,        "g",         0.60) ]
      (xs, ys, zs) = unzip3 rows
      byRow = DF.fromNamedColumns
        [ ("x", DF.fromList xs), ("y", DF.fromList ys), ("z", DF.fromList zs) ]
  putStrLn "\n# tribble(...) 相当 (行ごとに書く → unzip3 で列に)"
  print byRow

  putStrLn "\ndata import examples ran OK"

-- | §7.4: sales ファイル群を縦に積み、 出所を表す file 列を足す。
--   各ファイルは同じ列 (month,year,brand,item,n) を持つ。
stackSalesFiles :: [FilePath] -> IO DF.DataFrame
stackSalesFiles paths = do
  dfs <- mapM DF.readCsv paths
  let months = concat [ DF.columnAsList (F.col @Text "month") d :: [Text] | d <- dfs ]
      years  = concat [ DF.columnAsList (F.col @Int  "year")  d :: [Int]  | d <- dfs ]
      brands = concat [ DF.columnAsList (F.col @Int  "brand") d :: [Int]  | d <- dfs ]
      items  = concat [ DF.columnAsList (F.col @Int  "item")  d :: [Int]  | d <- dfs ]
      ns     = concat [ DF.columnAsList (F.col @Int  "n")     d :: [Int]  | d <- dfs ]
      files  = concat [ replicate (length (DF.columnAsList (F.col @Int "n") d :: [Int])) (T.pack p)
                      | (p, d) <- zip paths dfs ]
  return $ DF.fromNamedColumns
    [ ("file",  DF.fromList files)
    , ("month", DF.fromList months)
    , ("year",  DF.fromList years)
    , ("brand", DF.fromList brands)
    , ("item",  DF.fromList items)
    , ("n",     DF.fromList ns) ]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists p = do
  e <- doesFileExist p
  if e then removeFile p else return ()
