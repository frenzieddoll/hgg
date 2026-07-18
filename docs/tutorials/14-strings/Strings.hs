-- | チュートリアル 14: 文字列 (R4DS 2e Ch14 "Strings")
--   https://r4ds.hadley.nz/strings
--
--   文字列はデータ解析でほぼ必ず触れる型である。本章では stringr (tidyverse) の
--   str_* 関数群に対応する操作を学ぶ:
--   ・文字列リテラルの作成 (エスケープ・raw string・特殊文字 \n \t \u)
--   ・データから多数の文字列を作る (str_c / str_glue / str_flatten)
--   ・文字列からデータを取り出す (separate_longer / separate_wider・too_few/too_many)
--   ・文字 (str_length・str_sub の正/負 index)
--   ・非英語テキスト (encoding=charToRaw・正規化比較 str_equal・locale 依存)
--   を扱う。R4DS Ch14 は **データ可視化の図が 0 枚**の章で (唯一の視覚要素は
--   RStudio オートコンプリートのスクショ 1 枚)、出力は count/mutate の表とベクトル値が
--   主役である。よって本ファイルは全ベクトル/全表出力を実データ (US SSA babynames・
--   192 万行) で忠実再現する。stringr/tidyr の関数は analyze 側 module
--   @Hanalyze.Data.Strings@ に実装済 (Phase 28 Ch14 A2-A5)。本ファイルは
--   それを呼んで R4DS の各出力を再現する。
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Main (main) where

import           Control.DeepSeq           (force, NFData)
import           Control.Exception         (try, evaluate, SomeException)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.Maybe                (fromMaybe)
import           Data.List                 (sortBy)
import           Data.Ord                  (Down (..), comparing)
import           Numeric                   (showHex)
import           Data.Word                 (Word8)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as M
import qualified DataFrame.IO.CSV                     as DF
import qualified DataFrame.Internal.Column            as DF
import qualified DataFrame.Internal.DataFrame         as DF
import qualified DataFrame.Operators                  as DF
import qualified DataFrame.Operations.Core            as DF
import qualified DataFrame.Internal.Column as DFC

-- analyze 側 stringr/tidyr 相当 (Phase 28 Ch14)
import           Hanalyze.Data.Strings

-- === 列抽出 (Numbers.hs / Logicals.hs と同方式・型不一致は [] / Nothing に倒す) ===

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

-- === 表示ヘルパ ===============================================================

sect :: String -> IO ()
sect s = putStrLn ("\n========== " <> s <> " ==========")

comma :: Int -> String
comma = reverse . go . reverse . show
  where go (a:b:c:d:rest) = a:b:c:',': go (d:rest)
        go xs             = xs

base :: FilePath
base = "../_data/_raw/"

-- | str_view 風: 各文字列を 1 行で番号付き表示 (制御文字は {\n}/{\t} に見える化)。
strView :: [Text] -> IO ()
strView xs = mapM_ row (zip [1 :: Int ..] xs)
  where row (i, t) = putStrLn $ "[" ++ show i ++ "] | " ++ visible (T.unpack t)
        visible = concatMap esc
        esc '\n' = "{\\n}"
        esc '\t' = "{\\t}"
        esc c    = [c]

-- | Maybe を R の NA 風に表示。
showM :: Show a => Maybe a -> String
showM = maybe "NA" show

-- | charToRaw のバイト列を R 風 16 進 (2 桁・小文字) で表示。
hexBytes :: [Word8] -> String
hexBytes = unwords . map (pad . flip showHex "")
  where pad s = if length s == 1 then '0' : s else s

main :: IO ()
main = do
  putStrLn "R4DS 2e Ch14 \"Strings\" の忠実再現 (図 0 枚・出力は表/ベクトル値)。"

  -- ===========================================================================
  sect "14.2 文字列の作成 — エスケープ / raw string / 特殊文字"
  -- 14.2.1 Escapes: R の c(single_quote, double_quote, backslash)
  let singleQuote = "'"    :: Text
      doubleQuote = "\""   :: Text
      backslash   = "\\"   :: Text
  putStrLn "x = c(single_quote, double_quote, backslash):"
  strView [singleQuote, doubleQuote, backslash]
  -- 14.2.2 Raw strings: Haskell に R の r\"(...)\" 相当は無い (注記のみ)。
  putStrLn "\n(14.2.2 raw string: Haskell に r\"(...)\" 相当の構文は無い。"
  putStrLn " バックスラッシュは通常どおりエスケープする → README で注記)"
  -- 14.2.3 特殊文字: \n \t µ(µ) \U0001f604(😄)
  let special = ["one\ntwo", "one\ttwo", "\x00b5", "\x1f604"] :: [Text]
  putStrLn "\nx = c(\"one\\ntwo\", \"one\\ttwo\", \"\\u00b5\", \"\\U0001f604\"):"
  strView special

  -- ===========================================================================
  sect "14.3 データから多数の文字列を作る — str_c / str_glue / str_flatten"
  -- 14.3.1 str_c (recycling)
  putStrLn $ "str_c(\"x\", \"y\")            = " ++ show (strC [["x"],["y"]])
  putStrLn $ "str_c(\"x\", \"y\", \"z\")       = " ++ show (strC [["x"],["y"],["z"]])
  putStrLn $ "str_c(\"Hello \", c(\"John\",\"Susan\")) = "
           ++ show (strC [["Hello "], ["John","Susan"]])
  -- str_c の NA 伝播 (mutate(greeting = str_c("Hi ", name, "!")))
  let names = [Just "Flora", Just "David", Just "Terra", Nothing] :: [Maybe Text]
      greet = strCMaybe [[Just "Hi "], names, [Just "!"]]
  putStrLn "\ndf |> mutate(greeting = str_c(\"Hi \", name, \"!\")):  (NA 伝播)"
  mapM_ (\(nm, g) -> putStrLn $ "  name=" ++ showM nm ++ "  greeting=" ++ showM g)
        (zip names greet)
  -- coalesce 相当 (NA を "you" で置換してから連結)
  let coalesce d = maybe d id
      greet1 = strC [["Hi "], map (coalesce "you") names, ["!"]]
  putStrLn "\ndf |> mutate(greeting1 = str_c(\"Hi \", coalesce(name, \"you\"), \"!\")):"
  mapM_ (putStrLn . ("  " ++) . T.unpack) greet1

  -- 14.3.2 str_glue
  let glued = strGlue "Hi {name}!" [("name", ["Flora","David","Terra","NA"])]
  putStrLn "\ndf |> mutate(greeting = str_glue(\"Hi {name}!\")):"
  mapM_ (putStrLn . ("  " ++) . T.unpack) glued
  let gluedEsc = strGlue "{{Hi {name}!}}" [("name", ["Flora","David","Terra","NA"])]
  putStrLn "\ndf |> mutate(greeting = str_glue(\"{{Hi {name}!}}\")):  (波括弧エスケープ)"
  mapM_ (putStrLn . ("  " ++) . T.unpack) gluedEsc

  -- 14.3.3 str_flatten
  putStrLn $ "\nstr_flatten(c(\"x\",\"y\",\"z\"))        = "
           ++ show (strFlatten "" ["x","y","z"])
  putStrLn $ "str_flatten(c(\"x\",\"y\",\"z\"), \", \")   = "
           ++ show (strFlatten ", " ["x","y","z"])
  -- group_by(name) |> summarize(fruits = str_flatten(fruit, ", "))
  let fruitRows = [ ("Carmen","banana"),("Carmen","apple")
                  , ("Marvin","nectarine")
                  , ("Terence","cantaloupe"),("Terence","papaya"),("Terence","mandarin") ]
                  :: [(Text,Text)]
      grouped = M.toList $ M.fromListWith (\a b -> b ++ a)
                  [ (nm, [fr]) | (nm, fr) <- fruitRows ]
  putStrLn "\ndf |> group_by(name) |> summarize(fruits = str_flatten(fruit, \", \")):"
  mapM_ (\(nm, frs) -> putStrLn $ "  " ++ T.unpack nm ++ "  " ++ T.unpack (strFlatten ", " frs))
        grouped

  -- ===========================================================================
  sect "14.4 文字列からデータを取り出す — separate_longer / separate_wider"
  -- 14.4.1 separate_longer_delim
  let df1 = DF.fromNamedColumns [("x", DF.fromList ["a,b,c","d,e","f" :: Text])]
  putStrLn "df1 |> separate_longer_delim(x, delim = \",\"):"
  print (separateLongerDelim "x" "," df1)
  -- separate_longer_position
  let df2 = DF.fromNamedColumns [("x", DF.fromList ["1211","131","21" :: Text])]
  putStrLn "df2 |> separate_longer_position(x, width = 1):"
  print (separateLongerPosition "x" 1 df2)

  -- 14.4.2 separate_wider_delim
  let df3 = DF.fromNamedColumns
              [("x", DF.fromList ["a10.1.2022","b10.2.2011","e15.1.2015" :: Text])]
  putStrLn "df3 |> separate_wider_delim(x, \".\", names = c(\"code\",\"edition\",\"year\")):"
  print (separateWiderDelim "x" "." [Just "code", Just "edition", Just "year"] df3)
  putStrLn "df3 |> separate_wider_delim(x, \".\", names = c(\"code\", NA, \"year\")):  (NA で列を捨てる)"
  print (separateWiderDelim "x" "." [Just "code", Nothing, Just "year"] df3)
  -- separate_wider_position
  let df4 = DF.fromNamedColumns
              [("x", DF.fromList ["202215TX","202122LA","202325CA" :: Text])]
  putStrLn "df4 |> separate_wider_position(x, c(year=4, age=2, state=2)):"
  print (separateWiderPosition "x" [("year",4),("age",2),("state",2)] df4)

  -- 14.4.3 too_few / too_many
  let dfFew = DF.fromNamedColumns
                [("a", DF.fromList ["1-1-1","1-1-2","1-3","1-3-2","1" :: Text])]
  putStrLn "\ndfFew |> separate_wider_delim(a, \"-\", c(\"x\",\"y\",\"z\"), too_few=\"debug\"):"
  print (separateWiderDelimWith "a" "-" [Just "x",Just "y",Just "z"] TooFewDebug TooManyError dfFew)
  putStrLn "dfFew |> ... too_few=\"align_start\":"
  print (separateWiderDelimWith "a" "-" [Just "x",Just "y",Just "z"] AlignStart TooManyError dfFew)
  let dfMany = DF.fromNamedColumns
                 [("a", DF.fromList ["1-1-1","1-1-2","1-3-5-6","1-3-2","1-3-5-7-9" :: Text])]
  putStrLn "dfMany |> ... too_many=\"drop\":"
  print (separateWiderDelimWith "a" "-" [Just "x",Just "y",Just "z"] TooFewError DropExtra dfMany)
  putStrLn "dfMany |> ... too_many=\"merge\":"
  print (separateWiderDelimWith "a" "-" [Just "x",Just "y",Just "z"] TooFewError MergeExtra dfMany)

  -- ===========================================================================
  sect "14.5 文字 — str_length / str_sub (実データ babynames)"
  -- 14.5.1 str_length のベクトル例
  let lenEx = [Just "a", Just "R for data science", Nothing] :: [Maybe Text]
  putStrLn $ "str_length(c(\"a\",\"R for data science\", NA)) = "
           ++ show (map (fmap strLength) lenEx)

  babynames <- DF.readCsv (base ++ "babynames.csv")
  let (nr, _) = DF.dimensions babynames
      nameCol = colPlain @Text "name" babynames
      nCol    = colPlain @Int  "n"    babynames
  putStrLn $ "\nbabynames: 全 " ++ comma nr ++ " 行"

  -- count(length = str_length(name), wt = n)  — 文字数分布 (n で加重)
  let lenDist = M.toList $ M.fromListWith (+)
                  [ (strLength nm, cnt) | (nm, cnt) <- zip nameCol nCol ]
  putStrLn "\nbabynames |> count(length = str_length(name), wt = n):"
  putStrLn "  length        n"
  mapM_ (\(l, c) -> putStrLn $ "  " ++ pad6 l ++ comma c) lenDist

  -- filter(str_length(name) == 15) |> count(name, wt = n, sort = TRUE)
  let len15 = M.toList $ M.fromListWith (+)
                [ (nm, cnt) | (nm, cnt) <- zip nameCol nCol, strLength nm == 15 ]
      len15Sorted = sortBy (comparing (Down . snd) <> comparing fst) len15
  putStrLn $ "\nbabynames |> filter(str_length(name) == 15) |> count(name, wt = n, sort = TRUE):"
  putStrLn $ "  (異なる名前: " ++ show (length len15) ++ " 件・上位 6)"
  putStrLn "  name                n"
  mapM_ (\(nm, c) -> putStrLn $ "  " ++ T.unpack nm ++ replicate (16 - T.length nm) ' ' ++ show c)
        (take 6 len15Sorted)

  -- 14.5.2 str_sub の正/負 index
  let fruits = ["Apple","Banana","Pear"] :: [Text]
  putStrLn $ "\nstr_sub(c(\"Apple\",\"Banana\",\"Pear\"), 1, 3)   = "
           ++ show (map (strSub 1 3) fruits)
  putStrLn $ "str_sub(c(\"Apple\",\"Banana\",\"Pear\"), -3, -1) = "
           ++ show (map (strSub (-3) (-1)) fruits)
  putStrLn $ "str_sub(\"a\", 1, 5)                          = "
           ++ show (strSub 1 5 "a")
  -- mutate(first = str_sub(name,1,1), last = str_sub(name,-1,-1)) の先頭 6 行
  putStrLn "\nbabynames |> mutate(first = str_sub(name,1,1), last = str_sub(name,-1,-1)):  (先頭 6 行)"
  putStrLn "  name       first last"
  mapM_ (\nm -> putStrLn $ "  " ++ T.unpack nm ++ replicate (11 - T.length nm) ' '
                         ++ T.unpack (strSub 1 1 nm) ++ "     " ++ T.unpack (strSub (-1) (-1) nm))
        (take 6 nameCol)

  -- ===========================================================================
  sect "14.6 非英語テキスト — encoding / 正規化比較 / locale"
  -- 14.6.1 charToRaw
  putStrLn $ "charToRaw(\"Hadley\") = " ++ hexBytes (charToRaw "Hadley")
  -- 14.6.2 letter variations: ü を 合成済(ü) と 基底+結合(ü) で
  let uc = "\x00fc"      :: Text   -- 合成済 ü (1 コードポイント)
      ud = "u\x0308"     :: Text   -- u + 結合分音記号 (2 コードポイント)
  putStrLn $ "\nu = c(\"\\u00fc\", \"u\\u0308\")  (どちらも見た目は ü)"
  putStrLn $ "str_length(u)        = " ++ show [strLength uc, strLength ud]
  putStrLn $ "str_sub(u, 1, 1)     = [\"" ++ T.unpack (strSub 1 1 uc)
           ++ "\",\"" ++ T.unpack (strSub 1 1 ud) ++ "\"]"
  putStrLn $ "u[[1]] == u[[2]]     = " ++ show (uc == ud)
  putStrLn $ "str_equal(u[[1]], u[[2]]) = " ++ show (strEqual uc ud)
  -- 14.6.3 locale 依存 (既定 locale = コードポイント順・概念注記は README)
  putStrLn $ "\nstr_to_upper(c(\"i\",\"hello\"))  = "
           ++ show (map strToUpper ["i","hello" :: Text])
  putStrLn $ "str_sort(c(\"a\",\"c\",\"ch\",\"h\",\"z\")) = "
           ++ show (strSort ["a","c","ch","h","z"])
  putStrLn "(locale 依存の str_to_upper(\"tr\") / str_sort(\"cs\") は README で概念注記)"

  putStrLn "\n--- 完了 (全出力は実データ・R4DS Ch14 と突合済) ---"

-- | 文字数を 6 幅右寄せ + 余白 (length 列の桁揃え)。
pad6 :: Int -> String
pad6 l = let s = show l in replicate (max 0 (6 - length s)) ' ' ++ s ++ "  "
