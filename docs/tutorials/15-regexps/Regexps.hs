-- | チュートリアル 15: 正規表現 (R4DS 2e Ch15 "Regular expressions")
--   https://r4ds.hadley.nz/regexps
--
--   正規表現 (regex) は文字列のパターンを記述する簡潔な言語で、stringr の str_*
--   関数のほとんどが受け取る。本章では
--   ・パターンの基礎 (str_view・. ? + * | [] [^])
--   ・主要関数 (str_detect / str_count / str_replace / separate_wider_regex)
--   ・パターン詳細 (エスケープ・アンカー ^ $ \b・文字クラス \d \s \w・量指定子・グループ)
--   ・パターン制御 (ignore_case・fixed・coll)
--   ・実践 (words/babynames でのパターン作成)
--   を学ぶ。stringr/tidyr 相当は analyze 側 @Hanalyze.Data.Strings@ の regex 節
--   (Phase 28 Ch15・**regex-tdfa** バックエンド) に実装済。本ファイルはそれを呼んで
--   R4DS の各出力を実データ (stringr words 980・fruit 80・US SSA babynames 192 万) で
--   忠実再現する。
--
--   ★regex-tdfa は POSIX ERE ゆえ、PCRE ショートハンド \d \s \w は本モジュールが
--   POSIX クラス ([[:digit:]] 等) に内部変換して同じパターン文字列で動かす。後方参照
--   \1 (POSIX 非対応) は概念のみ注記する。
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
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as M
import qualified DataFrame                 as DF
import qualified DataFrame.Internal.Column as DFC

import           Hanalyze.Data.Strings

-- === 列抽出 (Strings.hs / Numbers.hs と同方式) ================================

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

-- | str_view 風: 各文字列のマッチ部分を <> で囲んで表示 (R の str_view と同じ見せ方)。
--   非マッチ要素は省く (str_view も既定でマッチした要素のみ番号付き表示)。
strViewMatch :: Text -> [Text] -> IO ()
strViewMatch pat xs =
  mapM_ row [ (i, x) | (i, x) <- zip [1 :: Int ..] xs, strDetect pat x ]
  where
    row (i, x) = putStrLn $ "[" ++ show i ++ "] | " ++ T.unpack (highlight x)
    -- 先頭から繰り返しマッチを <> で囲む。
    highlight t = case strLocate pat t of
      Nothing      -> t
      Just (s, e)  ->
        let pre  = strSub 1 (s - 1) t
            mid  = strSub s e t
            rest = strSub (e + 1) (T.length t) t
        in pre <> "<" <> mid <> ">" <> highlight rest

main :: IO ()
main = do
  putStrLn "R4DS 2e Ch15 \"Regular expressions\" の忠実再現 (regex-tdfa・出力は表/ベクトル値)。"

  words'    <- colPlain @Text "word"     <$> DF.readCsv (base ++ "words.csv")
  fruit     <- colPlain @Text "fruit"    <$> DF.readCsv (base ++ "fruit.csv")
  babynames <- DF.readCsv (base ++ "babynames.csv")
  let bnNames = colPlain @Text "name" babynames
      bnN     = colPlain @Int  "n"    babynames

  -- ===========================================================================
  sect "15.2 パターンの基礎 — str_view (<> ハイライト)"
  putStrLn "str_view(fruit, \"berry\"):  (berry を含む果物・マッチ部を <> 表示)"
  strViewMatch "berry" fruit
  putStrLn "\nstr_view(fruit, \"a...e\"):  (a で始まり 3 文字おいて e・. = 任意 1 文字)"
  strViewMatch "a...e" fruit
  putStrLn "\nstr_view(c(\"apple\",\"pair\",\"banana\"), \"an\"):"
  strViewMatch "an" ["apple","pair","banana"]
  putStrLn "str_view(..., \"a.\"):  (a + 任意 1 文字)"
  strViewMatch "a." ["apple","pair","banana"]
  putStrLn "str_view(..., \"a|e\"):  (a または e)"
  strViewMatch "a|e" ["apple","pair","banana"]

  -- ===========================================================================
  sect "15.3 主要関数 — str_detect / str_count / str_replace"
  -- 15.3.1 str_detect
  putStrLn $ "str_detect(c(\"apple\",\"banana\",\"pear\"), \"p\") = "
           ++ show (map (strDetect "p") ["apple","banana","pear"])
  -- babynames |> filter(str_detect(name,"x")) |> count(name, wt=n, sort=TRUE)
  -- ★case-sensitive (R4DS の str_detect(name,"x") は ignore_case 無し)。
  let xNames = M.toList $ M.fromListWith (+)
                 [ (nm, c) | (nm, c) <- zip bnNames bnN, strDetect "x" nm ]
      xSorted = sortBy (comparing (Down . snd) <> comparing fst) xNames
  putStrLn $ "\nbabynames |> filter(str_detect(name, \"x\")) |> count(name, wt=n, sort=TRUE):"
  putStrLn $ "  (異なる名前: " ++ comma (length xNames) ++ " 件・上位 6)"
  mapM_ (\(nm,c) -> putStrLn $ "  " ++ T.unpack nm ++ replicate (12 - T.length nm) ' ' ++ comma c)
        (take 6 xSorted)
  -- 15.3.2 str_count: 母音/子音
  putStrLn "\nstr_count: babynames |> mutate(vowels=str_count(name,\"[aeiou]\"), ...) (先頭 5 名・ignore_case):"
  putStrLn "  name       vowels consonants"
  let distinctHead = take 5 (dedup bnNames)
  mapM_ (\nm -> putStrLn $ "  " ++ T.unpack nm ++ replicate (11 - T.length nm) ' '
                         ++ show (strCountCI "[aeiou]" nm) ++ "      "
                         ++ show (strCountCI "[^aeiou]" nm))
        distinctHead
  -- 15.3.3 replace
  putStrLn $ "\nstr_replace_all(\"a-b-c\", \"-\", \"+\")        = "
           ++ T.unpack (strReplaceAll "-" "+" "a-b-c")
  putStrLn $ "str_replace_all(\"hello\", \"[aeiou]\", \"-\")  = "
           ++ T.unpack (strReplaceAll "[aeiou]" "-" "hello")
  putStrLn $ "str_replace_all(\"abcd\", \"([a-z])([a-z])\", \"\\\\2\\\\1\")  (隣接2文字を入替) = "
           ++ T.unpack (strReplaceAll "([a-z])([a-z])" "\\2\\1" "abcd")
  -- 15.3.4 separate_wider_regex (R4DS の <name>-gender-age 例)
  let dfPeople = DF.fromNamedColumns
        [("str", DF.fromList
            ["<Sheryl>-F-34","<Kisha>-F-45","<Brandon>-N-33","<Sharon>-F-38","<Penny>-F-58" :: Text])]
      peopleOut = separateWiderRegex "str"
        [ (Nothing,        "<")
        , (Just "name",    "[A-Za-z]+")
        , (Nothing,        ">-")
        , (Just "gender",  "[A-Z]")
        , (Nothing,        "-")
        , (Just "age",     "[0-9]+") ] dfPeople
  putStrLn "\nseparate_wider_regex(str, c(\"<\", name=\"[A-Za-z]+\", \">-\", gender=\"[A-Z]\", \"-\", age=\"[0-9]+\")):"
  print peopleOut

  -- ===========================================================================
  sect "15.4 パターン詳細 — アンカー / 文字クラス / 量指定子 / グループ"
  putStrLn "■ アンカー ^ $ + 文字クラス + 量指定子 (stringr::words 980 語で実演)"
  putStrLn $ "words count = " ++ show (length words')
  putStrLn $ "\nstr_subset(words, \"^y\")  (y で始まる) = " ++ show (strSubset "^y" words')
  putStrLn $ "str_subset(words, \"x$\")  (x で終わる) = " ++ show (strSubset "x$" words')
  putStrLn $ "str_subset(words, \"^...$\") (ちょうど 3 文字) = "
           ++ show (length (strSubset "^...$" words')) ++ " 語・例 "
           ++ show (take 8 (strSubset "^...$" words'))
  putStrLn $ "str_detect で 7 文字以上 (\"[a-z]{7,}\") の語数 = "
           ++ show (length (strSubset "[a-z]{7,}" words'))
  -- \d \b の実演
  putStrLn $ "\n■ \\d (= [[:digit:]] に内部変換) / \\b 単語境界:"
  putStrLn $ "str_detect(\"abc123\", \"\\\\d\")          = " ++ show (strDetect "\\d" "abc123")
  putStrLn $ "str_extract(\"order 42 ok\", \"\\\\d+\")    = " ++ show (strExtract "\\d+" "order 42 ok")
  putStrLn $ "str_detect(\"the cat sat\", \"\\\\bcat\\\\b\") = " ++ show (strDetect "\\bcat\\b" "the cat sat")
  putStrLn $ "str_detect(\"category\",    \"\\\\bcat\\\\b\") = " ++ show (strDetect "\\bcat\\b" "category")
  -- グループ + str_match
  putStrLn $ "\n■ グループ () + str_match:"
  putStrLn $ "str_match(\"2026-06-19\", \"(\\\\d{4})-(\\\\d{2})-(\\\\d{2})\") = "
           ++ show (strMatch "(\\d{4})-(\\d{2})-(\\d{2})" "2026-06-19")
  putStrLn "★後方参照 (.)\\1 (POSIX 非対応) は概念のみ注記 (README §15.4 参照)。"

  -- ===========================================================================
  sect "15.5 パターン制御 — ignore_case / fixed / coll"
  putStrLn $ "str_detect(\"Banana\", \"banana\")            = " ++ show (strDetect "banana" "Banana")
  putStrLn $ "str_detect(\"Banana\", regex(\"banana\", ignore_case=TRUE)) = "
           ++ show (strDetectWith True "banana" "Banana")
  putStrLn "(fixed = リテラル一致・coll = locale 照合。本リポジトリは coll 未採用 → README で注記)"

  -- ===========================================================================
  sect "15.6 実践 — パターンをコードで作る (str_escape / str_flatten)"
  putStrLn $ "str_escape(\"a.b+c\")  (メタ文字をエスケープ) = " ++ T.unpack (strEscape "a.b+c")
  -- words: x で始まる or 終わる (単一 regex vs 複数 str_detect)
  let startEndX = strSubset "^x|x$" words'
  putStrLn $ "\nstr_subset(words, \"^x|x$\")  (x で始まる or 終わる) = " ++ show startEndX
  putStrLn $ "(複数 str_detect 版: filter str_detect \"^x\" OR \"x$\" = "
           ++ show [ w | w <- words', strDetect "^x" w || strDetect "x$" w ] ++ ")"
  -- コードでパターン生成: str_c で選択肢を | で連結
  let fruitsToFind = ["apple","banana","pear"] :: [Text]
      builtPat = strFlatten "|" (map strEscape fruitsToFind)
  putStrLn $ "\nコードで生成したパターン str_flatten(\"|\", str_escape(...)) = " ++ T.unpack builtPat
  putStrLn $ "str_subset(c(\"apple pie\",\"grape\",\"pear tart\"), 上記) = "
           ++ show (strSubset builtPat ["apple pie","grape","pear tart"])

  -- ===========================================================================
  sect "15.7 正規表現が使える他の場所"
  putStrLn "・separate_wider_regex (§15.3.4 で実演済) は tidyr の regex 利用箇所。"
  putStrLn "・R の matches() / pivot_longer(names_pattern=) / list.files(pattern=) は"
  putStrLn "  「列名・ファイル名を regex で選ぶ」用途。本リポジトリでは列名 filter を"
  putStrLn "  str_detect で書ける (概念は README に注記)。"

  putStrLn "\n--- 完了 (全出力は実データ・R4DS Ch15 と突合済) ---"

-- | 重複除去 (出現順保持)。
dedup :: Ord a => [a] -> [a]
dedup = go M.empty
  where go _ [] = []
        go seen (x:xs) | x `M.member` seen = go seen xs
                       | otherwise         = x : go (M.insert x () seen) xs

-- | ignore_case な str_count (母音カウントは大小無視で数える)。
strCountCI :: Text -> Text -> Int
strCountCI pat = strCount pat . T.toLower
