-- | チュートリアル 12: 論理ベクトル (R4DS 2e Ch12 "Logical vectors")
--   https://r4ds.hadley.nz/logicals
--
--   論理ベクトルは最も単純な型 (各要素は TRUE / FALSE / NA の 3 値のみ) だが、
--   ほぼすべての解析で作成・操作する。本章では
--   ・数値比較 (< <= > >= != ==) による作成と浮動小数点の落とし穴 (near)
--   ・欠損値の「伝染」と is.na()
--   ・ブール代数 (& | ! xor)・演算順序の罠・%in%
--   ・要約 (any/all・sum/mean・論理サブセット)
--   ・条件変換 (if_else / case_when)
--   を学ぶ。この章は表とベクトル出力が主役で、R4DS が描く図は唯一ブール演算の
--   ベン図 (概念イラスト・echo:false) のみ。実データの ggplot 図は 0 枚ゆえ、
--   概念は散文で説明し、全ベクトル/全表出力を実データ (nycflights13 flights 全量)
--   で忠実再現する。R の near / %in% / if_else / case_when / any / all / is.na 等は
--   Haskell に直接対応が無いものを本ファイル内に小ヘルパとして自前実装する
--   (CLAUDE.md「機能不足は実装で埋める」)。
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Main (main) where

import           Control.DeepSeq           (force, NFData)
import           Control.Exception         (try, evaluate, SomeException)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.Maybe                (fromMaybe, mapMaybe, isNothing)
import           Data.List                 (sortBy)
import           Data.Ord                  (comparing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as M
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import qualified DataFrame.IO.CSV                     as DF
import qualified DataFrame.Internal.Column            as DF
import qualified DataFrame.Internal.DataFrame         as DF
import qualified DataFrame.Operators                  as DF
import qualified DataFrame.Operations.Core            as DF
import qualified DataFrame.Operations.Subset          as DF
import qualified DataFrame.Internal.Column as DFC
import           DataFrame.Operations.Aggregation (selectIndices)

-- === 列抽出 (Joins.hs と同方式・型不一致は [] / Nothing に倒れる安全版) ==========

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

colMaybe :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [Maybe a]
colMaybe n df = fromMaybe [] (safeCol @(Maybe a) n df)

-- === 3 値論理 (TRUE / FALSE / NA = Maybe Bool) ================================
--   NA は「TRUE か FALSE か不明」を表す。論理演算は不明でも結果が確定する場合のみ
--   確定値を返す (Kleene の 3 値論理)。R の & | の挙動はこれに一致する。

-- | and: どちらかが FALSE なら結果は確定で FALSE、両方 TRUE なら TRUE、他は NA。
andK :: Maybe Bool -> Maybe Bool -> Maybe Bool
andK (Just False) _            = Just False
andK _            (Just False) = Just False
andK (Just True)  (Just True)  = Just True
andK _            _            = Nothing

-- | or: どちらかが TRUE なら結果は確定で TRUE、両方 FALSE なら FALSE、他は NA。
orK :: Maybe Bool -> Maybe Bool -> Maybe Bool
orK (Just True)  _            = Just True
orK _            (Just True)  = Just True
orK (Just False) (Just False) = Just False
orK _            _            = Nothing

-- | xor (R の xor): 両方確定なら排他的論理和、片方でも NA なら NA。
xorK :: Maybe Bool -> Maybe Bool -> Maybe Bool
xorK (Just a) (Just b) = Just (a /= b)
xorK _        _        = Nothing

-- | 比較で論理ベクトルを作る: 値が NA (Nothing) なら結果も NA。
cmpM :: (a -> Bool) -> Maybe a -> Maybe Bool
cmpM = fmap

-- | TRUE/FALSE/NA の R 風表示。
showB :: Maybe Bool -> Text
showB (Just True)  = "TRUE"
showB (Just False) = "FALSE"
showB Nothing      = "NA"

-- | 値付き NA を R 風に表示。
showM :: Show a => Maybe a -> String
showM = maybe "NA" show

-- === R の関数群を自前実装 ====================================================

-- | dplyr::near() : 既定許容差 sqrt(.Machine$double.eps) ≈ 1.49e-8 で近似一致。
near :: Double -> Double -> Bool
near a b = abs (a - b) < tol
  where tol = sqrt (2.220446049250313e-16)  -- = .Machine$double.eps の平方根

-- | %in% : x の各要素が集合 ys に含まれるか。NA も値として比較される
--   (NA %in% NA が TRUE = == と異なる挙動。Eq (Maybe a) がそれを与える)。
inSet :: Eq a => [a] -> a -> Bool
inSet ys x = x `elem` ys

-- | if_else(): 条件 (Maybe Bool) で true/false を選ぶ。NA は missing を返す。
ifElseE :: Maybe Bool -> a -> a -> Maybe a -> Maybe a
ifElseE (Just True)  t _ _    = Just t
ifElseE (Just False) _ f _    = Just f
ifElseE Nothing      _ _ miss = miss

-- | case_when(): (条件, 出力) の対を上から評価し最初に TRUE の出力を返す。
--   NA 条件は「一致しない」扱い。どれも一致しなければ .default。
caseWhen :: [(Maybe Bool, a)] -> Maybe a -> Maybe a
caseWhen []                 def = def
caseWhen ((Just True , v):_) _  = Just v
caseWhen (_              :rest) def = caseWhen rest def

-- | any() / all() (na.rm = TRUE 相当)。NA を除いた上で判定。空なら any=False, all=True。
anyNARM :: [Maybe Bool] -> Bool
anyNARM = or  . mapMaybe id

allNARM :: [Maybe Bool] -> Bool
allNARM = and . mapMaybe id

-- | logical を数値とみなした sum / mean (na.rm = TRUE)。
sumLogical :: [Maybe Bool] -> Int
sumLogical = length . filter (== True) . mapMaybe id

meanLogical :: [Maybe Bool] -> Double
meanLogical xs = let ys = mapMaybe id xs
                     n  = length ys
                 in if n == 0 then nan
                    else fromIntegral (length (filter id ys)) / fromIntegral n
  where nan = 0/0

-- | 平均 (na.rm = TRUE)。空なら NaN (R の mean(numeric(0)) = NaN に合わせる)。
meanD :: [Double] -> Double
meanD [] = 0/0
meanD xs = sum xs / fromIntegral (length xs)

-- === 集約 (group_by + summarize 相当・キー昇順 = dplyr と同じ並び) ============

-- | キー列とペイロード列を群にまとめ、キー昇順で返す。
--   群内の値順は逆順になるが、本章の用途 (mean/sum/all/any) は順序非依存ゆえ問題なし。
--   (++) で prepend し O(n) に保つ (flip (++) は 33 万行で二次オーダーになる)。
groupAsc :: Ord k => [k] -> [v] -> [(k, [v])]
groupAsc ks vs = M.toList (M.fromListWith (++) (zip ks (map (:[]) vs)))

-- === 行選択・表示ヘルパ (Joins.hs と同方式) ===================================

rows :: [Int] -> DF.DataFrame -> DF.DataFrame
rows is = selectIndices (VU.fromList is)

dims :: DF.DataFrame -> IO ()
dims df = let (nr, nc) = DF.dimensions df
          in putStrLn $ "# 全 " ++ comma nr ++ " 行 × " ++ show nc ++ " 列\n"

printHead :: DF.DataFrame -> IO ()
printHead df = do
  print (DF.take 10 df)
  dims df

comma :: Int -> String
comma = reverse . go . reverse . show
  where go (a:b:c:d:rest) = a:b:c:',': go (d:rest)
        go xs             = xs

sect :: String -> IO ()
sect s = putStrLn ("\n========== " <> s <> " ==========")

base :: FilePath
base = "../_data/_raw/"

main :: IO ()
main = do
  flights <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "flights.csv")

  -- ==========================================================================
  -- 12.1 はじめに — ダミーベクトルと mutate
  -- ==========================================================================
  sect "12.1 はじめに"
  -- R: x <- c(1, 2, 3, 5, 7, 11, 13); x * 2
  let x0 = [1,2,3,5,7,11,13] :: [Int]
  putStrLn $ "x         = " ++ show x0
  putStrLn $ "x * 2     = " ++ show (map (*2) x0)
  -- R: df <- tibble(x); df |> mutate(y = x * 2)
  let dfXY = DF.fromNamedColumns [ ("x", DF.fromList x0)
                                 , ("y", DF.fromList (map (*2) x0)) ]
  putStrLn "df |> mutate(y = x * 2):"
  print dfXY

  -- ==========================================================================
  -- 12.2 比較 (Comparisons)
  -- ==========================================================================
  sect "12.2 比較"
  let depTime  = colMaybe @Int "dep_time"  flights
      arrDelay = colMaybe @Int "arr_delay" flights
      -- daytime = dep_time > 600 & dep_time < 2000 (3 値論理・NA は伝染)
      daytime  = [ andK (cmpM (>600) d) (cmpM (<2000) d) | d <- depTime ]
      -- approx_ontime = abs(arr_delay) < 20
      approxOT = [ cmpM (\v -> abs v < 20) a | a <- arrDelay ]
      -- filter(daytime & approx_ontime): NA 行は filter が落とす (Just True のみ残す)
      keepFlt  = [ i | (i, dt, ot) <- zip3 [0..] daytime approxOT
                     , andK dt ot == Just True ]
  putStrLn "flights |> filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20):"
  printHead (rows keepFlt flights)

  -- mutate(daytime, approx_ontime, .keep = "used") : 使った列 + 新列のみ
  let usedDF = DF.insertVector "approx_ontime" (V.fromList (map showB approxOT))
             $ DF.insertVector "daytime"       (V.fromList (map showB daytime))
             $ DF.select ["dep_time","arr_delay"] flights
  putStrLn "flights |> mutate(daytime, approx_ontime, .keep = \"used\"):"
  printHead usedDF

  -- ---- 12.2.1 浮動小数点比較 -------------------------------------------------
  sect "12.2.1 浮動小数点比較"
  -- R: x <- c(1/49*49, sqrt(2)^2)
  let xf = [ 1/49*49, sqrt 2 ** 2 ] :: [Double]
  putStrLn $ "x (R 既定 7 桁表示風)  = " ++ unwords (map (showG 7) xf)
  putStrLn $ "x == c(1, 2)          = " ++ unwords (map (T.unpack . showB) (zipWith (\v t -> Just (v == t)) xf [1,2]))
  putStrLn $ "print(x, digits = 16) = " ++ unwords (map show xf)
  putStrLn $ "near(x, c(1, 2))      = " ++ unwords (map (T.unpack . showB) (zipWith (\v t -> Just (near v t)) xf [1,2]))

  -- ---- 12.2.2 欠損値 (比較) --------------------------------------------------
  sect "12.2.2 欠損値 (比較)"
  -- NA は「不明」: ほぼ全演算が NA に伝染する。
  putStrLn $ "NA > 5    = " ++ T.unpack (showB (cmpM (>5) (Nothing :: Maybe Int)))
  putStrLn $ "10 == NA  = " ++ T.unpack (showB (cmpM (==(10::Int)) (Nothing :: Maybe Int)))
  putStrLn $ "NA == NA  = " ++ T.unpack (showB (eqM (Nothing :: Maybe Int) Nothing))
  -- Mary と John の年齢はともに不明 → 同じ年齢かは「不明」
  let ageMary = Nothing :: Maybe Int
      ageJohn = Nothing :: Maybe Int
  putStrLn $ "age_mary == age_john = " ++ T.unpack (showB (eqM ageMary ageJohn))
  -- filter(dep_time == NA): すべて NA になり filter が全行落とす → 0 行
  putStrLn "flights |> filter(dep_time == NA):"
  printHead (rows [] flights)

  -- ---- 12.2.3 is.na() -------------------------------------------------------
  sect "12.2.3 is.na()"
  putStrLn $ "is.na(c(TRUE, NA, FALSE)) = "
             ++ unwords (map (T.unpack . showB . fmap isNothing)
                             [Just (Just True), Just Nothing, Just (Just False)])
             ++ "  -- (論理ベクトルの NA 判定)"
  putStrLn $ "is.na(c(1, NA, 3))        = "
             ++ unwords (map (T.unpack . showB . Just . isNothing)
                             [Just (1::Int), Nothing, Just 3])
  putStrLn $ "is.na(c(\"a\", NA, \"b\"))    = "
             ++ unwords (map (T.unpack . showB . Just . isNothing)
                             [Just ("a"::Text), Nothing, Just "b"])
  -- filter(is.na(dep_time)): dep_time が欠損の行 (欠航便)
  let naDep = [ i | (i, d) <- zip [0..] depTime, isNothing d ]
  putStrLn "flights |> filter(is.na(dep_time)):"
  printHead (rows naDep flights)

  -- arrange: 既定は NA を末尾。arrange(desc(is.na(dep_time)), dep_time) で NA を先頭へ。
  let month = colPlain @Int "month" flights
      day   = colPlain @Int "day"   flights
      jan1  = [ i | (i, m, dd) <- zip3 [0..] month day, m == 1, dd == 1 ]
      -- arrange(dep_time): 値昇順・NA は末尾
      jan1ByDep = sortBy (cmpDepLast depTime) jan1
      -- arrange(desc(is.na(dep_time)), dep_time): NA 先頭 → 値昇順
      jan1NAFirst = sortBy (cmpNAFirst depTime) jan1
  putStrLn "flights |> filter(month==1, day==1) |> arrange(dep_time):"
  printHead (rows jan1ByDep flights)
  putStrLn "flights |> filter(month==1, day==1) |> arrange(desc(is.na(dep_time)), dep_time):"
  printHead (rows jan1NAFirst flights)

  -- ==========================================================================
  -- 12.3 ブール代数 (Boolean algebra)
  -- ==========================================================================
  sect "12.3 ブール代数"
  putStrLn "R の論理演算子: & (and) / | (or) / ! (not) / xor() (排他的論理和)。"
  putStrLn "R4DS 図 12.1 は x・y を 2 円のベン図で示す概念イラスト (実データ図ではない):"
  putStrLn "  x & !y = x から y を除いた部分 / x & y = 交わり / !x & y = y から x を除く"
  putStrLn "  x = x 全体 / xor(x,y) = 交わり以外すべて / y = y 全体 / x | y = 全体。"
  putStrLn "(本ライブラリは統計プロット用で、こうした解説イラストは対象外ゆえ散文で説明する)"
  putStrLn "※ && と || は短絡評価で単一の TRUE/FALSE しか返さない。dplyr の中では使わない。"
  -- 例: !is.na / 範囲外
  let exV = [Just 1, Nothing, Just (-15), Just 5] :: [Maybe Int]
  putStrLn $ "!is.na(x), x=c(1,NA,-15,5)        = "
             ++ unwords (map (T.unpack . showB . Just . not . isNothing) exV)
  putStrLn $ "x < -10 | x > 0                    = "
             ++ unwords (map (T.unpack . showB . (\v -> orK (cmpM (< (-10)) v) (cmpM (>0) v))) exV)
  -- xor: 片方だけ TRUE のとき TRUE (R の xor()。片方でも NA なら NA)
  putStrLn $ "xor(x > 0, x < 3), x=c(1,NA,-15,5) = "
             ++ unwords (map (T.unpack . showB . (\v -> xorK (cmpM (>0) v) (cmpM (<3) v))) exV)

  -- ---- 12.3.1 欠損値 (ブール代数) -------------------------------------------
  sect "12.3.1 欠損値 (ブール代数)"
  let xb = [Just True, Just False, Nothing] :: [Maybe Bool]
      boolDF = DF.fromNamedColumns
        [ ("x",   DF.fromList (map showB xb))
        , ("and", DF.fromList (map (showB . (`andK` Nothing)) xb))  -- x & NA
        , ("or",  DF.fromList (map (showB . (`orK`  Nothing)) xb)) ] -- x | NA
  putStrLn "tibble(x = c(TRUE, FALSE, NA)) |> mutate(and = x & NA, or = x | NA):"
  print boolDF
  putStrLn "理由: NA|TRUE=TRUE (少なくとも片方 TRUE)・NA|FALSE=NA (不明)。"
  putStrLn "      NA&FALSE=FALSE (少なくとも片方 FALSE)・NA&TRUE=NA (不明)。"

  -- ---- 12.3.2 演算順序 ------------------------------------------------------
  sect "12.3.2 演算順序"
  putStrLn "R: filter(month == 11 | 12) は英語の語順に引きずられた誤り。"
  putStrLn "   month == 11 を nov とすると nov | 12 を評価する。数値 12 は (0 以外ゆえ) TRUE に"
  putStrLn "   変換され nov | TRUE = 常に TRUE → 全行が選ばれてしまう。"
  let nov   = [ cmpM (==11) (Just m) | m <- month ]
      final = [ orK n (Just True) | n <- nov ]   -- nov | 12  (12 → TRUE)
      ooDF  = DF.insertVector "final" (V.fromList (map showB final))
            $ DF.insertVector "nov"   (V.fromList (map showB nov))
            $ DF.select ["month"] flights
  putStrLn "flights |> mutate(nov = month == 11, final = nov | 12, .keep = \"used\"):"
  printHead ooDF
  putStrLn $ "全 " ++ comma (length (filter (== Just True) final))
             ++ " 行が final == TRUE (= 全行。正しくは month == 11 | month == 12)"

  -- ---- 12.3.3 %in% ----------------------------------------------------------
  sect "12.3.3 %in%"
  putStrLn $ "1:12 %in% c(1, 5, 11)               = "
             ++ unwords (map (T.unpack . showB . Just . inSet [1,5,11]) ([1..12] :: [Int]))
  putStrLn $ "letters[1:10] %in% c(a,e,i,o,u)     = "
             ++ unwords (map (T.unpack . showB . Just . inSet ["a","e","i","o","u"])
                             (map (T.singleton) ['a'..'j']))
  putStrLn $ "c(1, 2, NA) == NA                    = "
             ++ unwords (map (T.unpack . showB . (`eqM` Nothing)) ([Just 1, Just 2, Nothing] :: [Maybe Int]))
  putStrLn $ "c(1, 2, NA) %in% NA                  = "
             ++ unwords (map (T.unpack . showB . Just . inSet [Nothing]) ([Just 1, Just 2, Nothing] :: [Maybe Int]))
  putStrLn "(== は NA を NA にするが、%in% は NA を 1 つの値として比較し NA %in% NA = TRUE)"
  -- filter(dep_time %in% c(NA, 0800)) : dep_time が NA か 800 の行 (0800 = 800)
  let inDep = [ i | (i, d) <- zip [0..] depTime, inSet [Nothing, Just 800] d ]
  putStrLn "flights |> filter(dep_time %in% c(NA, 0800)):"
  printHead (rows inDep flights)

  -- ==========================================================================
  -- 12.4 要約 (Summaries)
  -- ==========================================================================
  sect "12.4 要約 — any() / all()"
  let depDelay = colMaybe @Int "dep_delay" flights
      year     = colPlain @Int "year"      flights
      ymd      = zip3 year month day
      -- all(dep_delay <= 60, na.rm=T) / any(arr_delay >= 300, na.rm=T) を日ごとに
      grpAll   = groupAsc ymd [ cmpM (<=60)  v | v <- depDelay ]
      grpAny   = groupAsc ymd [ cmpM (>=300) v | v <- arrDelay ]
      allDelayed   = [ allNARM vs | (_, vs) <- grpAll ]
      anyLongDelay = [ anyNARM vs | (_, vs) <- grpAny ]
      keysAll      = map fst grpAll
      sumDF = DF.fromNamedColumns
        [ ("year",  DF.fromList [ y | (y,_,_) <- keysAll ])
        , ("month", DF.fromList [ m | (_,m,_) <- keysAll ])
        , ("day",   DF.fromList [ d | (_,_,d) <- keysAll ])
        , ("all_delayed",    DF.fromList (map (showB . Just) allDelayed))
        , ("any_long_delay", DF.fromList (map (showB . Just) anyLongDelay)) ]
  putStrLn "flights |> group_by(year, month, day) |> summarize("
  putStrLn "  all_delayed = all(dep_delay <= 60, na.rm=T),"
  putStrLn "  any_long_delay = any(arr_delay >= 300, na.rm=T)):"
  printHead sumDF

  sect "12.4.1 論理ベクトルの数値要約 — sum() / mean()"
  let propDelayed = [ meanLogical vs | (_, vs) <- grpAll ]   -- mean(dep_delay <= 60)
      countLong   = [ sumLogical  vs | (_, vs) <- grpAny ]   -- sum(arr_delay >= 300)
      sumDF2 = DF.fromNamedColumns
        [ ("year",  DF.fromList [ y | (y,_,_) <- keysAll ])
        , ("month", DF.fromList [ m | (_,m,_) <- keysAll ])
        , ("day",   DF.fromList [ d | (_,_,d) <- keysAll ])
        , ("proportion_delayed", DF.fromList propDelayed)
        , ("count_long_delay",   DF.fromList countLong) ]
  putStrLn "flights |> group_by(year, month, day) |> summarize("
  putStrLn "  proportion_delayed = mean(dep_delay <= 60, na.rm=T),"
  putStrLn "  count_long_delay = sum(arr_delay >= 300, na.rm=T)):"
  printHead sumDF2

  sect "12.4.2 論理サブセット — x[条件]"
  -- (a) filter(arr_delay > 0) してから日ごとに mean(arr_delay), n()
  let posIdx = [ (k, v) | (k, Just v) <- zip ymd arrDelay, v > 0 ]
      grpPos = M.toList (M.fromListWith (++) [ (k, [v]) | (k, v) <- posIdx ])
      behindDF = DF.fromNamedColumns
        [ ("year",   DF.fromList [ y | ((y,_,_),_) <- grpPos ])
        , ("month",  DF.fromList [ m | ((_,m,_),_) <- grpPos ])
        , ("day",    DF.fromList [ d | ((_,_,d),_) <- grpPos ])
        , ("behind", DF.fromList [ meanD (map fromIntegral vs) | (_, vs) <- grpPos ])
        , ("n",      DF.fromList [ length vs | (_, vs) <- grpPos ]) ]
  putStrLn "flights |> filter(arr_delay > 0) |> group_by(...) |> summarize(behind = mean(arr_delay), n = n()):"
  printHead behindDF
  -- (b) 群はそのまま (全便) で、列内サブセット arr_delay[arr_delay>0] / [<0]
  let grpArr = groupAsc ymd arrDelay
      behind2DF = DF.fromNamedColumns
        [ ("year",   DF.fromList [ y | ((y,_,_),_) <- grpArr ])
        , ("month",  DF.fromList [ m | ((_,m,_),_) <- grpArr ])
        , ("day",    DF.fromList [ d | ((_,_,d),_) <- grpArr ])
        , ("behind", DF.fromList [ meanD [ fromIntegral v | Just v <- vs, v > 0 ] | (_, vs) <- grpArr ])
        , ("ahead",  DF.fromList [ meanD [ fromIntegral v | Just v <- vs, v < 0 ] | (_, vs) <- grpArr ])
        , ("n",      DF.fromList [ length vs | (_, vs) <- grpArr ]) ]
  putStrLn "summarize(behind = mean(arr_delay[arr_delay>0], na.rm=T), ahead = mean(arr_delay[arr_delay<0], na.rm=T), n = n()):"
  printHead behind2DF
  putStrLn "(注: (a) の n() は遅延便のみの数、(b) の n() は全便数。群の大きさが異なる)"

  -- ==========================================================================
  -- 12.5 条件変換 (Conditional transformations)
  -- ==========================================================================
  sect "12.5 条件変換 — if_else()"
  let xc = [Just (-3), Just (-2), Just (-1), Just 0, Just 1, Just 2, Just 3, Nothing] :: [Maybe Int]
  putStrLn $ "x = c(-3:3, NA)"
  putStrLn $ "if_else(x > 0, \"+ve\", \"-ve\")        = "
             ++ unwords [ showM (ifElseE (cmpM (>0) v) ("+ve"::String) "-ve" Nothing) | v <- xc ]
  putStrLn $ "if_else(x > 0, \"+ve\", \"-ve\", \"???\") = "
             ++ unwords [ showM (ifElseE (cmpM (>0) v) ("+ve"::String) "-ve" (Just "???")) | v <- xc ]
  putStrLn $ "if_else(x < 0, -x, x)  (= abs)      = "
             ++ unwords [ showM (ifElseE (cmpM (<0) v) (fmap negate v) v Nothing >>= id) | v <- xc ]
  -- coalesce 風
  let x1 = [Nothing, Just 1, Just 2, Nothing] :: [Maybe Int]
      y1 = [Just 3, Nothing, Just 4, Just 6]  :: [Maybe Int]
  putStrLn $ "if_else(is.na(x1), y1, x1)          = "
             ++ unwords [ showM (ifElseE (Just (isNothing a)) b a Nothing >>= id)
                        | (a, b) <- zip x1 y1 ]
  -- 入れ子 if_else
  putStrLn $ "if_else(x==0,\"0\",if_else(x<0,\"-ve\",\"+ve\"),\"???\") = "
             ++ unwords [ showM (ifElseE (eqM v (Just 0)) "0"
                                   (fromMaybe "?" (ifElseE (cmpM (<0) v) "-ve" "+ve" Nothing))
                                   (Just "???"))
                        | v <- xc ]

  sect "12.5.1 case_when()"
  putStrLn $ "case_when(x==0~\"0\", x<0~\"-ve\", x>0~\"+ve\", is.na(x)~\"???\"):"
  putStrLn $ "  = " ++ unwords
    [ fromMaybe "NA" (caseWhen [ (eqM v (Just 0), "0")
                               , (cmpM (<0) v,    "-ve")
                               , (cmpM (>0) v,    "+ve")
                               , (Just (isNothing v), "???") ] Nothing)
    | v <- xc ]
  putStrLn $ "case_when(x<0~\"-ve\", x>0~\"+ve\")  (どれも一致しない要素は NA):"
  putStrLn $ "  = " ++ unwords
    [ fromMaybe "NA" (caseWhen [ (cmpM (<0) v, "-ve"), (cmpM (>0) v, "+ve") ] Nothing) | v <- xc ]
  putStrLn $ "case_when(x<0~\"-ve\", x>0~\"+ve\", .default=\"???\"):"
  putStrLn $ "  = " ++ unwords
    [ fromMaybe "NA" (caseWhen [ (cmpM (<0) v, "-ve"), (cmpM (>0) v, "+ve") ] (Just "???")) | v <- xc ]
  putStrLn $ "case_when(x>0~\"+ve\", x>2~\"big\")  (複数一致は最初のみ):"
  putStrLn $ "  = " ++ unwords
    [ fromMaybe "NA" (caseWhen [ (cmpM (>0) v, "+ve"), (cmpM (>2) v, "big") ] Nothing) | v <- xc ]

  -- flights の status ラベル (case_when・.keep="used")
  let status a = caseWhen
        [ (Just (isNothing a),          "cancelled")
        , (cmpM (< (-30)) a,            "very early")
        , (cmpM (< (-15)) a,            "early")
        , (cmpM (\v -> abs v <= 15) a,  "on time")
        , (cmpM (< 60) a,              "late")
        , (cmpM (const True) a,        "very late") ] Nothing
      statusDF = DF.insertVector "status"
                   (V.fromList [ fromMaybe "NA" (status a) :: Text | a <- arrDelay ])
               $ DF.select ["arr_delay"] flights
  putStrLn "flights |> mutate(status = case_when(... arr_delay ラベル ...), .keep = \"used\"):"
  printHead statusDF

  sect "12.5.2 互換な型 (Compatible types)"
  putStrLn "R では if_else() / case_when() の出力は互換な型でなければならない。"
  putStrLn "  if_else(TRUE, \"a\", 1)               → エラー (文字列と数値は非互換)"
  putStrLn "  case_when(x < -1 ~ TRUE, x > 0 ~ now()) → エラー (論理と日時は非互換)"
  putStrLn "互換な主な組合せ: 数値と論理 / 文字列と factor / 日付と日時 / NA は全型と互換。"
  putStrLn "(Haskell は静的型ゆえこれらは“実行時エラー”でなく“コンパイルエラー”になる。"
  putStrLn " 上記 if_else ヘルパも true/false が同型である必要があり、型システムが保証する)"

  putStrLn "\n(以上で R4DS Ch12 の全節を再現。図は概念ベン図のみゆえ散文化・出力表は実データ全量)"

-- === 比較・並び替えの補助 ====================================================

-- | Maybe 同士の == (NA があれば NA = R の == 挙動)。
eqM :: Eq a => Maybe a -> Maybe a -> Maybe Bool
eqM (Just a) (Just b) = Just (a == b)
eqM _        _        = Nothing

-- | R 既定の有効桁数風表示。digits 桁に丸め、結果が整数なら整数表示する
--   (digits=7 で 1/49*49 → "1"、digits=16 で完全精度 "0.9999999999999999")。
showG :: Int -> Double -> String
showG digits v =
  let r = sigDigits digits v
  in if r == fromIntegral (round r :: Integer)
       then show (round r :: Integer)
       else show r

-- | v を有効 digits 桁に丸める。
sigDigits :: Int -> Double -> Double
sigDigits digits v
  | v == 0    = 0
  | otherwise = let e = floor (logBase 10 (abs v)) :: Int
                    f = 10 ^^ (digits - 1 - e)
                in fromIntegral (round (v * f) :: Integer) / f

-- | arrange(dep_time): 値昇順・NA は末尾。
cmpDepLast :: [Maybe Int] -> Int -> Int -> Ordering
cmpDepLast col i j = naLast (col !! i) (col !! j)
  where naLast (Just a) (Just b) = compare a b
        naLast (Just _) Nothing  = LT
        naLast Nothing  (Just _) = GT
        naLast Nothing  Nothing  = EQ

-- | arrange(desc(is.na(dep_time)), dep_time): NA を先頭・その後値昇順。
cmpNAFirst :: [Maybe Int] -> Int -> Int -> Ordering
cmpNAFirst col i j =
  case (col !! i, col !! j) of
    (Nothing, Nothing) -> EQ
    (Nothing, Just _)  -> LT
    (Just _,  Nothing) -> GT
    (Just a,  Just b)  -> compare a b
