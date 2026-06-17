-- | チュートリアル 13: 数値ベクトル (R4DS 2e Ch13 "Numbers")
--   https://r4ds.hadley.nz/numbers
--
--   数値ベクトルでできることを体系的に概観する。
--   ・文字列→数値 (parse_number) / count() と加重・欠損カウント
--   ・数値変換: リサイクル規則・pmin/pmax・剰余算 (%/% %%)・対数・丸め (Banker's)・
--     cut (区間化)・累積 (cumsum)
--   ・汎用変換: 順位 (min_rank 等)・オフセット (lag/lead)・連続識別子
--   ・数値要約: 中心 (mean/median)・分位点・分散 (sd/IQR)・分布 (図)・位置 (first/last/nth)
--
--   ★高レベル API 既定 ([[feedback-use-high-level-api]]): 記述統計は analyze の
--   `Stat.Descriptive`、 dplyr 動詞は `Data.Transform`、 summarise/mutate/groupBy は
--   `Data.Wrangle` を使う (Phase 65/66/67)。 図は plot の Easy DSL。
--
--   実データ図 4 枚: ① キャンセル率 vs 出発時刻 (line+点サイズ) / ② 日次 mean vs
--   median 散布 (+45°線) / ③ dep_delay ヒストグラム 2 枚 (full / <120 拡大) /
--   ④ 365 日の頻度ポリゴン重畳 (単色 alpha)。
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Main (main) where

import           Control.DeepSeq           (force, NFData)
import           Control.Exception         (try, evaluate, SomeException)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.Char                 (isDigit)
import           Data.Maybe                (fromMaybe, mapMaybe, catMaybes, isNothing)
import           Data.List                 (sortBy, sortOn, nub)
import           Data.Ord                  (Down (..), comparing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import qualified DataFrame                 as DF
import qualified DataFrame.Internal.Column as DFC
import qualified DataFrame.Functions       as F
import           DataFrame.Operators       ((|>), (.<))
import           DataFrame.Operations.Aggregation (selectIndices)
-- analyze 高レベル API (Phase 65/66/67)
import qualified Hanalyze.Stat.Descriptive as D
import qualified Hanalyze.Data.Transform   as Tr
import           Hanalyze.Data.Wrangle
-- plot
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame        ((|>>), toResolver)
import           Hgg.Plot.Backend.SVG  (saveSVGBound, saveSVG)
import           Hgg.Plot.DataFrame    ()

-- === 列抽出 (Joins.hs / Logicals.hs と同方式) =================================

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

colMaybe :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [Maybe a]
colMaybe n df = fromMaybe [] (safeCol @(Maybe a) n df)

-- === 表示ヘルパ ===============================================================

sect :: String -> IO ()
sect s = putStrLn ("\n========== " <> s <> " ==========")

dims :: DF.DataFrame -> IO ()
dims df = let (nr, nc) = DF.dimensions df
          in putStrLn $ "# 全 " ++ comma nr ++ " 行 × " ++ show nc ++ " 列\n"

printHead :: DF.DataFrame -> IO ()
printHead df = print (DF.take 10 df) >> dims df

comma :: Int -> String
comma = reverse . go . reverse . show
  where go (a:b:c:d:rest) = a:b:c:',': go (d:rest)
        go xs             = xs

base :: FilePath
base = "../_data/_raw/"

-- === R 関数の小物 (tutorial local・統計/変換は analyze API へ委譲) =============

-- | parse_double: 数値文字列をそのまま読む (科学表記 "1e3"→1000 も)。
parseDouble :: Text -> Double
parseDouble t = read (T.unpack t) :: Double

-- | parse_number 風: 文字列から数値部分のみ取り出す ("$1,234"→1234・"59%"→59)。
parseNumber :: Text -> Double
parseNumber t = read (keep (T.unpack t)) :: Double
  where keep s = case filter (\c -> isDigit c || c == '.' || c == '-') s of
                   "" -> "0"; xs -> xs

-- | 指定桁での丸め (R round(x, digits)・Banker's は Haskell round が既に半偶数)。
roundTo :: Int -> Double -> Double
roundTo d x = fromIntegral (round (x * f) :: Integer) / f
  where f = 10 ^^ d :: Double

-- | 行ごとの最小/最大 (R pmin/pmax・na.rm=TRUE)。
pmin', pmax' :: [Maybe Double] -> [Maybe Double] -> [Maybe Double]
pmin' = zipWith (rowOp min)
pmax' = zipWith (rowOp max)

rowOp :: (Double -> Double -> Double) -> Maybe Double -> Maybe Double -> Maybe Double
rowOp f (Just a) (Just b) = Just (f a b)
rowOp _ (Just a) Nothing  = Just a
rowOp _ Nothing  (Just b) = Just b
rowOp _ Nothing  Nothing  = Nothing

showM :: Show a => Maybe a -> String
showM = maybe "NA" show

main :: IO ()
main = do
  flights <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "flights.csv")

  -- ==========================================================================
  -- 13.1 数を作る (Making numbers)
  -- ==========================================================================
  sect "13.1 数を作る — parse_number"
  -- R: parse_double(c("1.2","5.6","1e3")) = 1.2 5.6 1000
  putStrLn $ "parse_double(c(\"1.2\",\"5.6\",\"1e3\")) = "
           ++ unwords (map (show . parseDouble) ["1.2","5.6","1e3"])
  -- R: parse_number(c("$1,234","USD 3,513","59%")) = 1234 3513 59
  putStrLn $ "parse_number(c(\"$1,234\",\"USD 3,513\",\"59%\")) = "
           ++ unwords (map (show . parseNumber) ["$1,234","USD 3,513","59%"])

  -- ==========================================================================
  -- 13.2 カウント (Counts)
  -- ==========================================================================
  sect "13.2 カウント — count() / n_distinct / 加重 / 欠損"
  -- R: flights |> count(dest)   (高レベル: groupBy + nOf)
  let cntDest = flights |> groupBy ["dest"] |> summarise [ "n" =: nOf ]
  putStrLn "flights |> count(dest):"
  printHead cntDest
  -- R: flights |> count(dest, sort = TRUE)  (n 降順)
  putStrLn "flights |> count(dest, sort = TRUE): (上位 10)"
  printHead (sortByCol "n" cntDest)
  -- R: group_by(dest) |> summarize(n=n(), delay=mean(arr_delay, na.rm=TRUE))
  let destDelay = flights |> groupBy ["dest"]
                          |> summarise [ "n" =: nOf, "delay" =: meanOf "arr_delay" ]
  putStrLn "flights |> group_by(dest) |> summarize(n, delay = mean(arr_delay, na.rm=T)):"
  printHead destDelay
  -- R: n_distinct(carrier) by dest (= 何社が就航するか)。 carrier は Text ゆえ
  --    Wrangle v1 の nDistinctOf (数値専用) では数えられない → Set で手計算。
  let carrierV = colPlain @Text "carrier" flights
      destAll  = colPlain @Text "dest"    flights
      cbyd     = M.toList (M.fromListWith S.union [ (d, S.singleton c) | (d, c) <- zip destAll carrierV ])
      cbydSort = sortOn (Down . snd) [ (d, S.size s) | (d, s) <- cbyd ]
      carriersDF = DF.fromNamedColumns
                     [ ("dest",     DF.fromList (map fst cbydSort))
                     , ("carriers", DF.fromList (map snd cbydSort)) ]
  putStrLn "flights |> group_by(dest) |> summarize(carriers = n_distinct(carrier)) |> arrange(desc):"
  printHead carriersDF
  -- R: 加重カウント = sum(distance) by tailnum
  let milesByTail = flights |> groupBy ["tailnum"] |> summarise [ "miles" =: sumOf "distance" ]
  putStrLn "flights |> group_by(tailnum) |> summarize(miles = sum(distance)):"
  printHead milesByTail
  -- R: 欠損カウント = sum(is.na(dep_time)) by dest (= キャンセル便数)
  let depTime = colMaybe @Int "dep_time" flights
      destsV  = colPlain @Text "dest" flights
      cancByDest = M.toAscList (M.fromListWith (+)
                     [ (d, if isNothing dt then 1 else 0 :: Int)
                     | (d, dt) <- zip destsV depTime ])
      cancDF = DF.fromNamedColumns
                 [ ("dest",        DF.fromList (map fst cancByDest))
                 , ("n_cancelled", DF.fromList (map snd cancByDest)) ]
  putStrLn "flights |> group_by(dest) |> summarize(n_cancelled = sum(is.na(dep_time))):"
  printHead cancDF

  -- ==========================================================================
  -- 13.3 数値変換 (Numeric transformations)
  -- ==========================================================================
  sect "13.3.1 算術とリサイクル規則"
  let xr = [1, 2, 10, 20] :: [Double]
  putStrLn $ "x = c(1,2,10,20); x / 5         = " ++ unwords (map show (map (/5) xr))
  putStrLn $ "x * c(1, 2)  (リサイクル)        = "
           ++ unwords (map show (zipWith (*) xr (cycle [1,2])))
  putStrLn "★ filter(month == c(1,2)) は罠: リサイクルで奇数行=1月/偶数行=2月 だけ拾う"
  let monthV = colPlain @Int "month" flights
      trap   = length [ () | (i, m) <- zip [0 :: Int ..] monthV
                           , m == (if even i then 1 else 2) ]
  putStrLn $ "  flights |> filter(month == c(1,2)) は " ++ comma trap
           ++ " 行 (正しくは month %in% c(1,2))"

  sect "13.3.2 pmin / pmax (行ごと) vs min / max (要約)"
  -- R: tribble(~x,~y, 1,3, 5,2, 7,NA)
  let pxs = [Just 1, Just 5, Just 7]  :: [Maybe Double]
      pys = [Just 3, Just 2, Nothing] :: [Maybe Double]
  putStrLn $ "pmin(x,y,na.rm=T) = " ++ unwords (map showM (pmin' pxs pys))
  putStrLn $ "pmax(x,y,na.rm=T) = " ++ unwords (map showM (pmax' pxs pys))
  putStrLn $ "min(x,y,na.rm=T) = " ++ show (minimum (catMaybes (pxs++pys)))
           ++ " / max = " ++ show (maximum (catMaybes (pxs++pys)))
           ++ "  (= 全体の単一値・pmin/pmax と取り違え注意)"

  sect "13.3.3 剰余算 %/% %%"
  putStrLn $ "1:10 %/% 3 = " ++ unwords (map (show . (`div` 3)) [1..10 :: Int])
  putStrLn $ "1:10 %% 3  = " ++ unwords (map (show . (`mod` 3)) [1..10 :: Int])
  -- sched_dep_time を hour/minute に分解 (%/% 100, %% 100)
  let schedV = colPlain @Int "sched_dep_time" flights
      hourMin = DF.insertVector "minute" (V.fromList (map (`mod` 100) schedV))
              $ DF.insertVector "hour"   (V.fromList (map (`div` 100) schedV))
              $ DF.select ["sched_dep_time"] flights
  putStrLn "flights |> mutate(hour = sched_dep_time %/% 100, minute = sched_dep_time %% 100):"
  printHead hourMin

  -- ---- 図① キャンセル率 vs 出発時刻 -----------------------------------------
  -- R: group_by(hour) |> summarize(prop_cancelled = mean(is.na(dep_time)), n) |> filter(hour>1)
  let hourOf   = map (`div` 100) schedV
      grpHour  = M.toAscList (M.fromListWith (\(a,b) (c,d) -> (a+c, b+d))
                   [ (h, (if isNothing dt then 1 else 0 :: Int, 1 :: Int))
                   | (h, dt) <- zip hourOf depTime ])
      cancRows  = [ (fromIntegral h, fromIntegral nc / fromIntegral n, fromIntegral n)
                  | (h, (nc, n)) <- grpHour, h > 1 ]
                  :: [(Double, Double, Double)]
      cancViz = DF.fromNamedColumns
                  [ ("hour",           DF.fromList [ a | (a,_,_) <- cancRows ])
                  , ("prop_cancelled", DF.fromList [ b | (_,b,_) <- cancRows ])
                  , ("n",              DF.fromList [ c | (_,_,c) <- cancRows ]) ]
  saveSVGBound "fig1-prop-cancelled.svg" $
    cancViz |>> layer (line "hour" "prop_cancelled" <> colorStatic "#808080")
            <> layer (scatter "hour" "prop_cancelled" <> sizeBy "n")
  putStrLn "[図] fig1-prop-cancelled.svg を生成 (キャンセル率は 19 時頃まで増加・点サイズ=便数)"

  sect "13.3.4 丸め (Banker's rounding)"
  putStrLn $ "round(123.456)       = " ++ show (round (123.456 :: Double) :: Integer)
  putStrLn $ "round(123.456, 2)    = " ++ show (roundTo 2    123.456)
  putStrLn $ "round(123.456, 1)    = " ++ show (roundTo 1    123.456)
  putStrLn $ "round(123.456, -1)   = " ++ show (roundTo (-1) 123.456)
  putStrLn $ "round(123.456, -2)   = " ++ show (roundTo (-2) 123.456)
  putStrLn $ "round(c(1.5, 2.5))   = " ++ unwords (map (show . (\v -> round v :: Integer)) [1.5, 2.5 :: Double])
           ++ "  (= 2 2・半偶数丸め)"
  putStrLn $ "floor(123.456) = " ++ show (floor (123.456 :: Double) :: Integer)
           ++ " / ceiling = " ++ show (ceiling (123.456 :: Double) :: Integer)

  sect "13.3.5 区間化 cut"
  -- R: cut(c(1,2,5,10,15,20), breaks=c(0,5,10,15,20))
  let cx = [1,2,5,10,15,20] :: [Double]
  putStrLn $ "cut(x, breaks=c(0,5,10,15,20)) bin = "
           ++ unwords (map showM (Tr.cut [0,5,10,15,20] cx))
  putStrLn $ "ラベル付き (sm/md/lg/xl)        = "
           ++ unwords (map (maybe "NA" T.unpack) (Tr.cutLabels ["sm","md","lg","xl"] [0,5,10,15,20] cx))
  putStrLn $ "範囲外は NA: cut(c(NA→略,-10,5,10,30)) = "
           ++ unwords (map showM (Tr.cut [0,5,10,15,20] [-10,5,10,30]))

  sect "13.3.6 累積 cumsum"
  putStrLn $ "cumsum(1:10) = " ++ unwords (map show (Tr.cumsum [1..10 :: Int]))

  -- ==========================================================================
  -- 13.4 汎用変換 (General transformations)
  -- ==========================================================================
  sect "13.4.1 順位 (ranks)"
  let xrk = [Just 1, Just 5, Just 5, Just 17, Just 22, Nothing] :: [Maybe Int]
  putStrLn $ "x = c(1,5,5,17,22,NA)"
  putStrLn $ "min_rank(x)       = " ++ unwords (map showM (Tr.minRankNA xrk))
  putStrLn $ "min_rank(desc(x)) = " ++ unwords (map showM (Tr.minRankNA (map (fmap Down) xrk)))
  putStrLn $ "row_number(x)     = " ++ unwords (map showM (Tr.rowNumberNA xrk))
  putStrLn $ "dense_rank(x)     = " ++ unwords (map showM (Tr.denseRankNA xrk))
  putStrLn $ "percent_rank(x)   = " ++ unwords (map showM (Tr.percentRankNA xrk))
  putStrLn $ "cume_dist(x)      = " ++ unwords (map showM (Tr.cumeDistNA xrk))
  -- row_number() + %% / %/% で群分割
  let ids = [1..10] :: [Int]
      r0  = map (subtract 1) ids
  putStrLn $ "row0 = row_number()-1            = " ++ unwords (map show r0)
  putStrLn $ "three_groups = row0 %% 3         = " ++ unwords (map (show . (`mod` 3)) r0)
  putStrLn $ "three_in_each_group = row0 %/% 3 = " ++ unwords (map (show . (`div` 3)) r0)

  sect "13.4.2 オフセット lag / lead"
  let xo = [2, 5, 11, 11, 19, 35] :: [Int]
  putStrLn $ "x        = " ++ unwords (map show xo)
  putStrLn $ "lag(x)   = " ++ unwords (map showM (Tr.lag 1 Nothing (map Just xo)))
  putStrLn $ "lead(x)  = " ++ unwords (map showM (Tr.lead 1 Nothing (map Just xo)))
  putStrLn $ "x - lag(x) = " ++ unwords (map showM (zipWith (\a b -> (-) <$> Just a <*> b) xo (Tr.lag 1 Nothing (map Just xo))))

  sect "13.4.3 連続識別子 consecutive_id"
  let cxs = ["a","a","a","b","c","c","d","e","a","a","b","b"] :: [String]
  putStrLn $ "x            = " ++ unwords cxs
  putStrLn $ "consecutive_id = " ++ unwords (map show (Tr.consecutiveId cxs))

  -- ==========================================================================
  -- 13.5 数値要約 (Numeric summaries)
  -- ==========================================================================
  sect "13.5.1 中心 — mean vs median"
  let dayDelay = flights |> groupBy ["year","month","day"]
                         |> summarise [ "mean"   =: meanOf "dep_delay"
                                      , "median" =: medianOf "dep_delay"
                                      , "n"      =: nOf ]
  putStrLn "flights |> group_by(year,month,day) |> summarize(mean, median):"
  printHead dayDelay
  -- ---- 図② mean vs median 散布 (+45°線) -------------------------------------
  -- R4DS の geom_abline(slope=1,intercept=0) = y=x 参照線。 plot の公開 API
  -- `refIdentity` (= refLine RefIdentity) でそのまま描ける (api-guide 03-decoration の参照線)。
  saveSVGBound "fig2-mean-vs-median.svg" $
    dayDelay |>> layer (scatter "mean" "median") <> refIdentity
  putStrLn "[図] fig2-mean-vs-median.svg を生成 (点は対角線 y=x の下 = median < mean・右に歪んだ遅延)"

  sect "13.5.2 最小・最大・分位点"
  -- R: group_by(day) summarize(max, q95 = quantile(dep_delay, 0.95))
  let dayMaxQ = flights |> groupBy ["year","month","day"]
                        |> summarise [ "max" =: maxOf "dep_delay"
                                     , "q95" =: quantileOf 0.95 "dep_delay" ]
  putStrLn "flights |> group_by(year,month,day) |> summarize(max, q95 = quantile(dep_delay, 0.95)):"
  printHead dayMaxQ

  sect "13.5.3 散布 — sd / IQR (EGE の異常)"
  -- R: group_by(origin,dest) summarize(distance_iqr = IQR(distance), n) |> filter(iqr>0)
  let originV = colPlain @Text "origin"   flights
      destV2  = colPlain @Text "dest"     flights
      distV   = colPlain @Int  "distance" flights
      odGroups = M.toAscList (M.fromListWith (++)
                   [ ((o,d), [fromIntegral dist :: Double])
                   | (o, d, dist) <- zip3 originV destV2 distV ])
      odIqr = [ (o, d, D.iqrL ds, length ds)
              | ((o,d), ds) <- odGroups, D.iqrL ds > 0 ]
      egeDF = DF.fromNamedColumns
                [ ("origin",       DF.fromList [ o | (o,_,_,_) <- odIqr ])
                , ("dest",         DF.fromList [ d | (_,d,_,_) <- odIqr ])
                , ("distance_iqr", DF.fromList [ q | (_,_,q,_) <- odIqr ])
                , ("n",            DF.fromList [ n | (_,_,_,n) <- odIqr ]) ]
  putStrLn "group_by(origin,dest) |> summarize(distance_iqr = IQR(distance), n) |> filter(iqr>0):"
  putStrLn "(空港間距離は一定のはずだが EGE は距離が複数あり IQR>0 = データの奇妙な点)"
  printHead egeDF

  sect "13.5.4 分布 (図③ ヒストグラム 2 枚 patchwork + 図④ 365 頻度ポリゴン)"
  -- ③ 本流: flights を直接束縛し列名 "dep_delay" で histogram (Maybe Int の NA は
  --    resolver が内部処理・生列抽出は不要)。 R4DS は filter(dep_delay<120) してから
  --    binwidth=5 ゆえ DataFrame 側で DF.filterJust + DF.filterWhere (= ggplot の
  --    filter |> ggplot と同型)。 patchwork は subplots + bakeSpec (各 panel に別 df)。
  let depDelay   = colMaybe @Int "dep_delay" flights
      depDelayNN = map fromIntegral (catMaybes depDelay) :: [Double]   -- 図④ 用
      ddZoom = flights |> DF.filterJust  "dep_delay"
                       |> DF.filterWhere (F.col @Int "dep_delay" .< (120 :: DF.Expr Int))
  saveSVG "fig3-dist.svg" $ subplots
    [ bakeSpec (toResolver flights) (layer (histogram "dep_delay" <> binWidth 15) <> title "全体 (binwidth 15)")
    , bakeSpec (toResolver ddZoom)  (layer (histogram "dep_delay" <> binWidth 5)  <> title "dep_delay < 120 (binwidth 5)") ]
    <> subplotCols 2
  putStrLn "[図] fig3-dist.svg (本流: flights 直接 + DF.filterWhere・左 全体 / 右 <120 拡大)"

  -- ④ 365 日の頻度ポリゴンを単色 alpha で重畳 (= R geom_freqpoly group=interaction(day,month))
  let monthV2 = colPlain @Int "month" flights
      dayV2   = colPlain @Int "day"   flights
      bw      = 5 :: Double
      binsLo  = fromIntegral (floor (minimum (filter (<120) depDelayNN) / bw) * round bw) :: Double
      centers = takeWhile (< 120) [ binsLo + bw/2 + bw * fromIntegral k | k <- [0 :: Int ..] ]
      binIdx v = floor ((v - binsLo) / bw) :: Int
      dayGroups = M.toAscList (M.fromListWith (++)
                    [ ((m,d), [v]) | (m, d, Just dd) <- zip3 monthV2 dayV2 depDelay
                                   , let v = fromIntegral dd, v < 120 ])
      freqLine vs = let cnt = M.fromListWith (+) [ (binIdx v, 1 :: Double) | v <- vs ]
                    in [ M.findWithDefault 0 k cnt | k <- [0 .. length centers - 1] ]
      polyLayers = mconcat
        [ layer (lineXY centers (freqLine vs) <> colorStatic "#000000" <> alpha 0.2)
        | (_, vs) <- dayGroups ]
  saveSVGBound "fig4-freqpoly-365.svg" $ ddZoom |>> polyLayers
  putStrLn $ "[図] fig4-freqpoly-365.svg (" ++ show (length dayGroups)
           ++ " 日の頻度ポリゴンを単色 alpha で重畳 → 太い黒帯・共通パターン)"

  sect "13.5.5 位置 — first / last / nth (日ごと)"
  -- R: summarize(first_dep = first(dep_time,na_rm=T), fifth_dep = nth(dep_time,5), last_dep = last(...))
  let depTimeV = colMaybe @Int "dep_time" flights
      ymdV     = zip3 (colPlain @Int "year" flights) monthV2 dayV2
      posGroups = M.toAscList (M.fromListWith (flip (++))
                    [ (k, [dt]) | (k, dt) <- zip ymdV depTimeV ])
      nthMaybe n xs = case drop (n-1) (catMaybes xs) of (v:_) -> Just v; [] -> Nothing
      posDF = DF.fromNamedColumns
                [ ("year",      DF.fromList [ y | ((y,_,_),_) <- posGroups ])
                , ("month",     DF.fromList [ m | ((_,m,_),_) <- posGroups ])
                , ("day",       DF.fromList [ d | ((_,_,d),_) <- posGroups ])
                , ("first_dep", DF.fromList [ nthMaybe 1 vs | (_, vs) <- posGroups ])
                , ("fifth_dep", DF.fromList [ nthMaybe 5 vs | (_, vs) <- posGroups ])
                , ("last_dep",  DF.fromList [ if null (catMaybes vs) then Nothing
                                              else Just (last (catMaybes vs)) | (_, vs) <- posGroups ]) ]
  putStrLn "group_by(year,month,day) |> summarize(first_dep, fifth_dep = nth(.,5), last_dep):"
  printHead posDF

  sect "13.5.6 mutate との組合せ (群標準化)"
  -- R: x/sum(x) / (x-mean)/sd / (x-min)/(max-min) / x/first(x)
  let zdf = DF.fromNamedColumns [ ("x", DF.fromList ([2,4,4,4,5,5,7,9] :: [Double])) ]
            |> mutate [ "zscore" =: zscoreOf "x" ]
  putStrLn "x = c(2,4,4,4,5,5,7,9) |> mutate(zscore = (x - mean(x))/sd(x)):"
  printHead zdf
  putStrLn "(他に x/sum(x)=割合・(x-min)/(max-min)=[0,1] 化・x/first(x)=指数化 もリサイクルで書ける)"

  putStrLn "\n(以上で R4DS Ch13 の全節を再現。図 4 枚 = fig1〜fig4 SVG)"

-- | 数値列で降順ソートした DataFrame (count(sort=TRUE) / arrange(desc) 用)。
sortByCol :: Text -> DF.DataFrame -> DF.DataFrame
sortByCol name df =
  let n   = fst (DF.dimensions df)
      key = case safeCol @Int name df of
              Just is -> map fromIntegral is
              Nothing -> fromMaybe (replicate n 0) (safeCol @Double name df)
      ord = map snd (sortOn (Down . fst) (zip key [0 :: Int ..]))
  in selectIndices (VU.fromList ord) df

