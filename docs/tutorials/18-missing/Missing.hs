-- | チュートリアル 09: 欠損値 (R4DS 2e Ch18 "Missing values")
--   https://r4ds.hadley.nz/missing-values
--
--   明示的な欠損 (NA) と暗黙的な欠損 (行が無い) の扱いを学ぶ。 fill (前方補完)・
--   coalesce (固定値で穴埋め)・pivot_wider/complete (暗黙の欠損を明示化)・
--   factor の空グループ。 データは R4DS 本文の実例 (treatment/stocks/health)。
--   この章は表操作が主役で、 R4DS が描く図は 2 枚 (空 factor の有無)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.DeepSeq          (force, NFData)
import           Control.Exception        (try, evaluate, SomeException)
import           System.IO.Unsafe         (unsafePerformIO)
import           Data.Maybe               (fromMaybe)
import           Data.List                (nub, sortOn)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified DataFrame.IO.CSV                     as DF
import qualified DataFrame.Internal.Column            as DF
import qualified DataFrame.Internal.DataFrame         as DF
import qualified DataFrame.Operators                  as DF
import qualified DataFrame.Operations.Core            as DF
import qualified DataFrame.Internal.Column as DFC
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Frame       ((|>>))
import           Graphics.Hgg.Backend.SVG (saveSVGBound)
import           Graphics.Hgg.DataFrame   ()

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colMaybe :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [Maybe a]
colMaybe n df = fromMaybe [] (safeCol @(Maybe a) n df)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

-- | 前方補完 (last observation carried forward = tidyr::fill)。
fillForward :: [Maybe a] -> [Maybe a]
fillForward = go Nothing
  where go _    []           = []
        go prev (Just x : r) = Just x : go (Just x) r
        go prev (Nothing: r) = prev   : go prev r

main :: IO ()
main = do
  -- === 明示的な欠損: fill (前方補完) ===
  treatment <- DF.readCsv "treatment.csv"
  putStrLn "== treatment (person が NA = 直前と同じ人) =="
  print treatment
  let persons = colMaybe @Text "person" treatment
      treats  = colPlain @Int "treatment" treatment
      resps   = colMaybe @Int "response" treatment
      filledDF = DF.fromNamedColumns
        [ ("person",    DF.fromList (fillForward persons :: [Maybe Text]))
        , ("treatment", DF.fromList (treats :: [Int]))
        , ("response",  DF.fromList (resps  :: [Maybe Int])) ]
  putStrLn "== fill(person): 直前の値で前方補完 =="
  print filledDF

  -- === 固定値で穴埋め: coalesce(response, 0) ===
  let coalescedDF = DF.fromNamedColumns
        [ ("person",    DF.fromList (fillForward persons :: [Maybe Text]))
        , ("treatment", DF.fromList (treats :: [Int]))
        , ("response0", DF.fromList (map (fromMaybe 0) resps :: [Int])) ]
  putStrLn "== coalesce(response, 0): NA を 0 に =="
  print coalescedDF

  -- === 暗黙的な欠損: stocks (2021 Q1 の行が無い・2020 Q4 は NA) ===
  stocks <- DF.readCsv "stocks.csv"
  putStrLn "== stocks (2020 Q4 = 明示 NA、 2021 Q1 = 行が無い暗黙の欠損) =="
  print stocks
  let syear  = colPlain @Int "year" stocks
      sqtr   = colPlain @Int "qtr"  stocks
      sprice = colMaybe @Double "price" stocks
      rows   = zip3 syear sqtr sprice
      priceOf y q = case [ p | (y',q',p) <- rows, y'==y, q'==q ] of
                      (p:_) -> p
                      []    -> Nothing
      years  = nub syear
      qtrs   = [1,2,3,4]

  -- pivot_wider(names_from=qtr, values_from=price): 暗黙の欠損が NA として見える
  let wideDF = DF.fromNamedColumns $
        ("year", DF.fromList (years :: [Int]))
        : [ ("q" <> tshow q, DF.fromList ([ priceOf y q | y <- years ] :: [Maybe Double]))
          | q <- qtrs ]
  putStrLn "== pivot_wider(qtr): 2021 Q1 の欠損が NA として明示化 =="
  print wideDF

  -- complete(year, qtr): 全 (year×qtr) の組合せを生成し、 無い行を NA で補う
  let completeDF = DF.fromNamedColumns
        [ ("year",  DF.fromList ([ y | y <- years, _ <- qtrs ] :: [Int]))
        , ("qtr",   DF.fromList (concat [ qtrs | _ <- years ] :: [Int]))
        , ("price", DF.fromList ([ priceOf y q | y <- years, q <- qtrs ] :: [Maybe Double])) ]
  putStrLn "== complete(year, qtr): 8 組すべて (2021 Q1 が NA 行として出現) =="
  print completeDF

  -- === factor と空グループ: 図2枚 (= drop の有無) ===
  -- health の smoker は levels = {yes, no} だが全員 no。 yes は「空グループ」。
  health <- DF.readCsv "health.csv"
  let smokers = colPlain @Text "smoker" health
      nNo  = fromIntegral (length (filter (== "no")  smokers)) :: Double
      nYes = fromIntegral (length (filter (== "yes") smokers)) :: Double

  -- 図1: 観測された値だけ数える (= 既定。 空グループ "yes" は出ない)
  let presentDF = DF.fromNamedColumns
        [ ("smoker", DF.fromList (nub smokers :: [Text]))
        , ("n",      DF.fromList ([ fromIntegral (length (filter (== s) smokers)) :: Double
                                  | s <- nub smokers ])) ]
  saveSVGBound "01-drop-empty.svg" $
    presentDF |>> theme ThemeGrey <> layer (bar "smoker" "n")
              <> title "空グループを落とす (観測された値のみ)"
              <> xLabel "smoker" <> yLabel "count"

  -- 図2: factor の全 level を保持 (= drop = FALSE。 yes = 0 も棒として出す)
  let keptDF = DF.fromNamedColumns
        [ ("smoker", DF.fromList (["yes","no"] :: [Text]))
        , ("n",      DF.fromList ([nYes, nNo] :: [Double])) ]
  saveSVGBound "02-keep-empty.svg" $
    keptDF |>> theme ThemeGrey <> layer (bar "smoker" "n")
           <> title "空グループも保持 (drop = FALSE、 yes = 0)"
           <> xLabel "smoker" <> yLabel "count"

  putStrLn "wrote 01-drop-empty.svg, 02-keep-empty.svg"
  where
    tshow :: Int -> Text
    tshow = T.pack . show
