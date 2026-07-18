-- | チュートリアル 16: 因子 (R4DS 2e Ch16 "Factors")
--   https://r4ds.hadley.nz/factors
--
--   factor は「取りうる値が固定されたカテゴリ変数」を表す型である。R では
--   forcats (tidyverse) の `fct_*` 関数群で水準 (levels) の順序や中身を操作する。
--   本章はその全節 (16.2 基礎 / 16.3 GSS / 16.4 順序変更 / 16.5 水準変更 /
--   16.6 順序付き因子) を **実データ gss_cat** (General Social Survey 抽出・
--   21,483 行) で忠実再現する。forcats の `fct_*` は analyze 側 module
--   @Hanalyze.Data.Factor@ に実装済 (Phase 28 Ch16 A2-A4)。本ファイルは
--   それを呼んで R4DS の各出力 (count 表・reorder 後の水準順・lump 結果) と
--   §16.4 の図 (relig×tvhours の reorder 前後・rincome×age・marital の年齢推移と棒) を
--   再現する。
--
--   ・図 (§16.4) は geom_point / geom_line / geom_bar を使う。水準の並べ替えは
--     Data.Factor で算出し、`scaleYDiscreteLimits` / `colorCats` / `scaleXDiscreteLimits`
--     で軸・凡例順に反映する (= R の fct_reorder/fct_reorder2/fct_infreq の効果)。
--   ・gss_cat の factor 水準の **定義順序** (R の factor 既定順) は SOURCE.md 記録に従う
--     (アルファベット順ではない・捏造しない)。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.DeepSeq           (force, NFData)
import           Control.Exception         (try, evaluate, SomeException)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.Maybe                (fromMaybe, mapMaybe, catMaybes)
import           Data.List                 (sort, sortBy, nub)
import           Data.Ord                  (Down (..), comparing)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as M
import qualified DataFrame.IO.CSV                     as DF
import qualified DataFrame.Internal.DataFrame         as DF
import qualified DataFrame.Operators                  as DF
import qualified DataFrame.Operations.Core            as DF
import qualified DataFrame.Internal.Column as DFC

-- analyze 側 forcats 相当 (Phase 28 Ch16)
import           Hanalyze.Data.Factor

-- plot
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Frame        ((|>>))
import           Graphics.Hgg.Backend.SVG  (saveSVGBound)
import           Graphics.Hgg.DataFrame    ()

-- ============================================================================
-- 列抽出 (Strings.hs / Numbers.hs と同方式・型不一致は [] / Nothing に倒す)
-- ============================================================================

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

-- tvhours / age は欠損 (空欄) を含むため Maybe Int で読む。
colMaybeInt :: Text -> DF.DataFrame -> [Maybe Int]
colMaybeInt n df = case safeCol @(Maybe Int) n df of
  Just xs -> xs
  Nothing -> map Just (colPlain @Int n df)   -- 欠損なし列は Int で来る

-- ============================================================================
-- gss_cat の factor 定義順序 (R の factor 既定順・SOURCE.md 一次根拠)
-- ============================================================================

maritalLevels :: [Text]
maritalLevels =
  ["No answer", "Never married", "Separated", "Divorced", "Widowed", "Married"]

raceLevels :: [Text]
raceLevels = ["Other", "Black", "White", "Not applicable"]

rincomeLevels :: [Text]
rincomeLevels =
  [ "No answer", "Don't know", "Refused", "$25000 or more", "$20000 - 24999"
  , "$15000 - 19999", "$10000 - 14999", "$8000 to 9999", "$7000 to 7999"
  , "$6000 to 6999", "$5000 to 5999", "$4000 to 4999", "$3000 to 3999"
  , "$1000 to 2999", "Lt $1000", "Not applicable" ]

partyidLevels :: [Text]
partyidLevels =
  [ "No answer", "Don't know", "Other party", "Strong republican"
  , "Not str republican", "Ind,near rep", "Independent", "Ind,near dem"
  , "Not str democrat", "Strong democrat" ]

religLevels :: [Text]
religLevels =
  [ "No answer", "Don't know", "Inter-nondenominational", "Native american"
  , "Christian", "Orthodox-christian", "Moslem/islam", "Other eastern"
  , "Hinduism", "Buddhism", "Other", "None", "Jewish", "Catholic"
  , "Protestant", "Not applicable" ]

-- ============================================================================
-- 表示ヘルパ
-- ============================================================================

sect :: String -> IO ()
sect s = putStrLn ("\n========== " <> s <> " ==========")

-- count() 風: (label, n) を表示。drop0=True なら 0 件水準を落とす (R count() 既定)。
putCount :: Bool -> [(Text, Int)] -> IO ()
putCount drop0 = mapM_ row . filter keep
  where keep (_, n) = not drop0 || n > 0
        row (lv, n)  = putStrLn ("  " <> pad 28 (T.unpack lv) <> rjust 6 (show n))

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

rjust :: Int -> String -> String
rjust n s = replicate (max 0 (n - length s)) ' ' ++ s

-- 中央値 (R の median と同義・偶数個は中央 2 値平均)。
medianD :: [Double] -> Double
medianD [] = 0
medianD xs =
  let s = sort xs; n = length s
  in if odd n then s !! (n `div` 2)
     else (s !! (n `div` 2 - 1) + s !! (n `div` 2)) / 2

-- ============================================================================
-- 集計ヘルパ
-- ============================================================================

-- | 水準ごとの (非欠損値の平均, 総行数)。levels 順に返す。
summarizeMean :: [Text] -> [Text] -> [Maybe Double] -> [(Text, Maybe Double, Int)]
summarizeMean lvls keys vals =
  [ (lv, mmean lv, M.findWithDefault 0 lv cntMap) | lv <- lvls ]
  where
    cntMap = M.fromListWith (+) [ (k, 1 :: Int) | k <- keys ]
    valMap = M.fromListWith (++) [ (k, [v]) | (k, Just v) <- zip keys vals ]
    mmean lv = case M.findWithDefault [] lv valMap of
                 [] -> Nothing
                 vs -> Just (sum vs / fromIntegral (length vs))

-- | 横向き dot plot (R4DS の aes(x=値, y=カテゴリ) + geom_point)。
--   scatter は categorical 位置を描けないため、水準を数値 index にして
--   `axisBreaksLabeled` で y 軸ラベルを差す。@order@ は下→上の水準順、
--   @meanMap@ は水準→x 値。
saveDotH :: FilePath -> [Text] -> M.Map Text Double -> Text -> Text -> Text -> IO ()
saveDotH path order meanMap xlab ylab ttl =
    saveSVGBound path $
      DF.empty |>> theme ThemeGrey <> layer (scatter (inline xs) (inline ys))
         <> yAxis (axisRange (-0.6) (fromIntegral n - 0.4) <> axisBreaksLabeled ticks)
         <> xLabel xlab <> yLabel ylab <> title ttl
  where
    n     = length order
    xs    = [ M.findWithDefault (0 / 0) lv meanMap | lv <- order ]
    ys    = [ fromIntegral i | i <- [0 .. n - 1] ] :: [Double]
    ticks = [ (fromIntegral i, lv) | (i, lv) <- zip [0 :: Int ..] order ]

main :: IO ()
main = do
  gss0 <- DF.readCsv "../_data/_raw/gss_cat.csv"
  let -- factor 列 (Text)
      marital = colPlain @Text "marital" gss0
      race    = colPlain @Text "race"    gss0
      rincome = colPlain @Text "rincome" gss0
      partyid = colPlain @Text "partyid" gss0
      relig   = colPlain @Text "relig"   gss0
      -- 数値列 (欠損あり)
      tvhours = map (fmap fromIntegral) (colMaybeInt "tvhours" gss0) :: [Maybe Double]
      ageM    = colMaybeInt "age" gss0
      year    = colPlain @Int "year" gss0

  -- =========================================================================
  -- §16.2 Factor basics
  -- =========================================================================
  sect "16.2 Factor basics — factor() / fct() / levels"
  let x1          = ["Dec", "Apr", "Jan", "Mar"] :: [Text]
      monthLevels = [ "Jan","Feb","Mar","Apr","May","Jun"
                    , "Jul","Aug","Sep","Oct","Nov","Dec" ] :: [Text]
  -- R: factor(x1) は水準をアルファベット順にする (誤植に気づきにくい既定)。
  putStrLn $ "factor(x1) levels      = " <> show (levels (factor x1))
  -- R: factor(x1, levels=month_levels) で意味的順序を与える。
  putStrLn $ "factorWith months      = " <> show (levels (factorWith monthLevels x1))
  -- R: fct(x1) は出現順 (forcats・誤った値はエラーにできる安全版)。
  putStrLn $ "fct(x1) levels         = " <> show (levels (fct x1))
  -- as.character で元の値へ戻る (水準順は保持)。
  putStrLn $ "asTexts(factorWith)    = " <> show (asTexts (factorWith monthLevels x1))

  -- =========================================================================
  -- §16.3 General Social Survey — count() で水準の頻度を見る
  -- =========================================================================
  sect "16.3 GSS — count(race) (0 件水準は drop = R count() 既定)"
  putCount True (fctCount (factorWith raceLevels race))
  sect "16.3 GSS — 最多の relig / partyid"
  let religCount   = fctCount (factorWith religLevels relig)
      partyidCount = fctCount (factorWith partyidLevels partyid)
      topOf        = fst . head . sortBy (comparing (Down . snd))
  putStrLn $ "  relig 最多   = " <> T.unpack (topOf religCount)
               <> " (" <> show (maximum (map snd religCount)) <> ")"
  putStrLn $ "  partyid 最多 = " <> T.unpack (topOf partyidCount)
               <> " (" <> show (maximum (map snd partyidCount)) <> ")"

  -- =========================================================================
  -- §16.4 Modifying factor order
  -- =========================================================================
  sect "16.4 順序変更 — relig×tvhours summary + fct_reorder"
  -- relig_summary: group_by(relig) |> summarize(mean tvhours na.rm, n)。
  let religSummary = summarizeMean religLevels relig tvhours
      religPresent = [ lv | (lv, Just _, _) <- religSummary ]
      religMeans   = [ m  | (_,  Just m, _) <- religSummary ]
      -- fct_reorder(relig, tvhours): 各 relig を tvhours 平均の昇順に並べ替える。
      religReord   = levels (fctReorder medianD (factorWith religPresent religPresent) religMeans)
      religMeanMap = M.fromList (zip religPresent religMeans)
  mapM_ (\(lv, mm, n) -> putStrLn $ "  " <> pad 26 (T.unpack lv)
                  <> rjust 6 (maybe "NA" (\v -> show (round2 v)) mm)
                  <> rjust 8 (show n)) religSummary

  -- 図 1: 並べ替え前 (relig の factor 既定順 = 解釈しにくい)。
  saveDotH "01-relig-tvhours-unordered.svg" religPresent religMeanMap
           "tvhours" "relig" "16.4 relig vs tvhours (並べ替え前)"

  -- 図 2: fct_reorder で tvhours 昇順に並べ替え (Don't know が最多視聴と読める)。
  saveDotH "02-relig-tvhours-reorder.svg" religReord religMeanMap
           "tvhours" "fct_reorder(relig, tvhours)" "16.4 relig vs tvhours (fct_reorder 後)"

  -- 図 3: rincome×age を fct_relevel で "Not applicable" を先頭 (= y 軸下端) へ。
  sect "16.4 順序変更 — rincome×age + fct_relevel"
  let rincomeSummary = summarizeMean rincomeLevels rincome (map (fmap fromIntegral) ageM)
      rincomePresent = [ lv | (lv, Just _, _) <- rincomeSummary ]
      rincomeAges    = [ m  | (_,  Just m, _) <- rincomeSummary ]
      rincomeReleveled = levels (fctRelevel ["Not applicable"]
                                   (factorWith rincomePresent rincomePresent))
      rincomeMeanMap   = M.fromList (zip rincomePresent rincomeAges)
  saveDotH "03-rincome-age-relevel.svg" rincomeReleveled rincomeMeanMap
           "age" "fct_relevel(rincome, \"Not applicable\")" "16.4 rincome vs age (fct_relevel)"

  -- 図 4: marital の年齢別割合を折れ線 + fct_reorder2 で凡例順を右端の高さに合わせる。
  sect "16.4 順序変更 — marital の年齢推移 (fct_reorder2 で凡例順)"
  let ages        = sort (nub (catMaybes ageM))
      -- by_age: 各 age での marital 構成比 prop = n / sum(n at that age)。
      maritalPresent = [ lv | lv <- maritalLevels
                            , lv `elem` nub marital ]
      propAt lv a =
        let atAge   = [ m | (m, Just a') <- zip marital ageM, a' == a ]
            total   = length atAge
            hit     = length (filter (== lv) atAge)
        in if total == 0 then 0 else fromIntegral hit / fromIntegral total :: Double
      -- 折れ線用 long フォーマット (marital ごとに age 系列を連結)。
      longAge  = concat [ map fromIntegral ages | _ <- maritalPresent ]
      longProp = concat [ [ propAt lv a | a <- ages ] | lv <- maritalPresent ]
      longCat  = concat [ replicate (length ages) lv | lv <- maritalPresent ]
      -- fct_reorder2(marital, age, prop): 最大 age での prop の降順 (凡例 = 線の右端順)。
      maritalReord2 = levels (fctReorder2 (factorWith maritalPresent longCat) longAge longProp)
  saveSVGBound "04-marital-age-line.svg" $
    DF.empty |>> theme ThemeGrey <> layer (line (inline longAge) (inline longProp)
                          <> colorBy (inlineCat longCat) <> colorCats maritalReord2)
       <> xLabel "age" <> yLabel "prop" <> legendTitle "marital"
       <> title "16.4 marital prop by age (fct_reorder2)"

  -- 図 5: marital の棒グラフを fct_infreq |> fct_rev で頻度昇順に並べる。
  sect "16.4 順序変更 — marital 棒 (fct_infreq |> fct_rev)"
  let maritalFac   = factorWith maritalLevels marital
      maritalOrder = levels (fctRev (fctInfreq maritalFac))
      maritalCount = fctCount maritalFac
      mcPresent    = [ (lv, n) | (lv, n) <- maritalCount, n > 0 ]
  saveSVGBound "05-marital-bar.svg" $
    DF.empty |>> theme ThemeGrey <> layer (bar (inlineCat (map fst mcPresent))
                            (inline (map (fromIntegral . snd) mcPresent)))
       <> scaleXDiscreteLimits maritalOrder
       <> xLabel "marital" <> yLabel "count"
       <> title "16.4 marital 棒 (fct_infreq |> fct_rev)"

  -- =========================================================================
  -- §16.5 Modifying factor levels
  -- =========================================================================
  sect "16.5 水準変更 — fct_recode(partyid)"
  let partyFac     = factorWith partyidLevels partyid
      partyRecoded = fctRecode
        [ ("Republican, strong",    "Strong republican")
        , ("Republican, weak",      "Not str republican")
        , ("Independent, near rep", "Ind,near rep")
        , ("Independent, near dem", "Ind,near dem")
        , ("Democrat, weak",        "Not str democrat")
        , ("Democrat, strong",      "Strong democrat") ] partyFac
  putCount True (fctCount partyRecoded)

  sect "16.5 水準変更 — fct_collapse(partyid)"
  let partyCollapsed = fctCollapse
        [ ("other", ["No answer", "Don't know", "Other party"])
        , ("rep",   ["Strong republican", "Not str republican"])
        , ("ind",   ["Ind,near rep", "Independent", "Ind,near dem"])
        , ("dem",   ["Not str democrat", "Strong democrat"]) ] partyFac
  putCount True (fctCount partyCollapsed)

  sect "16.5 水準変更 — fct_lump_lowfreq(relig)"
  let religFac = factorWith religLevels relig
  putCount True (fctCount (fctLumpLowfreq religFac))

  sect "16.5 水準変更 — fct_lump_n(relig, n = 10) (sort=TRUE)"
  let lumpedN = fctLumpN 10 religFac
  mapM_ (\(lv, n) -> putStrLn ("  " <> pad 16 (T.unpack lv) <> rjust 7 (show n)))
        (sortBy (comparing (Down . snd)) (filter ((>0) . snd) (fctCount lumpedN)))

  -- =========================================================================
  -- §16.6 Ordered factors
  -- =========================================================================
  sect "16.6 Ordered factors — ordered()"
  let oz = ordered ["a", "b", "c"] ["a", "b", "c"]
  putStrLn $ "  ordered levels = " <> T.unpack (T.intercalate " < " (levels oz))
               <> "   (isOrdered = " <> show (isOrdered oz) <> ")"
  putStrLn "  注: ggplot2 では ordered factor に viridis 連続色、線形モデルでは多項式 contrast"
  putStrLn "      が当たる (概念のみ・本実装は順序フラグを保持)。"

  putStrLn "\n[done] 16-factors: 5 figs + 各節の表出力を生成しました。"

round2 :: Double -> Double
round2 x = fromIntegral (round (x * 100) :: Int) / 100
