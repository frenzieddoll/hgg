-- | チュートリアル 05: データの整然化 (R4DS 2e Ch5 "Data tidying")
--   https://r4ds.hadley.nz/data-tidy
--
--   整然 (tidy) データの 3 規則 — 「各列が 1 変数・各行が 1 観測・各セルが 1 値」 —
--   を tidyr の table1/2/3 で確認し、 pivot_longer / pivot_wider を dataframe で
--   忠実に再現する。 dataframe 1.3 には pivot が無いので、 列の型差を吸収する Cell
--   中間表現の上に汎用 helper (pivotLongerG / pivotLongerValueG / pivotWiderG) を
--   自前定義する (= pivot が裏でやることそのもの。 特定の表にハードコードしない)。
--
--   R4DS Ch5 が R コードで描く図は 2 枚:
--     (1) table1 の結核罹患者数の年次推移、 (2) billboard の順位推移。
--   その他の include_graphics 図 (tidy-1.png 等) は R4DS 手描きの解説図で、 コード
--   出力ではないため再現対象外 (README に明記)。
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE BangPatterns        #-}
module Main (main) where

import           Control.DeepSeq           (force, NFData)
import           Control.Exception         (try, evaluate, SomeException)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.List                 (foldl')
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Data.Map.Strict           as M
import qualified DataFrame                 as DF
import qualified DataFrame.Internal.Column as DFC
import qualified DataFrame.Functions       as F
import           DataFrame.Operators       ((|>))
import           Hgg.Plot.Easy
import           Hgg.Plot.Frame        ((|>>))
import           Hgg.Plot.Backend.SVG  (saveSVGBound)
import           Hgg.Plot.DataFrame    ()

-- ===========================================================================
-- 列の型差を吸収する中間表現 Cell
--   dataframe の列は Int / Double / Text / Maybe 各種と型が分かれており、
--   pivot のように複数列を 1 列に畳む / 1 列を複数列に開くと型が混在する。
--   いったん全部 Cell に持ち上げて畳み、 出力時に列ごとに型を選び直す。
-- ===========================================================================

data Cell = CI !Int | CD !Double | CT !Text | CNA
  deriving (Eq, Show)

-- | 例外セーフに列を読む (型が合わなければ Nothing)。
safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

-- | 列を Cell ベクトルへ。 ★Maybe 版を先に試す: dataframe は空セルを validity
--   bitmap で持ち、 @Int 等の非 Maybe 読みは validity を無視して 0 を返してしまう。
--   @(Maybe Int) 等で読めば NA を Nothing として正しく拾える (空文字も NA 扱い)。
readCells :: Text -> DF.DataFrame -> V.Vector Cell
readCells name df =
  case safeCol @(Maybe Int) name df of
    Just xs -> V.fromList (map (maybe CNA CI) xs)
    Nothing -> case safeCol @Int name df of
      Just xs -> V.fromList (map CI xs)
      Nothing -> case safeCol @(Maybe Double) name df of
        Just xs -> V.fromList (map (maybe CNA CD) xs)
        Nothing -> case safeCol @Double name df of
          Just xs -> V.fromList (map CD xs)
          Nothing -> case safeCol @(Maybe Text) name df of
            Just xs -> V.fromList (map (maybe CNA txt) xs)
            Nothing -> case safeCol @Text name df of
              Just xs -> V.fromList (map txt xs)
              Nothing -> V.replicate (fst (DF.dimensions df)) CNA
  where txt t = if T.null (T.strip t) then CNA else CT t

cellText :: Cell -> Text
cellText (CI i) = T.pack (show i)
cellText (CD d) = T.pack (show d)
cellText (CT t) = t
cellText CNA    = ""

-- | Cell のリストから列を組み立てる。 列内の型を見て Int / Double / Text を選び、
--   CNA を含むなら Maybe 版にする (= R の <int>/<dbl>/<chr> + NA 相当)。
buildColumn :: [Cell] -> DFC.Column
buildColumn cells
  | any isT cells = if hasNA then DF.fromList (map toMT cells) else DF.fromList (map cellText cells)
  | any isD cells = if hasNA then DF.fromList (map toMD cells) else DF.fromList (map toD cells)
  | otherwise     = if hasNA then DF.fromList (map toMI cells) else DF.fromList (map toI cells)
  where
    hasNA = CNA `elem` cells
    isT (CT _) = True; isT _ = False
    isD (CD _) = True; isD _ = False
    toI  (CI i) = i;            toI  _ = 0          :: Int
    toMI (CI i) = Just i;       toMI _ = Nothing    :: Maybe Int
    toD  (CD d) = d; toD (CI i) = fromIntegral i; toD _ = 0  :: Double
    toMD (CD d) = Just d; toMD (CI i) = Just (fromIntegral i); toMD _ = Nothing :: Maybe Double
    toMT CNA    = Nothing;      toMT c = Just (cellText c)    :: Maybe Text

-- | 先頭出現順を保つ nub。
nubKeep :: Eq a => [a] -> [a]
nubKeep = go []
  where go _ [] = []
        go seen (x:xs) | x `elem` seen = go seen xs
                       | otherwise     = x : go (seen ++ [x]) xs

-- ===========================================================================
-- pivot helper (= tidyr pivot_longer / pivot_wider 相当・汎用)
-- ===========================================================================

-- | wide → long (pivot_longer)。 nameExpand が「元の値列名 → [(出力 name 列, その値)]」 を
--   返す。 単一 names_to なら 1 要素、 names_sep で複数に割るなら複数要素を返す。
--   出力は行優先 (R4DS の表示順と一致): 元の行ごとに value 列を順に開く。
pivotLongerG
  :: (Text -> [(Text, Cell)])  -- ^ 値列名 → names_to 展開
  -> Text                      -- ^ values_to 列名
  -> Bool                      -- ^ values_drop_na
  -> [Text] -> [Text]          -- ^ idCols, valueCols
  -> DF.DataFrame -> DF.DataFrame
pivotLongerG nameExpand valueCol dropNA idCols valueCols df =
  let n        = fst (DF.dimensions df)
      idVecs   = [ (k, readCells k df) | k <- idCols ]
      valVecs  = [ (c, readCells c df) | c <- valueCols ]
      kept     = [ (i, c, cell)
                 | i <- [0 .. n-1]
                 , (c, vv) <- valVecs
                 , let cell = vv V.! i
                 , not (dropNA && cell == CNA) ]
      nameCols = map fst (nameExpand (head valueCols))
      idOut (k, vv)  = (k,  buildColumn [ vv V.! i              | (i,_,_) <- kept ])
      nameOut pos nm = (nm, buildColumn [ snd (nameExpand c !! pos) | (_,c,_) <- kept ])
  in DF.fromNamedColumns $
       map idOut idVecs
       ++ [ nameOut pos nm | (pos, nm) <- zip [0 ..] nameCols ]
       ++ [ (valueCol, buildColumn [ cell | (_,_,cell) <- kept ]) ]

-- | wide → long で、 names_to に特殊値 ".value" を使う場合 (pivot_longer の .value sentinel)。
--   各値列名を sep で 2 分割し、 第 1 片を「出力列名」 (= .value)、 第 2 片以降を
--   もう一方の name 列 (otherCol) の値にする。 例: "dob_child1" → 列 dob / child="child1"。
pivotLongerValueG
  :: Text                    -- ^ sep
  -> Text                    -- ^ もう一方の name 列名 (例 "child")
  -> Bool                    -- ^ values_drop_na (全 .value が NA の行を落とす)
  -> [Text] -> [Text]        -- ^ idCols, valueCols
  -> DF.DataFrame -> DF.DataFrame
pivotLongerValueG sep otherCol dropNA idCols valueCols df =
  let n          = fst (DF.dimensions df)
      idVecs     = [ (k, readCells k df) | k <- idCols ]
      valVecs    = [ (c, readCells c df) | c <- valueCols ]
      split c    = T.splitOn sep c
      pieceVal   = head . split
      pieceOther = T.intercalate sep . tail . split
      valuePieces = nubKeep (map pieceVal   valueCols)   -- 出力列名 (例 [dob, name])
      otherPieces = nubKeep (map pieceOther valueCols)   -- child 値   (例 [child1, child2])
      cellAt vp p i = maybe CNA (V.! i) (lookup (vp <> sep <> p) valVecs)
      kept       = [ (i, p)
                   | i <- [0 .. n-1], p <- otherPieces
                   , not (dropNA && all (\vp -> cellAt vp p i == CNA) valuePieces) ]
      idOut (k, vv) = (k, buildColumn [ vv V.! i | (i,_) <- kept ])
      otherOut      = (otherCol, buildColumn [ CT p | (_,p) <- kept ])
      valueOut vp   = (vp, buildColumn [ cellAt vp p i | (i,p) <- kept ])
  in DF.fromNamedColumns $
       map idOut idVecs ++ [otherOut] ++ map valueOut valuePieces

-- | long → wide (pivot_wider)。 idCols ごとに 1 行、 namesFrom の各値を新しい列に、
--   valuesFrom の値を埋める。 入力に無い (id, name) の組は NA。
pivotWiderG :: [Text] -> Text -> Text -> DF.DataFrame -> DF.DataFrame
pivotWiderG idCols namesFrom valuesFrom df =
  let n        = fst (DF.dimensions df)
      idVecs   = [ (k, readCells k df) | k <- idCols ]
      nameVec  = readCells namesFrom  df
      valVec   = readCells valuesFrom df
      idKeyAt i = T.intercalate "\0" [ cellText (vv V.! i) | (_,vv) <- idVecs ]
      idTuples  = nubKeepBy fst [ (idKeyAt i, [ vv V.! i | (_,vv) <- idVecs ]) | i <- [0 .. n-1] ]
      newCols   = nubKeep [ cellText (nameVec V.! i) | i <- [0 .. n-1] ]
      cellMap   = M.fromList [ ((idKeyAt i, cellText (nameVec V.! i)), valVec V.! i) | i <- [0 .. n-1] ]
      idColOut j (k,_) = (k, buildColumn [ comps !! j | (_, comps) <- idTuples ])
      newColOut nm     = (nm, buildColumn [ M.findWithDefault CNA (key, nm) cellMap | (key,_) <- idTuples ])
  in DF.fromNamedColumns $
       [ idColOut j kv | (j, kv) <- zip [0 ..] idVecs ]
       ++ [ newColOut nm | nm <- newCols ]
  where
    nubKeepBy f = go []
      where go _ [] = []
            go seen (x:xs) | f x `elem` seen = go seen xs
                           | otherwise       = x : go (seen ++ [f x]) xs

-- ===========================================================================

main :: IO ()
main = do
  -- === 整然データの 3 規則 と table1/2/3 ===
  let raw = "../_data/_raw/"
  table1raw <- DF.readCsv (raw <> "table1.csv")
  table2raw <- DF.readCsv (raw <> "table2.csv")
  table3raw <- DF.readCsv (raw <> "table3.csv")
  let table1 = table1raw |> DF.select ["country","year","cases","population"]
      table2 = table2raw |> DF.select ["country","year","type","count"]
      table3 = table3raw |> DF.select ["country","year","rate"]
  putStrLn "== table1 (整然: 1 行 = 1 国×年・cases/population が列) =="
  print table1
  putStrLn "== table2 (非整然: cases と population が type/count に縦持ち) =="
  print table2
  putStrLn "== table3 (非整然: rate セルに 2 値が詰まっている) =="
  print table3

  -- === 整然だと計算しやすい: rate と 年ごとの合計 ===
  -- R: table1 |> mutate(rate = cases / population * 10000)
  let withRate = table1 |> DF.derive "rate"
                   (F.toDouble (F.col @Int "cases") / F.toDouble (F.col @Int "population") * 10000)
  putStrLn "== mutate(rate = cases / population * 10000) =="
  print (withRate |> DF.select ["country","year","rate"])

  -- R: table1 |> group_by(year) |> summarize(total_cases = sum(cases))
  let byYear = table1 |> DF.groupBy ["year"]
                      |> DF.aggregate [ F.sum (F.col @Int "cases") `F.as` "total_cases" ]
                      |> DF.sortBy [DF.Asc (F.col @Int "year")]
  putStrLn "== group_by(year) |> summarize(total_cases) =="
  print byYear

  -- === 図1 (R4DS): 結核罹患者数の年次推移 (国別の折れ線 + 色/形の点) ===
  -- R: ggplot(table1, aes(year, cases)) + geom_line(aes(group=country)) +
  --      geom_point(aes(color=country, shape=country)) + scale_x_continuous(breaks=c(1999,2000))
  saveSVGBound "tb-cases.svg" $
    table1 |>> layer (line "year" "cases" <> color "country")
           <> layer (scatter "year" "cases"
                       <> color "country" <> shapeBy "country" <> size 7)
           <> palette okabeIto
           <> xAxis (axisBreaksAt [1999, 2000])   -- scale_x_continuous(breaks=c(1999,2000))
           <> title "結核の罹患者数 (年次・国別)"
           <> xLabel "year" <> yLabel "cases"
  putStrLn "wrote tb-cases.svg"

  -- === pivot_longer: billboard (wide: wk1..wk76) を long に ===
  -- R: billboard |> pivot_longer(starts_with("wk"), names_to="week",
  --      values_to="rank", values_drop_na=TRUE) |> mutate(week = parse_number(week))
  billboardRaw <- DF.readCsv (raw <> "billboard.csv")
  let wkCols  = filter ("wk" `T.isPrefixOf`) (DF.columnNames billboardRaw)
      parseWk = (read . T.unpack . T.drop 2)            -- "wk12" -> 12 (parse_number)
      bbLong  = pivotLongerG (\c -> [("week", CI (parseWk c))]) "rank" True
                             ["artist","track"] wkCols billboardRaw
  putStrLn $ "== pivot_longer(billboard): " <> show (DF.dimensions billboardRaw)
               <> " → " <> show (DF.dimensions bbLong) <> " (values_drop_na) =="
  print (bbLong |> DF.take 6)

  -- === 図2 (R4DS): billboard の順位推移 (曲ごとの折れ線・1 位を上に) ===
  -- R: ggplot(bbLong, aes(week, rank, group=track)) + geom_line(alpha=1/4) + scale_y_reverse()
  saveSVGBound "billboard-ranks.svg" $
    bbLong |>> layer (line "week" "rank" <> linetypeBy "track" <> linetype LtSolid
                        <> colorStatic "#88888855")
           <> reverseY        -- scale_y_reverse: rank 1 を上に
           <> title "billboard の順位推移 (曲ごと・2000 年)"
           <> xLabel "week (chart 入り後の週)" <> yLabel "rank (1 位が上)"
  putStrLn "wrote billboard-ranks.svg"

  -- === pivot の仕組み (toy df) ===
  -- R: df <- tribble(~id,~bp1,~bp2, "A",100,120, "B",140,115, "C",120,125)
  let toyL = DF.fromNamedColumns
               [ ("id",  DF.fromList (["A","B","C"] :: [Text]))
               , ("bp1", DF.fromList ([100,140,120] :: [Int]))
               , ("bp2", DF.fromList ([120,115,125] :: [Int])) ]
  putStrLn "== toy df (pivot_longer の説明用) =="
  print toyL
  -- R: df |> pivot_longer(bp1:bp2, names_to="measurement", values_to="value")
  putStrLn "== pivot_longer(bp1:bp2): id を反復・列名を measurement・値を value =="
  print (pivotLongerG (\c -> [("measurement", CT c)]) "value" False ["id"] ["bp1","bp2"] toyL)

  -- === pivot_longer + names_sep: who2 (列名に 3 情報) ===
  -- R: who2 |> pivot_longer(!(country:year), names_to=c("diagnosis","gender","age"),
  --      names_sep="_", values_to="count")
  who2 <- DF.readCsv (raw <> "who2.csv")
  let who2Vals = filter (`notElem` ["country","year"]) (DF.columnNames who2)
      who2Long = pivotLongerG
                   (\c -> zip ["diagnosis","gender","age"] (map CT (T.splitOn "_" c)))
                   "count" False ["country","year"] who2Vals who2
  putStrLn $ "== pivot_longer(who2, names_sep=\"_\"): " <> show (DF.dimensions who2)
               <> " → " <> show (DF.dimensions who2Long) <> " =="
  print (who2Long |> DF.take 6)

  -- === pivot_longer + ".value" sentinel: household ===
  -- R: household |> pivot_longer(!family, names_to=c(".value","child"),
  --      names_sep="_", values_drop_na=TRUE)
  household <- DF.readCsvWithSchema
                 (DF.makeSchema [ ("dob_child1",  DF.schemaType @Text)
                                , ("dob_child2",  DF.schemaType @Text)
                                , ("name_child1", DF.schemaType @Text)
                                , ("name_child2", DF.schemaType @Text) ])
                 (raw <> "household.csv")
  putStrLn "== household (列名に変数名 dob/name と変数値 1/2 が混在) =="
  print household
  let hhLong = pivotLongerValueG "_" "child" True ["family"]
                 ["dob_child1","dob_child2","name_child1","name_child2"] household
  putStrLn "== pivot_longer(.value): dob/name は列に・child1/child2 は child 値に =="
  print hhLong

  -- === pivot_wider: table2 (long) を整然形に ===
  -- R: table2 |> pivot_wider(names_from = type, values_from = count)
  putStrLn "== pivot_wider(table2): type(cases/population) を列に → table1 と同形 =="
  print (pivotWiderG ["country","year"] "type" "count" table2)

  -- === pivot_wider + id_cols: cms_patient_experience ===
  -- R: cms_patient_experience |> pivot_wider(id_cols=starts_with("org"),
  --      names_from=measure_cd, values_from=prf_rate)
  cms <- DF.readCsvWithSchema
           (DF.makeSchema [ ("org_pac_id",    DF.schemaType @Text)
                          , ("org_nm",        DF.schemaType @Text)
                          , ("measure_cd",    DF.schemaType @Text)
                          , ("measure_title", DF.schemaType @Text)
                          , ("prf_rate",      DF.schemaType @Double) ])
           (raw <> "cms_patient_experience.csv")
  -- R: cms |> distinct(measure_cd, measure_title)
  let cds = nubKeep (zip (map cellText (V.toList (readCells "measure_cd" cms)))
                         (map cellText (V.toList (readCells "measure_title" cms))))
  putStrLn "== distinct(measure_cd, measure_title) =="
  mapM_ (\(c,t) -> putStrLn ("  " <> T.unpack c <> "  " <> T.unpack t)) cds
  let cmsWide = pivotWiderG ["org_pac_id","org_nm"] "measure_cd" "prf_rate" cms
  putStrLn $ "== pivot_wider(cms, id_cols=org): " <> show (DF.dimensions cms)
               <> " → " <> show (DF.dimensions cmsWide) <> " =="
  print (cmsWide |> DF.take 8)

  -- === pivot_wider の仕組み (toy df) と重複セルの検出 ===
  -- R: df <- tribble(~id,~measurement,~value, "A","bp1",100, "B","bp1",140,
  --      "B","bp2",115, "A","bp2",120, "A","bp3",105)
  let toyW = DF.fromNamedColumns
               [ ("id",          DF.fromList (["A","B","B","A","A"] :: [Text]))
               , ("measurement", DF.fromList (["bp1","bp1","bp2","bp2","bp3"] :: [Text]))
               , ("value",       DF.fromList ([100,140,115,120,105] :: [Int])) ]
  putStrLn "== pivot_wider(toy): 欠けるセル (B,bp3) は NA =="
  print (pivotWiderG ["id"] "measurement" "value" toyW)

  -- R: 重複 (A,bp1 が 2 行) があると pivot_wider は list-column 警告を出す。
  --   本実装は型付き列なので list-column を作れない。 R4DS 推奨どおり
  --   group_by + summarize + filter(n>1) で重複箇所を検出して示す。
  let toyDup = [ ("A","bp1"::Text), ("A","bp1"), ("A","bp2"), ("B","bp1"), ("B","bp2") ] :: [(Text,Text)]
      dupCounts = M.toList (foldl' (\m k -> M.insertWith (+) k (1::Int) m) M.empty toyDup)
      dups      = [ (i,me,n) | ((i,me),n) <- dupCounts, n > 1 ]
  putStrLn "== 重複セルの検出 (group_by(id,measurement) |> summarize(n) |> filter(n>1)) =="
  mapM_ (\(i,me,n) -> putStrLn ("  id=" <> T.unpack i <> " measurement=" <> T.unpack me
                                  <> " n=" <> show n)) dups
