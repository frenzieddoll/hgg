-- | チュートリアル 10: 結合 (R4DS 2e Ch19 "Joins")
--   https://r4ds.hadley.nz/joins
--
--   複数のデータフレームを「キー」で繋ぐ join を学ぶ。
--   ・mutating join (left/inner/right/full): 一致する観測から変数を足す
--   ・filtering join (semi/anti): 一致の有無で行を絞る
--   ・non-equi join (cross/不等号/rolling/overlap): == 以外で照合する
--   この章は表操作が主役で R4DS が描く図は概念図のみ (実データの ggplot 図は 0 枚)。
--   dataframe の join は重複列を These 型に畳む等 dplyr と意味論が異なるため、
--   R の表出力 (year.x/year.y・NA 補填) を忠実再現すべく join を自前実装する
--   (CLAUDE.md「機能不足は実装で埋める」)。データは nycflights13 の 5 表 (全量)。
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Main (main) where

import           Control.DeepSeq           (force, NFData)
import           Control.Exception         (try, evaluate, SomeException)
import           System.IO.Unsafe          (unsafePerformIO)
import           Data.Maybe                (fromMaybe)
import           Data.List                 (nub)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
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

-- === 列抽出 (Missing.hs と同方式・型不一致は [] に倒れる安全版) =================

safeCol :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> Maybe [a]
safeCol name df = unsafePerformIO $ do
  r <- try (evaluate (force (DF.columnAsList (DF.col @a name) df)))
         :: IO (Either SomeException [a])
  pure (either (const Nothing) Just r)

colPlain :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [a]
colPlain n df = fromMaybe [] (safeCol @a n df)

colMaybe :: forall a. (DFC.Columnable a, NFData a) => Text -> DF.DataFrame -> [Maybe a]
colMaybe n df = fromMaybe [] (safeCol @(Maybe a) n df)

-- === join インデックス計算 (キー型 k は Ord で多相) ===========================

-- | left join: x の各行に対し、一致する y 行 index を列挙 (無ければ Nothing)。
--   1:多 にも対応 (一致が複数なら x 行は複製される = dplyr と同じ)。
leftIdx :: Ord k => [k] -> [k] -> [(Int, Maybe Int)]
leftIdx xs ys =
  let ym = M.fromListWith (flip (++)) [ (k, [j]) | (j, k) <- zip [0 ..] ys ]
  in concat [ case M.lookup k ym of
                Just js -> [ (i, Just j) | j <- js ]
                Nothing -> [ (i, Nothing) ]
            | (i, k) <- zip [0 ..] xs ]

-- | inner join: 一致した組のみ。
innerIdx :: Ord k => [k] -> [k] -> [(Int, Int)]
innerIdx xs ys = [ (i, j) | (i, Just j) <- leftIdx xs ys ]

-- | full join: left の全組 + y 側で未一致だった行 (x 側 NA)。
fullIdx :: Ord k => [k] -> [k] -> [(Maybe Int, Maybe Int)]
fullIdx xs ys =
  let lp       = leftIdx xs ys
      matchedY = S.fromList [ j | (_, Just j) <- lp ]
      unm      = [ (Nothing, Just j) | (j, _) <- zip [0 ..] ys, not (S.member j matchedY) ]
  in [ (Just i, mj) | (i, mj) <- lp ] ++ unm

-- | right join: y の全行を保持し、一致する x を付ける (x 順に並べ末尾に余り)。
rightIdx :: Ord k => [k] -> [k] -> [(Maybe Int, Int)]
rightIdx xs ys = [ (mi, j) | (j, mi) <- leftIdx ys xs ]

-- | semi join: y に一致がある x の index。
semiIdx :: Ord k => [k] -> [k] -> [Int]
semiIdx xs ys = let s = S.fromList ys in [ i | (i, k) <- zip [0 ..] xs, S.member k s ]

-- | anti join: y に一致が無い x の index。
antiIdx :: Ord k => [k] -> [k] -> [Int]
antiIdx xs ys = let s = S.fromList ys in [ i | (i, k) <- zip [0 ..] xs, not (S.member k s) ]

-- === y 列の取り出し (Maybe Int の index で・未一致は NA) =======================

-- | 通常の y 列を Maybe index で引く (未一致 → Nothing)。
pickJust :: V.Vector a -> [Maybe Int] -> [Maybe a]
pickJust v = map (fmap (v V.!))

-- | もともと Maybe な y 列を Maybe index で引く (未一致 or 元値 NA → Nothing)。
pickFlat :: V.Vector (Maybe a) -> [Maybe Int] -> [Maybe a]
pickFlat v = map (>>= (v V.!))

-- | x 行を index で再選択 (全列・型保持)。
rows :: [Int] -> DF.DataFrame -> DF.DataFrame
rows is = selectIndices (VU.fromList is)

-- === 表示ヘルパ ================================================================

-- | tibble 風フッタ。全行数・列数を出す (R の "# A tibble: N × C" 相当)。
dims :: DF.DataFrame -> IO ()
dims df = let (nr, nc) = DF.dimensions df
          in putStrLn $ "# 全 " ++ comma nr ++ " 行 × " ++ show nc ++ " 列\n"

-- | 大きな表は先頭 10 行のみ印字し、真の行数をフッタで示す。
printHead :: DF.DataFrame -> IO ()
printHead df = do
  print (DF.take 10 df)
  dims df

comma :: Int -> String
comma = reverse . go . reverse . show
  where go (a:b:c:d:rest) = a:b:c:',': go (d:rest)
        go xs             = xs

base :: FilePath
base = "../_data/_raw/"

-- 1 年 (2022・非閏) の通日。 rolling/overlap join 用。
cumDays :: [Int]
cumDays = scanl (+) 0 [31,28,31,30,31,30,31,31,30,31,30,31]

doy :: (Int, Int) -> Int
doy (m, d) = cumDays !! (m - 1) + d

main :: IO ()
main = do
  -- ============================================================================
  -- 1. キー (Keys)
  -- ============================================================================
  airlines <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "airlines.csv")
  airports <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "airports.csv")
  planes   <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "planes.csv")
  weather  <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "weather.csv")
  flights  <- DF.exclude ["rownames"] <$> DF.readCsv (base ++ "flights.csv")

  putStrLn "== airlines (主キー = carrier の 2 文字コード) =="
  print airlines

  putStrLn "== airports (主キー = faa の 3 文字コード) =="
  printHead airports

  putStrLn "== planes (主キー = tailnum 機体番号) =="
  printHead planes

  putStrLn "== weather (複合主キー = origin + time_hour) =="
  printHead weather

  -- 主キーの検証: count(key) して n>1 を探す (重複が無ければ主キーとして妥当)。
  let plTail = colPlain @Text "tailnum" planes
      dupTail = [ k | (k, n) <- M.toList (counts plTail), n > 1 ]
  putStrLn $ "== planes |> count(tailnum) |> filter(n>1): "
             ++ show (length dupTail) ++ " 件 (= tailnum は一意・主キー妥当) =="

  let wKey  = zip (colPlain @Text "origin" weather)
                  (colPlain @Text "time_hour" weather)
      dupWK = [ k | (k, n) <- M.toList (counts wKey), n > 1 ]
  putStrLn $ "== weather |> count(time_hour, origin) |> filter(n>1): "
             ++ show (length dupWK) ++ " 件 (= 複合主キー妥当) =="

  -- 主キーの欠損も確認。 planes/weather とも 0 件。
  let naTail = length (filter (== Nothing) (colMaybe @Text "tailnum" planes))
  putStrLn $ "== planes |> filter(is.na(tailnum)): " ++ show naTail ++ " 件 =="

  -- 代理キー: flights は time_hour+carrier+flight で一意か。
  let fKey  = zip3 (colPlain @Text "time_hour" flights)
                   (colPlain @Text "carrier"   flights)
                   (colPlain @Int  "flight"    flights)
      dupFK = [ k | (k, n) <- M.toList (counts fKey), n > 1 ]
  putStrLn $ "== flights |> count(time_hour, carrier, flight) |> filter(n>1): "
             ++ show (length dupFK) ++ " 件 (= 重複なし) =="

  -- だが「重複が無い」だけでは主キーの保証にならない例: airports の (alt, lat)。
  let aKey  = zip (colPlain @Int "alt" airports) (colPlain @Double "lat" airports)
      dupAK = [ k | (k, n) <- M.toList (counts aKey), n > 1 ]
  putStrLn $ "== airports |> count(alt, lat) |> filter(n>1): "
             ++ show (length dupAK) ++ " 件 (= alt+lat は主キーに不適) =="

  -- 行番号による単純な代理キー id を先頭に付与 (mutate(id=row_number(), .before=1))。
  let nF       = DF.nRows flights
      flightsId = DF.select (["id"] ++ DF.columnNames flights)
                $ DF.insertVector "id" (V.fromList [1 .. nF] :: V.Vector Int) flights
  putStrLn "== flights2 = flights |> mutate(id = row_number(), .before = 1) =="
  printHead flightsId

  -- ============================================================================
  -- 2. 基本の join (Basic joins) — mutating join: left_join
  -- ============================================================================
  -- 見やすいよう 6 変数に絞った flights2 を作る。
  let flights2 = DF.select ["year","time_hour","origin","dest","tailnum","carrier"] flights
  putStrLn "== flights2 (6 変数に射影) =="
  printHead flights2

  -- left_join(airlines): carrier をキーに会社名 name を足す (右端に追加)。
  let f2carr   = colPlain @Text "carrier" flights2
      alCarr   = colPlain @Text "carrier" airlines
      alNameV  = V.fromList (colPlain @Text "name" airlines)
      ix_al    = leftIdx f2carr alCarr
      joinAl   = DF.insertVector "name"
                   (V.fromList (pickJust alNameV (map snd ix_al)))
                   (rows (map fst ix_al) flights2)
  putStrLn "== flights2 |> left_join(airlines) =="
  printHead joinAl

  -- left_join(weather |> select(origin, time_hour, temp, wind_speed)):
  --   複合キー (origin, time_hour) で気温・風速を足す。
  let f2OT     = zip (colPlain @Text "origin" flights2) (colPlain @Text "time_hour" flights2)
      wOT      = zip (colPlain @Text "origin" weather)  (colPlain @Text "time_hour" weather)
      wTempV   = V.fromList (colMaybe @Double "temp" weather)
      wWindV   = V.fromList (colMaybe @Double "wind_speed" weather)
      ix_w     = leftIdx f2OT wOT
      joinW    = DF.insertVector "wind_speed" (V.fromList (pickFlat wWindV (map snd ix_w)))
               $ DF.insertVector "temp"       (V.fromList (pickFlat wTempV (map snd ix_w)))
               $ rows (map fst ix_w) flights2
  putStrLn "== flights2 |> left_join(weather |> select(origin, time_hour, temp, wind_speed)) =="
  printHead joinW

  -- left_join(planes |> select(tailnum, type, engines, seats)): tailnum で機材を足す。
  let f2Tail   = map (fromMaybe "") (colMaybe @Text "tailnum" flights2)
      plTailL  = colPlain @Text "tailnum" planes
      plTypeV  = V.fromList (colPlain @Text "type" planes)
      plEngV   = V.fromList (colPlain @Int  "engines" planes)
      plSeatV  = V.fromList (colPlain @Int  "seats" planes)
      ix_pl    = leftIdx f2Tail plTailL
      joinPl   = DF.insertVector "seats"   (V.fromList (pickJust plSeatV (map snd ix_pl)))
               $ DF.insertVector "engines" (V.fromList (pickJust plEngV  (map snd ix_pl)))
               $ DF.insertVector "type"    (V.fromList (pickJust plTypeV (map snd ix_pl)))
               $ rows (map fst ix_pl) flights2
  putStrLn "== flights2 |> left_join(planes |> select(tailnum, type, engines, seats)) =="
  printHead joinPl

  -- 一致しない行は新変数が NA になる例: tailnum == N3ALAA (planes に無い)。
  let f2idx_n3 = [ i | (i, t) <- zip [0 ..] f2Tail, t == "N3ALAA" ]
      f2_n3    = rows f2idx_n3 flights2
      n3Tail   = map (fromMaybe "") (colMaybe @Text "tailnum" f2_n3)
      ix_n3    = leftIdx n3Tail plTailL
      joinN3   = DF.insertVector "seats"   (V.fromList (pickJust plSeatV (map snd ix_n3)))
               $ DF.insertVector "engines" (V.fromList (pickJust plEngV  (map snd ix_n3)))
               $ DF.insertVector "type"    (V.fromList (pickJust plTypeV (map snd ix_n3)))
               $ rows (map fst ix_n3) f2_n3
  putStrLn "== filter(tailnum == \"N3ALAA\") |> left_join(planes ...): type/engines/seats が NA =="
  printHead joinN3

  -- 自然結合の落とし穴: left_join(planes) は year+tailnum を複合キーにしてしまう。
  --   flights$year (出発年) と planes$year (製造年) は意味が違うので一致せず NA 多数。
  let f2Year   = colPlain @Int "year" flights2
      plYearV  = V.fromList (colMaybe @Int "year" planes)
      plManuV  = V.fromList (colPlain @Text "manufacturer" planes)
      plModelV = V.fromList (colPlain @Text "model" planes)
      plEngiV  = V.fromList (colPlain @Text "engine" planes)
      plSpeedV = V.fromList (colMaybe @Int "speed" planes)
      natKeyX  = zip (map Just f2Year) f2Tail               -- (Just 2013, tailnum)
      natKeyY  = zip (colMaybe @Int "year" planes) plTailL  -- (製造年, tailnum)
      ix_nat   = leftIdx natKeyX natKeyY
      joinNat  = DF.insertVector "engine"       (V.fromList (pickJust plEngiV  (map snd ix_nat)))
               $ DF.insertVector "speed"        (V.fromList (pickFlat plSpeedV (map snd ix_nat)))
               $ DF.insertVector "seats"        (V.fromList (pickJust plSeatV  (map snd ix_nat)))
               $ DF.insertVector "engines"      (V.fromList (pickJust plEngV   (map snd ix_nat)))
               $ DF.insertVector "model"        (V.fromList (pickJust plModelV (map snd ix_nat)))
               $ DF.insertVector "manufacturer" (V.fromList (pickJust plManuV  (map snd ix_nat)))
               $ DF.insertVector "type"         (V.fromList (pickJust plTypeV  (map snd ix_nat)))
               $ rows (map fst ix_nat) flights2
  putStrLn "== flights2 |> left_join(planes) [自然結合 = year+tailnum]: 一致せず NA 多数 =="
  printHead joinNat

  -- join_by(tailnum) で明示: year は year.x (flights) と year.y (planes) に曖昧性解消。
  let baseTy   = DF.rename "year" "year.x" (rows (map fst ix_pl) flights2)
      joinTy   = DF.insertVector "engine"       (V.fromList (pickJust plEngiV  (map snd ix_pl)))
               $ DF.insertVector "speed"        (V.fromList (pickFlat plSpeedV (map snd ix_pl)))
               $ DF.insertVector "seats"        (V.fromList (pickJust plSeatV  (map snd ix_pl)))
               $ DF.insertVector "engines"      (V.fromList (pickJust plEngV   (map snd ix_pl)))
               $ DF.insertVector "model"        (V.fromList (pickJust plModelV (map snd ix_pl)))
               $ DF.insertVector "manufacturer" (V.fromList (pickJust plManuV  (map snd ix_pl)))
               $ DF.insertVector "type"         (V.fromList (pickJust plTypeV  (map snd ix_pl)))
               $ DF.insertVector "year.y"       (V.fromList (pickFlat plYearV  (map snd ix_pl)))
                 baseTy
  putStrLn "== flights2 |> left_join(planes, join_by(tailnum)): year.x / year.y に曖昧性解消 =="
  printHead joinTy

  -- 異なる列名でのキー指定: join_by(dest == faa) / join_by(origin == faa)。
  let f2Dest   = colPlain @Text "dest" flights2
      f2Orig   = colPlain @Text "origin" flights2
      apFaa    = colPlain @Text "faa" airports
      apNameV  = V.fromList (colPlain @Text "name" airports)
      apLatV   = V.fromList (colPlain @Double "lat" airports)
      apLonV   = V.fromList (colPlain @Double "lon" airports)
      apAltV   = V.fromList (colPlain @Int "alt" airports)
      addAirport ixp d =
          DF.insertVector "alt"  (V.fromList (pickJust apAltV  (map snd ixp)))
        $ DF.insertVector "lon"  (V.fromList (pickJust apLonV  (map snd ixp)))
        $ DF.insertVector "lat"  (V.fromList (pickJust apLatV  (map snd ixp)))
        $ DF.insertVector "name" (V.fromList (pickJust apNameV (map snd ixp)))
        $ rows (map fst ixp) d
  putStrLn "== flights2 |> left_join(airports, join_by(dest == faa)) =="
  printHead (addAirport (leftIdx f2Dest apFaa) flights2)
  putStrLn "== flights2 |> left_join(airports, join_by(origin == faa)) =="
  printHead (addAirport (leftIdx f2Orig apFaa) flights2)

  -- ============================================================================
  -- 2b. filtering join: semi_join / anti_join
  -- ============================================================================
  -- semi_join(flights2, faa == origin): 出発空港 (EWR/JFK/LGA) だけに airports を絞る。
  putStrLn "== airports |> semi_join(flights2, join_by(faa == origin)): 出発 3 空港 =="
  print (rows (semiIdx apFaa f2Orig) airports)

  -- semi_join(flights2, faa == dest): 就航先の空港だけに絞る。
  putStrLn "== airports |> semi_join(flights2, join_by(faa == dest)): 就航先空港 =="
  printHead (rows (semiIdx apFaa f2Dest) airports)

  -- anti_join(airports, dest == faa) |> distinct(dest):
  --   airports に無い就航先 (= 暗黙の欠損)。
  let missDest = nub [ d | (i, d) <- zip [0 ..] f2Dest, i `S.member` antiSet ]
        where antiSet = S.fromList (antiIdx f2Dest apFaa)
      destDF = DF.fromNamedColumns [("dest", DF.fromList missDest)]
  putStrLn "== flights2 |> anti_join(airports, join_by(dest == faa)) |> distinct(dest) =="
  print destDF

  -- anti_join(planes, tailnum) |> distinct(tailnum): planes に無い機体番号。
  let antiTset = S.fromList (antiIdx f2Tail plTailL)
      -- R の distinct(tailnum) は NA も 1 つの値として含む (= 722 行)。
      missTail = nub [ t | (i, t) <- zip [0 ..] (colMaybe @Text "tailnum" flights2)
                         , i `S.member` antiTset ] :: [Maybe Text]
      tailDF   = DF.fromNamedColumns [("tailnum", DF.fromList missTail)]
  putStrLn "== flights2 |> anti_join(planes, join_by(tailnum)) |> distinct(tailnum) =="
  printHead tailDF

  -- ============================================================================
  -- 3. join の仕組み (How do joins work?) — 小さな x, y で図解
  -- ============================================================================
  let xKey = [1,2,3 :: Int]; xVal = ["x1","x2","x3" :: Text]
      yKey = [1,2,4 :: Int]; yVal = ["y1","y2","y3" :: Text]
      xVV  = V.fromList xVal; yVV = V.fromList yVal
  putStrLn "== x =="; print (mk2 "key" xKey "val_x" xVal)
  putStrLn "== y =="; print (mk2 "key" yKey "val_y" yVal)

  -- inner join: キーが等しい行だけ。
  let iI = innerIdx xKey yKey
      innerDF = DF.fromNamedColumns
        [ ("key",   DF.fromList [ xKey !! i | (i,_) <- iI ])
        , ("val_x", DF.fromList [ xVal !! i | (i,_) <- iI ])
        , ("val_y", DF.fromList [ yVal !! j | (_,j) <- iI ]) ]
  putStrLn "== inner_join(x, y, join_by(key)) =="; print innerDF

  -- left join: x を全保持 (key=3 は val_y が NA)。
  let lI = leftIdx xKey yKey
      leftDF = DF.fromNamedColumns
        [ ("key",   DF.fromList [ xKey !! i | (i,_) <- lI ])
        , ("val_x", DF.fromList [ xVal !! i | (i,_) <- lI ])
        , ("val_y", DF.fromList (pickJust yVV (map snd lI))) ]
  putStrLn "== left_join(x, y): x 全行 (key=3 の val_y = NA) =="; print leftDF

  -- right join: y を全保持 (key=4 は val_x が NA)。
  let rI = rightIdx xKey yKey
      rightDF = DF.fromNamedColumns
        [ ("key",   DF.fromList [ yKey !! j | (_,j) <- rI ])
        , ("val_x", DF.fromList (pickJust xVV (map fst rI)))
        , ("val_y", DF.fromList [ yVal !! j | (_,j) <- rI ]) ]
  putStrLn "== right_join(x, y): y 全行 (key=4 の val_x = NA) =="; print rightDF

  -- full join: x または y にある行をすべて保持。
  let fI = fullIdx xKey yKey
      keyOf (Just i,  _)      = xKey !! i
      keyOf (Nothing, Just j) = yKey !! j
      keyOf (Nothing, Nothing) = error "full join: 行に x も y も無い (到達不能)"
      fullDF = DF.fromNamedColumns
        [ ("key",   DF.fromList (map keyOf fI))
        , ("val_x", DF.fromList (pickJust xVV (map fst fI)))
        , ("val_y", DF.fromList (pickJust yVV (map snd fI))) ]
  putStrLn "== full_join(x, y): 4 行 (key=2 の一方や key=4 で NA) =="; print fullDF

  -- 多対多 (many-to-many): df1, df2 でキー 2 が双方 2 行 → 組合せ爆発。
  let d1k = [1,2,2 :: Int]; d1v = ["x1","x2","x3" :: Text]
      d2k = [1,2,2 :: Int]; d2v = ["y1","y2","y3" :: Text]
      mmI = innerIdx d1k d2k
      mmDF = DF.fromNamedColumns
        [ ("key",   DF.fromList [ d1k !! i | (i,_) <- mmI ])
        , ("val_x", DF.fromList [ d1v !! i | (i,_) <- mmI ])
        , ("val_y", DF.fromList [ d2v !! j | (_,j) <- mmI ]) ]
  putStrLn "== df1 |> inner_join(df2, join_by(key)): 多対多 = 5 行 (key=2 が 2×2) =="
  print mmDF

  -- ============================================================================
  -- 4. 非等値 join (Non-equi joins)
  -- ============================================================================
  -- keep = TRUE で両キーを残す (key.x / key.y)。
  let kbI = innerIdx xKey yKey
      keepDF = DF.fromNamedColumns
        [ ("key.x", DF.fromList [ xKey !! i | (i,_) <- kbI ])
        , ("val_x", DF.fromList [ xVal !! i | (i,_) <- kbI ])
        , ("key.y", DF.fromList [ yKey !! j | (_,j) <- kbI ])
        , ("val_y", DF.fromList [ yVal !! j | (_,j) <- kbI ]) ]
  putStrLn "== x |> inner_join(y, join_by(key == key), keep = TRUE) =="; print keepDF

  -- cross join: 全組合せ (デカルト積)。 名前の全ペアを作る (self-join)。
  let names = ["John","Simon","Tracy","Max" :: Text]
      crossDF = DF.fromNamedColumns
        [ ("name.x", DF.fromList [ a | a <- names, _ <- names ])
        , ("name.y", DF.fromList [ b | _ <- names, b <- names ]) ]
  putStrLn "== df |> cross_join(df): 4×4 = 16 行 =="; printHead crossDF

  -- 不等号 join: join_by(id < id) で「全組合せ」でなく「全組」を作る。
  let ids   = [1,2,3,4 :: Int]
      ltI   = [ (i,j) | (i,a) <- zip [0..] ids, (j,b) <- zip [0..] ids, a < b ]
      ltDF  = DF.fromNamedColumns
        [ ("id.x",   DF.fromList [ ids !! i   | (i,_) <- ltI ])
        , ("name.x", DF.fromList [ names !! i | (i,_) <- ltI ])
        , ("id.y",   DF.fromList [ ids !! j   | (_,j) <- ltI ])
        , ("name.y", DF.fromList [ names !! j | (_,j) <- ltI ]) ]
  putStrLn "== df |> inner_join(df, join_by(id < id)): 6 行 (全組) =="; print ltDF

  -- rolling join / overlap join (誕生日とパーティ)。
  -- ※R 原文は set.seed(123) + babynames で 100 名を乱択。 R の RNG は外部再現
  --   できないため、 本章は代表的な固定ロスター 10 名で join ロジックを忠実に示す
  --   (置換するのは乱数入力のみ・join の方式は同一)。
  -- ※end の q2 は「修正前 (2022-07-11)」を載せる。 R4DS は Hadley のデータ入力ミスを
  --   そのまま示し、 overlap 自己結合で重なりを検出 → 後で 07-10 に修正、 という流れ。
  let parties =
        [ (1::Int, "2022-01-10", (1,10),  "2022-01-01", (1,1),  "2022-04-03", (4,3))
        , (2,      "2022-04-04", (4,4),   "2022-04-04", (4,4),  "2022-07-11", (7,11))
        , (3,      "2022-07-11", (7,11),  "2022-07-11", (7,11), "2022-10-02", (10,2))
        , (4,      "2022-10-03", (10,3),  "2022-10-03", (10,3), "2022-12-31", (12,31)) ]
      -- between 用は修正後 (q2 end = 2022-07-10)。
      pEdoyFixed = [ doy (4,3), doy (7,10), doy (10,2), doy (12,31) ]
      pQ     = [ q       | (q,_,_,_,_,_,_) <- parties ]
      pParty = [ p       | (_,p,_,_,_,_,_) <- parties ]
      pPdoy  = [ doy md  | (_,_,md,_,_,_,_) <- parties ]
      pStart = [ s       | (_,_,_,s,_,_,_) <- parties ]
      pSdoy  = [ doy md  | (_,_,_,_,md,_,_) <- parties ]
      pEnd   = [ e       | (_,_,_,_,_,e,_) <- parties ]
      pEdoy  = [ doy md  | (_,_,_,_,_,_,md) <- parties ]
      employees =
        [ ("Hazel",   "2022-01-03", (1,3))
        , ("Lily",    "2022-02-14", (2,14))
        , ("Oscar",   "2022-03-21", (3,21))
        , ("Ada",     "2022-04-04", (4,4))
        , ("Ivan",    "2022-05-30", (5,30))
        , ("Mei",     "2022-07-11", (7,11))
        , ("Noah",    "2022-08-19", (8,19))
        , ("Priya",   "2022-10-03", (10,3))
        , ("Quinn",   "2022-11-25", (11,25))
        , ("Theo",    "2022-12-31", (12,31)) ]
      eName  = [ n      | (n,_,_) <- employees ]
      eBday  = [ b      | (_,b,_) <- employees ]
      eBdoy  = [ doy md | (_,_,md) <- employees ]

  putStrLn "== parties (= 四半期ごとのパーティ日) =="
  print (DF.fromNamedColumns
    [ ("q",     DF.fromList pQ)
    , ("party", DF.fromList (map T.pack pParty))
    , ("start", DF.fromList (map T.pack pStart))
    , ("end",   DF.fromList (map T.pack pEnd)) ])

  putStrLn "== employees (固定ロスター 10 名・R は乱択 100 名) =="
  print (DF.fromNamedColumns
    [ ("name",     DF.fromList (map T.pack eName))
    , ("birthday", DF.fromList (map T.pack eBday)) ])

  -- rolling: left_join(parties, join_by(closest(birthday >= party)))
  --   各従業員に「誕生日以前で最も近いパーティ」を割り当てる。
  let closestParty bd = case [ (pd, k) | (pd, k) <- zip pPdoy [0..], pd <= bd ] of
                          [] -> Nothing
                          ps -> Just (snd (maximum ps))
      rollK = map closestParty eBdoy
      rollDF = DF.fromNamedColumns
        [ ("name",     DF.fromList (map T.pack eName))
        , ("birthday", DF.fromList (map T.pack eBday))
        , ("q",        DF.fromList (map (fmap (pQ !!)) rollK))
        , ("party",    DF.fromList (map (fmap (T.pack . (pParty !!))) rollK)) ]
  putStrLn "== employees |> left_join(parties, join_by(closest(birthday >= party))) =="
  print rollDF

  -- 1/10 より前の誕生日はパーティが付かない (anti_join で確認)。
  let noParty = [ i | (i, k) <- zip [0..] rollK, k == Nothing ]
      noPartyDF = DF.fromNamedColumns
        [ ("name",     DF.fromList [ T.pack (eName !! i) | i <- noParty ])
        , ("birthday", DF.fromList [ T.pack (eBday !! i) | i <- noParty ]) ]
  putStrLn "== employees |> anti_join(parties, join_by(closest(birthday >= party))): 1/10 前 =="
  print noPartyDF

  -- overlap 自己結合で期間の重なりを検査 (overlaps(start,end,start,end), q < q)。
  --   start/end の最初の版 (end 修正前) では Q2,Q3 が境界で重なる。
  let ov = [ (a, b)
           | (a, sa, ea, qa) <- zip4 [0..] pSdoy pEdoy pQ
           , (b, sb, eb, qb) <- zip4 [0..] pSdoy pEdoy pQ
           , qa < qb, sa <= eb, ea >= sb ]
      ovDF = DF.fromNamedColumns
        [ ("start.x", DF.fromList [ T.pack (pStart !! a) | (a,_) <- ov ])
        , ("end.x",   DF.fromList [ T.pack (pEnd   !! a) | (a,_) <- ov ])
        , ("start.y", DF.fromList [ T.pack (pStart !! b) | (_,b) <- ov ])
        , ("end.y",   DF.fromList [ T.pack (pEnd   !! b) | (_,b) <- ov ]) ]
  putStrLn "== parties |> inner_join(parties, join_by(overlaps(...), q < q)): 重なり検査 =="
  print ovDF

  -- between(birthday, start, end) で各従業員をパーティに割り当てる (期間版)。
  let betweenK bd = case [ k | (s, e, k) <- zip3 pSdoy pEdoyFixed [0..], s <= bd, bd <= e ] of
                      (k:_) -> Just k
                      []    -> Nothing
      betDF = DF.fromNamedColumns
        [ ("name",     DF.fromList (map T.pack eName))
        , ("birthday", DF.fromList (map T.pack eBday))
        , ("q",        DF.fromList (map (fmap (pQ !!) . betweenK) eBdoy))
        , ("party",    DF.fromList (map (fmap (T.pack . (pParty !!)) . betweenK) eBdoy)) ]
  putStrLn "== employees |> inner_join(parties, join_by(between(birthday, start, end))) =="
  print betDF

  putStrLn "\n(join 章: 図は概念図のみ・実データ図は無し。 表出力で忠実再現。)"

-- === 小ヘルパ =================================================================

counts :: Ord k => [k] -> M.Map k Int
counts = M.fromListWith (+) . map (\k -> (k, 1))

mk2 :: (DFC.Columnable a, DFC.Columnable b) => Text -> [a] -> Text -> [b] -> DF.DataFrame
mk2 n1 c1 n2 c2 = DF.fromNamedColumns [(n1, DF.fromList c1), (n2, DF.fromList c2)]

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []
