-- |
-- Module      : Graphics.Hgg.Layout.RangeOf
-- Description : Layer 2 ─ MarkKind 別 x/y axis range 寄与の計算
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 各 chart 種類 (MarkKind) は y/x domain の決め方が異なる:
--
--   * scatter / line / errorbar / regression : encY = 値、 そのまま domain
--   * bar / waterfall                         : encY = 値、 domain は 0-base + max
--   * histogram / density                     : encY 無し、 domain は count / KDE peak
--   * box                                     : domain は Tukey whisker 範囲 (outlier 除外)
--   * violin / strip / swarm / raincloud / ridge : encY = 値、 min-max
--   * autocorr                                : x = [0, maxLag]、 y = [-1, 1]
--   * ess                                     : x = [0, nChain]、 y = [0, N/nChain]
--
-- これらを 1 箇所 ('Graphics.Hgg.Layout' の旧 paddedRange) で吸収しようとすると
-- chart 横断の副作用が出る (Phase 7 §0)。 本 module は MarkKind 別の range 寄与を
-- 分離し、 'computeLayout' は各 layer の寄与を集めるだけにする足場を提供する。
--
-- Phase 7 A2a: まず既存 'Graphics.Hgg.Layout' から range 計算を「挙動不変」 で
-- 抽出 (= 出力 byte 一致)。 paddedRange 特例の除去は A2b で行う。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Layout.RangeOf
  ( collectXY
  , histogramYRange
  , histRawDomain
  , lagXRange
  , lagYRange
  , isLagAxis
  , extentsOrDefault
  , qqPoints
  , invNormCdf
  , ecdfPoints
  ) where

import           Graphics.Hgg.Spec (ColData (..), ColorEnc (..), Layer (..), MarkKind (..),
                                    Position (..),
                                    Resolver, histBinning, lyBinCount, lyChain, lyColor, lyDensityNorm,
                                    lyEncX, lyEncY, lyEncY2, lyErrorX, lyHistDensity, lyKind,
                                    lyMaxLag, lyPosition, resolveCol, resolveNum,
                                    vsLayers, VisualSpec)
import           Data.List         (nub, sort)
import           Data.Monoid       (First (..), Last (..), getFirst, getLast)
import qualified Data.Text         as T
import           Data.Vector       (Vector)
import qualified Data.Vector       as V

-- ===========================================================================
-- 全 layer 横断の x/y range 収集
-- ===========================================================================

-- | 全 layer の encX / encY を resolve して連結。
--
-- Phase 6 A4/A5: MAutocorr / MEss は encX を「値ベクター」 として使うが、
-- x 軸の domain は **lag** (= 0..maxLag) なので、 encX を x として使うと壊れる。
-- 該当 mark を持つ layer は x = [0, maxLag]、 y = [-1, 1] (autocorr) or
-- y = ESS 範囲 (= encX の長さ近辺) を contribute する。
collectXY :: Resolver -> VisualSpec -> (Vector Double, Vector Double)
collectXY r spec =
  let -- ★ Phase 36 D3: 合成 Layer (base + overlay) を range 計算用に展開し、 各 overlay の値列も
      --   y 範囲に含める (distCols は別列が別 slot ゆえ全列の値域和が要る)。 単一列 (raincloud) は
      --   同一列なので union 不変 = byte 不変。
      explodeOverlay l = l { lyOverlay = [] } : lyOverlay l
      normalLayers = concatMap explodeOverlay (filter (not . isLagAxis) (vsLayers spec))
      lagLayers    = filter        isLagAxis  (vsLayers spec)
      -- Phase 28: histogram の x 範囲は生 encX 値ではなく **bin 外縁** を使う
      -- (= sharedHistXRange)。 生値だと ggplot 流 origin (boundary=w/2) が data 下端
      -- より下から始まる / 最終 bin 端が上端を超える分だけ外側 bar がパネル外に
      -- はみ出す (binwidth 大で顕著)。 y 軸が MHistogram を除外するのと同じ扱い。
      xsNormal = V.concat
        [ v | l <- normalLayers
            , not (isHistogram l)
            , Just cr <- [getLast (lyEncX l)]
            , Just v  <- [resolveNum r cr] ]
      -- y range は MarkKind ごとに別計算 (= box は Tukey、 density は KDE peak、 hist は count、 等)
      -- - encY 経由値が意味ある chart: scatter / line / errorbar / regression 等
      -- - 自前計算: box / density / histogram / bar / waterfall (= histogramYRange に集約)
      ysFromEncY l = case getFirst (lyKind l) of
        Just MBox       -> V.empty  -- yRangeForMark (= histogramYRange) が Tukey 範囲を返す
        Just MDensity   -> V.empty
        Just MHistogram -> V.empty
        Just MBar       -> V.empty
        Just MWaterfall -> V.empty
        Just MStream    -> V.empty  -- streamYRange が中心化積層 [-M/2, M/2] を返す
        Just MEcdf      -> V.fromList [0, 1]  -- ECDF の y は確率 [0,1] (encY 無し)
        _ -> case getLast (lyEncY l) of
               Just cr -> maybe V.empty id (resolveNum r cr)
               Nothing -> V.empty
      ysNormal = V.concat (map ysFromEncY normalLayers)
      -- Histogram の y 軸は count (= bins から得る)、 encY 経由ではない。
      -- MDensity / MBar / MWaterfall / MBox は従来通り layer 別。 MHistogram のみ
      -- Phase 8 B7: 全 hist layer 共通 bin での maxCount を取る (= render が共通 bin で
      -- 描くので range も共通 bin で計算しないと wide-form でバーが y 上端を超える)。
      nonHistY = V.concat (map (histogramYRange r) (filter (not . isHistogram) normalLayers))
      histYs = nonHistY V.++ sharedHistYRange r (filter isHistogram normalLayers)
      -- Phase 11 A6-4b: linerange/pointrange/crossbar は y±err を range に含める
      prYs = V.concat (map (rangeBarYRange r) normalLayers)
      -- Phase 15 A8: MBand (area band) は encY=下境界 / encY2=上境界。 ysFromEncY は
      -- 下境界しか拾わないので上境界を別途 range に含める (= 含めないと上側が
      -- plotArea からはみ出しクリップ。 GLM の非対称 μ-CI 帯で露見)。
      bandYs = V.concat (map (bandYRange r) normalLayers)
      -- Phase 52.D2: streamgraph は中心化積層なので各 x の群和の最大 M で [-M/2, M/2]
      streamYs = V.concat (map (streamYRange r) normalLayers)
      -- lag-axis (autocorr / ess) は x = [0, maxLag or chainCount]
      -- y = [-1, 1] (autocorr) or [0, N] (ess、 簡略は N で十分)
      lagXs = V.concat (map (lagXRange r) lagLayers)
      lagYs = V.concat (map (lagYRange r) lagLayers)
      -- Forest (Phase 8 B14): x = estimate ± error なので CI 端を range に含める
      -- (= 含めないと CI 線が plotArea からはみ出す)。 中央 null line x=0 も含める。
      forestXs = V.concat (map (forestXRange r) normalLayers)
      -- Q-Q plot (Phase 11 A6-2): x = 理論正規分位点 Φ⁻¹((i-0.5)/n) (列に無い算出値)
      qqXs = V.concat (map (qqXRange r) normalLayers)
      -- Phase 28: histogram の x 範囲 = 全 hist layer 共通 bin の外縁 (はみ出し fix)
      histXs = sharedHistXRange r (filter isHistogram normalLayers)
      -- nullable 列対応: NA は NaN で運ばれるので range 計算前に除く
      -- (min/max が NaN で汚染されないように)。 有限データには no-op。
      finite = V.filter (not . isNaN)
      xs = finite (xsNormal V.++ lagXs V.++ forestXs V.++ qqXs V.++ histXs)
      ys = finite (ysNormal V.++ lagYs V.++ histYs V.++ prYs V.++ bandYs V.++ streamYs)
  in (xs, ys)

-- ===========================================================================
-- MarkKind 別 y axis range 寄与
-- ===========================================================================

-- | MHistogram layer か。
isHistogram :: Layer -> Bool
isHistogram l = getFirst (lyKind l) == Just MHistogram

-- | Phase 8 B7: 全 histogram layer 共通の生 (pad なし) x domain (lo, hi)。
-- render (renderHistogram) と y-range 計算 (sharedHistYRange) が **同じ** bin 境界を
-- 使うための単一情報源。 これがズレると bin 幅が変わり count が食い違って
-- バーが y range を突き抜ける (Phase 8 B7 のはみ出しバグの原因)。
histRawDomain :: Resolver -> [Layer] -> Maybe (Double, Double)
histRawDomain r histLayers =
  let allXs = filter (not . isNaN)   -- NA (NaN) を除く (nullable 列対応・有限には no-op)
              $ concat [ V.toList v | l <- histLayers
                       , isHistogram l
                       , Just cr <- [getLast (lyEncX l)]
                       , Just v  <- [resolveNum r cr] ]
  in if null allXs then Nothing else Just (minimum allXs, maximum allXs)

-- | Phase 8 B7: 全 histogram layer 共通 bin での maxCount を y range に。
-- bin 境界は 'histRawDomain' (= 生 min/max) を単一情報源とし render と一致させる。
-- density mode は count/(N*binW) に正規化。 戻り値は [0, 全層通じての maxY]。
sharedHistYRange :: Resolver -> [Layer] -> Vector Double
sharedHistYRange r histLayers = case histRawDomain r histLayers of
  Nothing -> V.empty
  Just (lo, hi) ->
    let xsPerLayer = [ filter (not . isNaN) (V.toList v)  -- NA 除去 (render の vecOr と一致)
                     | l <- histLayers
                     , Just cr <- [getLast (lyEncX l)]
                     , Just v  <- [resolveNum r cr] ]
        isDensity l = case getLast (lyHistDensity l) of Just b -> b; Nothing -> False
        -- Phase 28: bin 化は Spec.histBinning に一元化。 layer 毎に binWidth/binCount を
        -- 解決し、 共有 domain (lo,hi) で counts を取る (render と同式)。
        layerMaxY l xs =
          let (origin, binW, nB) = histBinning l (lo, hi)
              binIx x = min (nB - 1) (max 0 (floor ((x - origin) / binW)))
              cs = foldl (\acc x -> let i = binIx x
                                     in take i acc <> [acc !! i + 1] <> drop (i+1) acc)
                         (replicate nB (0 :: Int)) xs
              maxC = fromIntegral (maximum (0 : cs)) :: Double
              totalN = fromIntegral (length xs) :: Double
          in if isDensity l && totalN > 0 && binW > 0
               then maxC / (totalN * binW)
               else maxC
        maxY = maximum (0 : [ layerMaxY l xs | (l, xs) <- zip histLayers xsPerLayer ])
    in V.fromList [0, maxY]

-- | Phase 28: 全 histogram layer 共通 bin の x 軸範囲 (= bin 外縁)。
-- ggplot 流 origin (boundary = w/2) は data 下端より下から始まり、 最終 bin 端は
-- data 上端を超えうるので、 x domain を生 data min/max で取ると外側の bar が
-- パネル外にはみ出す (binwidth 大で顕著・binwidth 小でも潜在)。 render
-- (renderHistogram) と同じ 'histBinning' (共有 domain) で各 layer の
-- [origin, origin + nBin*binW] を求め、 その union を返す。
sharedHistXRange :: Resolver -> [Layer] -> Vector Double
sharedHistXRange r histLayers = case histRawDomain r histLayers of
  Nothing -> V.empty
  Just (lo, hi) ->
    let extents = [ (origin, origin + fromIntegral nB * binW)
                  | l <- histLayers
                  , let (origin, binW, nB) = histBinning l (lo, hi) ]
    in case extents of
         [] -> V.empty
         _  -> V.fromList [ minimum (map fst extents)
                          , maximum (map snd extents) ]

-- | MHistogram / MDensity / MBar / MWaterfall の y 軸 range 候補。
-- bar 系 chart の bar base は y=0、 だから y domain は [0, max(value)] にする。
-- (= matplotlib / seaborn 慣例)
histogramYRange :: Resolver -> Layer -> Vector Double
histogramYRange r l = case getFirst (lyKind l) of
  -- Bar / Waterfall: encY の max + 0 を contribute (= base = 0、 上方が data max)
  -- Phase 9 B: stack/fill は積み上げ後の高さで domain を取る。 fill は常に [0,1]、
  --   stack は x カテゴリごとの群和の最大値。 dodge/identity は従来 [lo, max(value)]。
  Just MBar ->
    let pos = maybe PosIdentity id (getLast (lyPosition l))
    in case getLast (lyEncY l) of
    Just cr -> case resolveNum r cr of
      Just v | not (V.null v) -> case pos of
        PosFill  -> V.fromList [0, 1]
        PosStack ->
          let ys      = V.toList v
              xlabels = case getLast (lyEncX l) of
                Just crX -> case resolveCol r crX of
                  Just (TxtData lb) -> map show (V.toList lb)
                  Just (NumData lb) -> map (show . (round :: Double -> Int)) (V.toList lb)
                  _                 -> []
                Nothing -> []
          in if null xlabels
               then V.fromList [0, V.maximum v]   -- x ラベル無し → 単純 max
               else let sums = [ sum [ y | (xl, y) <- zip xlabels ys, xl == xc ]
                               | xc <- nub xlabels ]
                    in V.fromList [0, maximum (0 : sums)]
        _ ->
          let mx = V.maximum v
              mn = V.minimum v
              -- 負値を含む場合は [min, max]、 通常は [0, max]
              lo = if mn < 0 then mn else 0
          in V.fromList [lo, mx]
      _ -> V.empty
    Nothing -> V.empty
  -- Box: encY = 値、 y domain = Tukey whisker 範囲 (outlier 除外、 matplotlib 流)。
  -- Phase 8 C (box-grouped fix): encX で群分けされる場合は **群ごと**に Tukey 髭を出し
  -- その和集合を domain にする。 全群プールの髭だと高値群の髭が domain を超え枠外に
  -- 出ていた (= ユーザ報告)。 renderBox も群ごとに髭を描くので両者整合。
  Just MBox -> case getLast (lyEncY l) of
    Just cr -> case resolveNum r cr of
      Just v | not (V.null v) ->
        -- ★ NaN (= Maybe の Nothing) を群ラベルと整列したまま落とす (tukeyWhisker が
        --   NaN を含むと whisker が NaN 化し値軸レンジが壊れる)。 renderBox と整合。
        let vals   = V.toList v
            groups = case getLast (lyEncX l) of
              Just crX -> case resolveCol r crX of
                Just (TxtData labels) ->
                  let paired = [ (lb, x) | (lb, x) <- zip (V.toList labels) vals, not (isNaN x) ]
                  in groupValsBy (map fst paired) (map snd paired)
                Just (NumData labels) ->
                  let paired = [ (lb, x) | (lb, x) <- zip (map (show . (round :: Double -> Int)) (V.toList labels)) vals, not (isNaN x) ]
                  in groupValsBy (map fst paired) (map snd paired)
                _ -> [filter (not . isNaN) vals]
              Nothing -> [filter (not . isNaN) vals]
            whiskersOf g = let (lo, hi) = tukeyWhisker g in [lo, hi]
        in V.fromList (concatMap whiskersOf groups)
      _ -> V.empty
    Nothing -> V.empty
  -- Violin / Strip / Swarm / Raincloud / Ridge も同じく encY = 値
  Just MViolin     -> ifEncY l
  Just MStrip      -> ifEncY l
  Just MSwarm      -> ifEncY l
  Just MRaincloud  -> ifEncY l
  Just MRidge      -> ifEncY l
  Just MWaterfall -> case getLast (lyEncY l) of
    Just cr -> case resolveNum r cr of
      Just v | not (V.null v) ->
        -- waterfall は累積位置を考慮 (= scanl sum)
        let xs = V.toList v
            cumulative = scanl (+) 0 xs
            hiW = maximum (0 : cumulative)
            loW = minimum (0 : cumulative)
        in V.fromList [loW, hiW]
      _ -> V.empty
    Nothing -> V.empty
  Just MHistogram -> case getLast (lyEncX l) of
    Just cr -> case resolveNum r cr of
      Just v | not (V.null v) ->
        let xs = V.toList v
            -- Phase 28: bin 化は Spec.histBinning に一元化 (binWidth 優先)。
            (origin, binW, nB) = histBinning l (minimum xs, maximum xs)
            isDensity = case getLast (lyHistDensity l) of
              Just b  -> b
              Nothing -> False
            binIx x = min (nB - 1) (max 0 (floor ((x - origin) / binW)))
            counts = foldl (\acc x -> let i = binIx x
                                       in take i acc <> [acc !! i + 1] <> drop (i+1) acc)
                            (replicate nB (0 :: Int)) xs
            maxC :: Double
            maxC = fromIntegral (maximum counts)
            totalN = fromIntegral (V.length v) :: Double
            maxY = if isDensity && totalN > 0 && binW > 0
                     then maxC / (totalN * binW)
                     else maxC
        in V.fromList [0, maxY]
      _ -> V.empty
    Nothing -> V.empty
  -- Ch10 EDA (Phase 28): freqpoly は histogram と同じ bin で count を取り bin 中心を
  -- 折れ線で結ぶ。 y domain = [0, max count]。 color 群分割時は群ごとに独立 bin count を
  -- 取り maxY の最大を採る (= 描画 renderFreqPoly と整合・はみ出し防止)。 bin 境界は
  -- 全 xs (= 群共通) で決める (ggplot geom_freqpoly 同様)。 density mode は群 N で正規化。
  Just MFreqPoly -> case getLast (lyEncX l) of
    Just cr -> case resolveNum r cr of
      Just v | not (V.null v) ->
        let xs = V.toList v
            (origin, binW, nB) = histBinning l (minimum xs, maximum xs)
            isDensity = getLast (lyHistDensity l) == Just True
            binIx x = min (nB - 1) (max 0 (floor ((x - origin) / binW)))
            groups = case getLast (lyColor l) of
              Just (ColorByCol gcr) ->
                case fmap colDataKeys (resolveCol r gcr) of
                  Just ks | length ks == length xs -> map snd (orderedGroupsR ks xs)
                  _                                -> [xs]
              _ -> [xs]
            groupMaxY g =
              let cs = foldl (\acc x -> let i = binIx x
                                         in take i acc <> [acc !! i + 1] <> drop (i+1) acc)
                              (replicate nB (0 :: Int)) g
                  maxC = fromIntegral (maximum (0 : cs)) :: Double
                  gN   = fromIntegral (length g) :: Double
              in if isDensity && gN > 0 && binW > 0 then maxC / (gN * binW) else maxC
            maxY = maximum (0 : map groupMaxY groups)
        in V.fromList [0, maxY]
      _ -> V.empty
    Nothing -> V.empty
  -- Phase 8 B16: densityNorm (pairs 対角) は y 軸 = 値範囲 (= 行変数値)。 KDE は描画側で
  -- panel 高さに独立正規化するので、 y range は値の min-max を返す。
  Just MDensity | getLast (lyDensityNorm l) == Just True ->
    case getLast (lyEncX l) of
      Just cr -> case resolveNum r cr of
        Just v0 | let v = V.filter (not . isNaN) v0, not (V.null v) ->
          V.fromList [V.minimum v, V.maximum v]
        _ -> V.empty
      Nothing -> V.empty
  Just MDensity -> case getLast (lyEncX l) of
    Just cr -> case resolveNum r cr of
      Just v | not (V.null v) ->
        -- 実 KDE を 50 grid で計算し peak を求める (= y domain = [0, peak])。
        -- 群別 density (lyColor = ColorByCol) なら各群を独立正規化して描くため、
        -- y domain は群ごとの peak の最大を採る (= はみ出し防止。 描画 renderDensity と整合)。
        -- ★ NaN (= Maybe の Nothing) は群キーと整列したまま落とす (resolveNum は NaN を含む)。
        let xsFull = V.toList v
            groups = case getLast (lyColor l) of
              Just (ColorByCol gcr) ->
                case fmap colDataKeys (resolveCol r gcr) of
                  Just ks | length ks == length xsFull ->
                    let paired = [ (k, x) | (k, x) <- zip ks xsFull, not (isNaN x) ]
                    in [ [ x | (k', x) <- paired, k' == k ] | k <- nub (map fst paired) ]
                  _ -> [filter (not . isNaN) xsFull]
              _ -> [filter (not . isNaN) xsFull]
            peak = maximum (1e-12 : [ kdePeakR g | g <- groups, length g >= 2 ])
        in V.fromList [0, peak]
      _ -> V.empty
    Nothing -> V.empty
  _ -> V.empty
  where
    -- ColData を群キー列 [Text] に (TxtData そのまま / NumData は show)。
    colDataKeys (TxtData vs) = V.toList vs
    colDataKeys (NumData vs) = map (T.pack . show) (V.toList vs)
    -- キー初出順で群化 (= Common.orderedGroups と同等・依存回避のため局所定義)。
    orderedGroupsR keys vals =
      let paired = zip keys vals
      in [ (k, [ x | (k', x) <- paired, k' == k ]) | k <- nub keys ]
    -- 1 群分の KDE peak (= y domain の上端候補)。 Silverman bw・50 grid。
    kdePeakR xs =
      let n     = length xs
          mu    = sum xs / fromIntegral n
          sd    = sqrt (sum [(x - mu)^(2::Int) | x <- xs] / fromIntegral (max 1 (n - 1)))
          bw    = max (1.06 * sd * fromIntegral n ** (-0.2 :: Double)) 1e-9
          lo    = minimum xs
          hi    = maximum xs
          nGrid = 50 :: Int
          step  = if hi > lo then (hi - lo) / fromIntegral (nGrid - 1) else 1
          kdeAt x = sum [ exp (negate ((x - xi) ** 2) / (2 * bw ** 2))
                        | xi <- xs ] / (fromIntegral n * bw * sqrt (2 * pi))
      in maximum [ kdeAt (lo + fromIntegral i * step) | i <- [0 .. nGrid - 1] ]
    ifEncY layer = case getLast (lyEncY layer) of
      Just cr -> case resolveNum r cr of
        Just v | not (V.null v) -> V.fromList [V.minimum v, V.maximum v]
        _ -> V.empty
      Nothing -> V.empty

-- ===========================================================================
-- lag 軸 (autocorr / ess) の x/y range 寄与
-- ===========================================================================

-- | autocorr / ess layer の x 軸 range 候補 (= [0, maxLag] or [0, nChain])
lagXRange :: Resolver -> Layer -> Vector Double
lagXRange r l = case getFirst (lyKind l) of
  Just MAutocorr ->
    let maxLag = maybe 40 id (getLast (lyMaxLag l))
    in V.fromList [0, fromIntegral maxLag]
  Just MEss ->
    -- chain 列が指定されていれば distinct chain 数、 未指定なら 1
    let nChain = case getLast (lyChain l) of
          Just cr -> case resolveCol r cr of
            Just (TxtData v) -> length (uniqList (V.toList v))
            Just (NumData v) -> length (uniqList (V.toList v))
            Nothing          -> 1
          Nothing -> 1
    in V.fromList [0, fromIntegral (max 1 nChain)]
  _ -> V.empty
  where
    uniqList :: Eq a => [a] -> [a]
    uniqList = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | autocorr / ess layer の y 軸 range 候補
lagYRange :: Resolver -> Layer -> Vector Double
lagYRange r l = case getFirst (lyKind l) of
  Just MAutocorr -> V.fromList [-1.0, 1.0]
  Just MEss ->
    -- ESS は理論上 [0, N] だが実用上 N/4 程度が上限 (= 強い autocorrelation で更に小)。
    -- chain 数があれば N/nChain を上限に。
    let n = case getLast (lyEncX l) of
          Just cr -> case resolveNum r cr of
            Just v  -> V.length v
            Nothing -> 1000
          Nothing -> 1000
        nChain = case getLast (lyChain l) of
          Just cr -> case resolveCol r cr of
            Just (TxtData v) -> max 1 (length (uniqList (V.toList v)))
            Just (NumData v) -> max 1 (length (uniqList (V.toList v)))
            Nothing          -> 1
          Nothing -> 1
        upper = fromIntegral (n `div` nChain)
    in V.fromList [0, upper]
  _ -> V.empty
  where
    uniqList :: Eq a => [a] -> [a]
    uniqList = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | Forest layer の x range 寄与 (= estimate ± error + 中央 null line x=0)。
-- これを range に含めないと CI 線が plotArea からはみ出す (Phase 8 B14)。
forestXRange :: Resolver -> Layer -> Vector Double
forestXRange r l = case getFirst (lyKind l) of
  Just MForest ->
    let ests = maybe V.empty id (getLast (lyEncX l) >>= resolveNum r)
        errs = case getLast (lyErrorX l) of
          Just cr -> maybe V.empty id (resolveNum r cr)
          Nothing -> V.empty
        los = V.zipWith (-) ests errs
        his = V.zipWith (+) ests errs
    in if V.null ests then V.empty
       else V.fromList [0] V.++ los V.++ his  -- null line x=0 も含める
  _ -> V.empty

-- | Phase 11 A6-4b: linerange / pointrange / crossbar の y 軸 range 寄与 = y ± errorY
-- (= forest の x ± err と同型)。 これが無いと区間 (y±err) が plotArea からはみ出す。
rangeBarYRange :: Resolver -> Layer -> Vector Double
rangeBarYRange r l = case getFirst (lyKind l) of
  Just k | k `elem` [MLineRange, MPointRange, MCrossbar] ->
    let ys   = maybe V.empty id (getLast (lyEncY l) >>= resolveNum r)
        errs = case getLast (lyErrorY l) of
          Just cr -> maybe V.empty id (resolveNum r cr)
          Nothing -> V.empty
        los = V.zipWith (-) ys errs
        his = V.zipWith (+) ys errs
    in if V.null ys then V.empty else los V.++ his
  _ -> V.empty

-- | Phase 15 A8: MBand (area band) の y 軸 range 寄与 = 上境界 encY2。
-- 下境界 encY は collectXY の ysFromEncY が既に拾う。 上境界 encY2 はどこも拾わ
-- ないため、 これが無いと帯の上側が plotArea からはみ出してクリップされる
-- (GLM の非対称 μ-CI 帯で露見)。
bandYRange :: Resolver -> Layer -> Vector Double
bandYRange r l = case getFirst (lyKind l) of
  Just MBand ->
    let los = maybe V.empty id (getLast (lyEncY  l) >>= resolveNum r)
        his = maybe V.empty id (getLast (lyEncY2 l) >>= resolveNum r)
    in los V.++ his
  _ -> V.empty

-- | Phase 52.D2: streamgraph の y 軸 range 寄与。 各 x 値ごとに全系列の y を合算した
-- 総和 total(x) の最大 M を取り、 中心化 (silhouette: baseline=-Σy/2) ゆえ [-M/2, M/2]
-- を返す。 系列は color で分かれるが range には x ごとの総和だけが要る。
streamYRange :: Resolver -> Layer -> Vector Double
streamYRange r l = case getFirst (lyKind l) of
  Just MStream ->
    let xs = maybe [] V.toList (getLast (lyEncX l) >>= resolveNum r)
        ys = maybe [] V.toList (getLast (lyEncY l) >>= resolveNum r)
        n   = min (length xs) (length ys)
        pts = take n (zip xs ys)
        totals = [ sum [ y | (xx, y) <- pts, xx == ux ] | ux <- nub (map fst pts) ]
        m = maximum (0 : totals)
    in if n == 0 then V.empty else V.fromList [negate (m / 2), m / 2]
  _ -> V.empty

-- | Phase 11 A6-2: Q-Q plot の x 軸 range 寄与。 sample (encY) をソートして得る
-- order statistic に理論正規分位点 Φ⁻¹((i-0.5)/n) を割り当て、 その min/max を
-- x domain に contribute する (= 理論分位点は列に無いので forestXRange と同型で算出)。
qqXRange :: Resolver -> Layer -> Vector Double
qqXRange r l = case getFirst (lyKind l) of
  Just MQQ -> case getLast (lyEncY l) >>= resolveNum r of
    Just v | not (V.null v) ->
      let xs = map fst (qqPoints (V.toList v))
      in if null xs then V.empty else V.fromList [minimum xs, maximum xs]
    _ -> V.empty
  _ -> V.empty

-- | Phase 11 A6-2: サンプルから Q-Q plot の点列 (理論分位点, order statistic) を作る。
-- render と range が **同じ式** を使うための単一情報源 (= histRawDomain と同思想)。
-- y_(i) = ソート済 sample の i 番目、 x_i = Φ⁻¹((i-0.5)/n) (= plotting position、
-- ggplot stat_qq / R qqnorm の既定 (a=0.5 of Blom 近傍))。
qqPoints :: [Double] -> [(Double, Double)]
qqPoints sample =
  let ys = sort sample
      n  = length ys
  in [ (invNormCdf ((fromIntegral i - 0.5) / fromIntegral n), y)
     | (i, y) <- zip [(1 :: Int) ..] ys ]

-- | Phase 11 A6-4: ECDF (= ggplot stat_ecdf) の階段ポリライン頂点。 render と x/y range が
-- 同じ式を使うための単一情報源。 右連続の階段 F(x)=#(≤x)/n を、 角点列で表す:
--   (x_1,0), (x_1,1/n), (x_2,1/n), (x_2,2/n), …, (x_n, n/n)。 空入力は []。
ecdfPoints :: [Double] -> [(Double, Double)]
ecdfPoints sample =
  let xs = sort sample
      n  = length xs
      fn :: Int -> Double
      fn i = fromIntegral i / fromIntegral n
  in case xs of
       []      -> []
       (x0:_)  -> (x0, 0)
                  : concat [ (x, fn i)
                             : [ (xs !! i, fn i) | i < n ]  -- 次の x まで水平 (最後は無し)
                           | (i, x) <- zip [(1 :: Int) ..] xs ]

-- | 標準正規分布の逆累積分布関数 Φ⁻¹ (= probit / qnorm)。 Acklam の有理多項式近似
-- (相対誤差 < 1.15e-9)。 p ∈ (0,1) を仮定 (端点は ±∞ を返すが qqPoints では (0.5/n)
-- 〜((n-0.5)/n) なので 0/1 には到達しない)。
invNormCdf :: Double -> Double
invNormCdf p
  | p <= 0    = -1 / 0
  | p >= 1    =  1 / 0
  | p < pLow  =
      let q = sqrt (-2 * log p)
      in (((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)
         / ((((d1*q+d2)*q+d3)*q+d4)*q+1)
  | p <= pHigh =
      let q = p - 0.5
          rr = q * q
      in (((((a1*rr+a2)*rr+a3)*rr+a4)*rr+a5)*rr+a6)*q
         / (((((b1*rr+b2)*rr+b3)*rr+b4)*rr+b5)*rr+1)
  | otherwise =
      let q = sqrt (-2 * log (1 - p))
      in -(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)
         / ((((d1*q+d2)*q+d3)*q+d4)*q+1)
  where
    pLow  = 0.02425
    pHigh = 1 - pLow
    a1 = -3.969683028665376e+01; a2 =  2.209460984245205e+02
    a3 = -2.759285104469687e+02; a4 =  1.383577518672690e+02
    a5 = -3.066479806614716e+01; a6 =  2.506628277459239e+00
    b1 = -5.447609879822406e+01; b2 =  1.615858368580409e+02
    b3 = -1.556989798598866e+02; b4 =  6.680131188771972e+01
    b5 = -1.328068155288572e+01
    c1 = -7.784894002430293e-03; c2 = -3.223964580411365e-01
    c3 = -2.400758277161838e+00; c4 = -2.549732539343734e+00
    c5 =  4.374664141464968e+00; c6 =  2.938163982698783e+00
    d1 =  7.784695709041462e-03; d2 =  3.224671290700398e-01
    d3 =  2.445134137142996e+00; d4 =  3.754408661907416e+00

-- | autocorr / ess は encX を「値」 としてではなく lag/chain 軸として扱う layer。
isLagAxis :: Layer -> Bool
isLagAxis l = case getFirst (lyKind l) of
  Just MAutocorr -> True
  Just MEss      -> True
  _              -> False

-- ===========================================================================
-- 共通 helper
-- ===========================================================================

-- | Vector の (min, max)。 空なら default (0, 1)。
extentsOrDefault :: Vector Double -> (Double, Double)
extentsOrDefault v
  | V.null v  = (0, 1)
  | otherwise = (V.minimum v, V.maximum v)

-- | Phase 8 C (box-grouped fix): ラベル列で値を群分け (出現順、 extent 用)。
groupValsBy :: Eq a => [a] -> [Double] -> [[Double]]
groupValsBy labels vals =
  let pairs = zip labels vals
      uniq  = foldr (\(k, _) acc -> if k `elem` acc then acc else k : acc) [] pairs
  in [ [ x | (k, x) <- pairs, k == lab ] | lab <- uniq ]

-- | Tukey 髭 (loV, hiV) = fence [Q1-1.5IQR, Q3+1.5IQR] 内の最小/最大データ点。
-- renderBox の髭計算と同一式 (= 群ごとの箱と domain が整合)。
tukeyWhisker :: [Double] -> (Double, Double)
tukeyWhisker xs0 =
  let sorted = sort xs0
      n      = length sorted
      q p =
        let pos  = p * fromIntegral (n - 1)
            lo'  = floor pos :: Int
            frac = pos - fromIntegral lo'
        in case (atIdx sorted lo', atIdx sorted (lo' + 1)) of
             (Just a, Just b) -> a + (b - a) * frac
             (Just a, Nothing) -> a
             _                 -> 0
      atIdx xs i_ = if i_ < 0 || i_ >= length xs then Nothing else Just (xs !! i_)
      q1  = q 0.25
      q3  = q 0.75
      iqr = q3 - q1
      loW = q1 - 1.5 * iqr
      hiW = q3 + 1.5 * iqr
      loV = case dropWhile (< loW) sorted of (x:_) -> x; [] -> q1
      hiV = case reverse (takeWhile (<= hiW) sorted) of (x:_) -> x; [] -> q3
  in (loV, hiV)
