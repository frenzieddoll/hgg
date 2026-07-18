-- |
-- Module      : Graphics.Hgg.Render.MCMC
-- Description : MCMC 診断 mark (forest/funnel/autocorr/ess)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 7 A4: Render モノリス分割 (出力中立・純粋移動)。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphics.Hgg.Render.MCMC where

import           Graphics.Hgg.Layout (numToText,
                                      Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical)
import           Graphics.Hgg.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Graphics.Hgg.Spec   (Annotation (..), AxisFormat (..),
                                      ColData (..), ColRef,
                                      ColorEnc (..), ConnectSpec (..),
                                      DAGEdge (..), DAGLayoutAlgorithm (..),
                                      DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..), DAGSpec (..), Layer (..),
                                      LegendPosition (..), LegendSpec (..),
                                      Inset (..), MarginalSpec (..), MarkKind (..),
                                      MarkShape (..), ShapeMapEntry (..),
                                      LineType (..), lineTypeDash, lineTypeForIndex,
                                      ReferenceLine (..), Resolver,
                                      Position (..), Coord (..),
                                      FacetScales (..), freeScaleX, freeScaleY,
                                      FacetSpace (..), freeSpaceX, freeSpaceY,
                                      ThemeOverride (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axShowGrid,
                                      FontSpec (..),
                                      colRefName, resolveCol, resolveNum)
import           Data.Maybe          (mapMaybe, isJust)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Graphics.Hgg.Spec
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)

import           Graphics.Hgg.Primitive
import           Graphics.Hgg.Render.Common


-- ===========================================================================
-- Phase 6 A4: MCMC autocorrelation
-- ===========================================================================

-- | autocorrelation plot (P19、 Phase 6 A4): 1 列の時系列から lag-k 自己相関 r(τ)
-- を計算 + bar chart。 ±1.96/√N の significance band も併せて。
-- | Autocorrelation plot (Phase 8 B12): encX = 生サンプル列、 lyChain = chain (任意)。
-- chain ごとに ACF ρ(k), k=0..maxLag を計算し、 lag を横軸に chain 別の細い棒で描く
-- (= bayesplot mcmc_acf_bar 流: ACF は plot 内で計算)。 x=lag/y=相関 で軸転置のため
-- Layout scale に頼らず自前マッピング。
renderAutocorr :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderAutocorr r layout thePal ly =
  let xs     = V.toList (vecOr (lyEncX ly) r)
      maxLag = maybe 40 id (getLast (lyMaxLag ly))
      area   = lpPlotArea layout
      pal    = lpCategoricalPalette layout
      -- chain 分け (lyChain、 無ければ単一群)
      groups = case getLast (lyChain ly) of
        Just cr -> case resolveCol r cr of
          Just (TxtData cs) -> chainGroups (map T.unpack (V.toList cs)) xs
          Just (NumData cs) -> chainGroups (map show     (V.toList cs)) xs
          Nothing           -> [("all", xs)]
        Nothing -> [("all", xs)]
      nCh    = max 1 (length groups)
      -- 値 → pixel: x=lag (0..maxLag を plotArea 幅へ)、 y=相関 [-1,1] を高さへ
      slotW  = rW area / fromIntegral (maxLag + 1)
      barW   = max 1.5 (slotW / fromIntegral nCh * 0.7)
      sy v   = rY area + rH area - ((v - (-1)) / 2) * rH area
      base   = sy 0
      -- Phase 10 A4: value 軸 = 相関 [-1,1] (Cartesian 縦 sy / flip 横 valPxF)、 cross 軸 = lag
      -- (Cartesian 横 slot / flip 縦 slot・lag0 を下端)。 自前マッピングのまま coord で辺を入替。
      coord  = flipOnly (lpCoord layout)   -- A7-c: autocorr は polar 非対象
      slotV  = rH area / fromIntegral (maxLag + 1)
      barWV  = max 1.5 (slotV / fromIntegral nCh * 0.7)
      valPxF v = rX area + ((v + 1) / 2) * rW area
      baseF  = valPxF 0
      mkBar k ci rk = case coord of
        CoordCartesian ->
          let slotCx = rX area + (fromIntegral k + 0.5) * slotW
              cx = slotCx - slotW * 0.5 + (fromIntegral ci + 0.5) * (slotW / fromIntegral nCh) - barW/2
          in Rect cx (min (sy rk) base) barW (abs (sy rk - base))
        CoordFlip ->
          let slotCy = rY area + rH area - (fromIntegral k + 0.5) * slotV
              cyTop = slotCy - slotV * 0.5 + (fromIntegral ci + 0.5) * (slotV / fromIntegral nCh) - barWV/2
          in Rect (min (valPxF rk) baseF) cyTop (abs (valPxF rk - baseF)) barWV
      drawChain ci (_lbl, vs) =
        let col = pal !! (ci `mod` length pal)
            rs  = map (autocorrAt vs) [0 .. maxLag]
        in [ PRect (mkBar k ci rk) (FillStyle col 0.85) (Just (StrokeStyle col 0.5))
           | (k, rk) <- zip [0 :: Int ..] rs ]
      bars = concat (zipWith drawChain [0..] groups)
      -- significance band ±1.96/sqrt(N) (= 95% null) + 0 線。 value=t の参照線 (cross 軸全長)。
      nTot = length xs
      sg = if nTot < 2 then 0 else 1.96 / sqrt (fromIntegral nTot :: Double)
      valRefLine t = case coord of
        CoordCartesian -> (Point (rX area) (sy t), Point (rX area + rW area) (sy t))
        CoordFlip      -> (Point (valPxF t) (rY area), Point (valPxF t) (rY area + rH area))
      sigBand = (let (z1, z2) = valRefLine 0 in [ PLine z1 z2 (solid (tpAxis thePal) 1.0) ])
             ++ concat [ [ PLine a1 a2 (solid "#888" 0.8), PLine b1 b2 (solid "#888" 0.8) ]
                       | sg > 0, let (a1, a2) = valRefLine sg, let (b1, b2) = valRefLine (negate sg) ]
      -- value 軸目盛り (相関 -1..1。 Cartesian 左辺 / flip 下辺)
      valAnchor = case coord of CoordCartesian -> AnchorEnd; CoordFlip -> AnchorMiddle
      tsY = mkFontTS Nothing thePal TickF valAnchor 0
      yTicks = [ p | tv <- [-1.0, -0.5, 0, 0.5, 1.0]
                   , p <- case coord of
                       CoordCartesian ->
                         [ PLine (Point (rX area) (sy tv)) (Point (rX area - 5) (sy tv)) (solid (tpAxis thePal) 1.0)
                         , PText (Point (rX area - 8) (sy tv + 4)) (numToText tv) tsY ]
                       CoordFlip ->
                         [ PLine (Point (valPxF tv) (rY area + rH area)) (Point (valPxF tv) (rY area + rH area + 5)) (solid (tpAxis thePal) 1.0)
                         , PText (Point (valPxF tv) (rY area + rH area + 18)) (numToText tv) tsY ] ]
  in axisFrame layout thePal ++ yTicks ++ sigBand ++ bars
  where
    chainGroups :: [String] -> [Double] -> [(String, [Double])]
    chainGroups labels values =
      let pairs = zip labels values
          uniqLabels = foldr (\(l, _) acc -> if l `elem` acc then acc else l : acc) [] pairs
      in [ (l, [v | (lv, v) <- pairs, lv == l]) | l <- uniqLabels ]
    -- r(τ) = Σ(x_t - μ)(x_{t+τ} - μ) / Σ(x_t - μ)²
    autocorrAt :: [Double] -> Int -> Double
    autocorrAt vs k
      | length vs <= k = 0
      | otherwise =
          let mu      = sum vs / fromIntegral (length vs)
              centered = map (subtract mu) vs
              denom   = sum (map (^ (2 :: Int)) centered)
              pairs   = zip centered (drop k centered)
              num     = sum (map (uncurry (*)) pairs)
          in if denom == 0 then 0 else num / denom

-- ===========================================================================
-- Phase 6 A5: Effective Sample Size
-- ===========================================================================

-- | ESS plot (Phase 8 B13): encX = パラメータ/chain 名、 encY = 計算済み ESS 値。
-- ESS 計算は統計ライブラリの責務、 plot は値を棒にするだけ (= ggplot/bayesplot 流の
-- 計算と描画の分離)。 ESS 閾値 (100/400) で色分け (赤=低い/橙=中/緑=高い)。
-- x=名前/y=ESS で軸が転置するため Layout scale に頼らず自前マッピング。
renderESS :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderESS r layout thePal ly =
  let names = catLabelsOf r ly
      vals  = V.toList (vecOr (lyEncY ly) r)
      n     = min (length names) (length vals)
      pairs = take n (zip names vals)
      area  = lpPlotArea layout
      yMax  = maximum (100 : vals)   -- 最低 100 まで (= 閾値が見える)
      sy v  = if yMax <= 0 then rY area + rH area
              else rY area + rH area - v / yMax * rH area
      nB    = length pairs
      step  = if nB == 0 then 0 else rW area / fromIntegral nB
      stepV = if nB == 0 then 0 else rH area / fromIntegral nB
      barW  = step * 0.6
      -- Phase 10 A4: value 軸 = ESS 値 (Cartesian 縦 sy / flip 横 valPxF)、 cross 軸 = 名前
      -- (Cartesian 横 cx / flip 縦 cy・先頭を下端に)。 自前マッピングのまま coord で辺を入替。
      coord = flipOnly (lpCoord layout)   -- A7-c: ess は polar 非対象
      valPxF v = rX area + (if yMax <= 0 then 0 else v / yMax) * rW area
      cxFor i = rX area + (fromIntegral i + 0.5) * step
      cyFor i = rY area + rH area - (fromIntegral i + 0.5) * stepV
      mkBarRect i v = case coord of
        CoordCartesian -> Rect (cxFor i - barW/2) (sy v) barW (rY area + rH area - sy v)
        CoordFlip      -> Rect (rX area) (cyFor i - barW/2) (valPxF v - rX area) barW
      valRefLine t = case coord of
        CoordCartesian -> (Point (rX area) (sy t), Point (rX area + rW area) (sy t))
        CoordFlip      -> (Point (valPxF t) (rY area), Point (valPxF t) (rY area + rH area))
      catAnchor = case coord of CoordCartesian -> AnchorMiddle; CoordFlip -> AnchorEnd
      valAnchor = case coord of CoordCartesian -> AnchorEnd;    CoordFlip -> AnchorMiddle
      tsCat = mkFontTS Nothing thePal TickF catAnchor 0
      drawOne i (nm, v) =
        let col | v < 100   = "#d9534f"   -- 低い (要注意)
                | v < 400   = "#f0ad4e"   -- 中
                | otherwise = "#5cb85c"   -- 良い
            lblPt = case coord of
              CoordCartesian -> Point (cxFor i) (rY area + rH area + 16)
              CoordFlip      -> Point (rX area - 6) (cyFor i + 4)
        in [ PRect (mkBarRect i v) (FillStyle col 0.85) (Just (StrokeStyle col 0.5))
           , PText lblPt nm tsCat ]
      -- ESS 閾値の参照線 (100 / 400)
      refLines =
        [ PLine p1 p2 (solid "#888888" 0.8)
        | t <- [100, 400], t <= yMax, let (p1, p2) = valRefLine t ]
      -- value 軸目盛り (Cartesian 左辺 / flip 下辺)
      tsY = mkFontTS Nothing thePal TickF valAnchor 0
      yTicks =
        [ p | tv <- niceTicks 5 0 yMax
            , p <- case coord of
                CoordCartesian ->
                  [ PLine (Point (rX area) (sy tv)) (Point (rX area - 5) (sy tv)) (solid (tpAxis thePal) 1.0)
                  , PText (Point (rX area - 8) (sy tv + 4)) (numToText tv) tsY ]
                CoordFlip ->
                  [ PLine (Point (valPxF tv) (rY area + rH area)) (Point (valPxF tv) (rY area + rH area + 5)) (solid (tpAxis thePal) 1.0)
                  , PText (Point (valPxF tv) (rY area + rH area + 18)) (numToText tv) tsY ] ]
  in axisFrame layout thePal ++ yTicks ++ refLines
       ++ concatMap (uncurry drawOne) (zip [0..] pairs)

-- ===========================================================================
-- Phase 6 A2: Forest plot
-- ===========================================================================

-- | Forest plot (Phase 6 A2): 各 row が「label + 点推定 + CI」 の horizontal CI bar 群。
-- encY = label index (= 0..n-1)、 encX = estimate、 errorX = ± 半幅。 中央 vertical 線。
renderForest :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderForest r layout pal ly =
  let ests = V.toList (vecOr (lyEncX ly) r)
      errs = case getLast (lyErrorX ly) of
        Just c  -> V.toList (vecOr (Last (Just c)) r)
        Nothing -> repeat 0
      n     = length ests
      -- Phase 8 B23-fix: row i を position (n-1-i) に置き、 先頭研究を上端へ (= PS と同方向)。
      -- Layout 側で forest の yCatLabels を反転済みなのでラベルとマーカーが整合する。
      ys    = take n [n - 1, n - 2 ..]  -- label y 位置 (= 上から先頭研究)
      c     = staticColorOr ly (tpDefault pal)
      a     = doubleOr (lyAlpha ly) 0.9
      ptSz  = doubleOr (lySize ly) (mmPt 1.5)
      sx    = scaleApply (lpXScale layout)
      nullX = maybe 0.0 (fromIntegral) (getLast (lyMaxLag ly))  -- 流用
      area  = lpPlotArea layout
      -- Phase 10 A4: glyph は projectPoint、 data-x の参照線は xRefLine で flip 追従。
      coord = flipOnly (lpCoord layout)   -- A7-c: forest は polar 非対象
      pp    = projectPoint coord layout
      -- data x=v の参照線 (Cartesian は縦線 panel 全高、 flip は横線 panel 全幅)。
      xRefLine v = case coord of
        CoordCartesian -> (Point (sx v) (rY area), Point (sx v) (rY area + rH area))
        CoordFlip      -> let yp = scaleApply (lpXScaleFlipped layout) v
                          in (Point (rX area) yp, Point (rX area + rW area) yp)
      -- 中央 null line
      nullLine = let (p1, p2) = xRefLine nullX in [ PLine p1 p2 (solid "#888" 1.0) ]
      -- 各 row: 水平 CI 線 + 点 marker
      rowsP = concat
        [ [ PLine (pp (e - err) yp) (pp (e + err) yp) (solid c 1.5)
          , PCircle (pp e yp) (ptSz / 2)
                   (FillStyle c a) (Just (StrokeStyle c 1.0)) Nothing
          ]
        | (e, err, yp) <- zip3 ests errs (map fromIntegral ys)
        ]
  in nullLine <> rowsP

-- ===========================================================================
-- Phase 6 A3: Funnel plot
-- ===========================================================================

-- | Funnel plot (Phase 6 A3): 効果量 vs 標準誤差の散布図 + 95% envelope。
-- encX = effect、 encY = SE。 envelope は y range の最大 SE まで diagonal で描画。
renderFunnel :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderFunnel r layout pal ly =
  let effects = V.toList (vecOr (lyEncX ly) r)
      ses     = V.toList (vecOr (lyEncY ly) r)
      c       = staticColorOr ly (tpDefault pal)
      a       = doubleOr (lyAlpha ly) 0.7
      ptSz    = doubleOr (lySize ly) (mmPt 1.25)
      sx      = scaleApply (lpXScale layout)
      n       = length effects
      mu      = if n == 0 then 0 else sum effects / fromIntegral n
      seMax   = if null ses then 1 else maximum ses
      area    = lpPlotArea layout
      -- Phase 10 A4: 点・envelope 端点は projectPoint、 mu 参照線は xRefLine で flip 追従。
      coord   = flipOnly (lpCoord layout)   -- A7-c: funnel は polar 非対象
      pp      = projectPoint coord layout
      xRefLine v = case coord of
        CoordCartesian -> (Point (sx v) (rY area), Point (sx v) (rY area + rH area))
        CoordFlip      -> let yp = scaleApply (lpXScaleFlipped layout) v
                          in (Point (rX area) yp, Point (rX area + rW area) yp)
      points  = [ PCircle (pp eff se) (ptSz / 2)
                          (FillStyle c a) (Just (StrokeStyle c 1.0)) Nothing
                | (eff, se) <- zip effects ses ]
      muLine = let (p1, p2) = xRefLine mu in [ PLine p1 p2 (solid "#888" 1.0) ]
      -- diagonal envelope (= ±1.96 SE)、 plotArea 矩形に Liang-Barsky clip
      clipLine (Point x1 y1) (Point x2 y2) =
        let (xMin, xMax) = (rX area, rX area + rW area)
            (yMin, yMax) = (rY area, rY area + rH area)
            dx = x2 - x1
            dy = y2 - y1
            ts = foldl (\acc (p, q) -> if p == 0
                                          then (if q < 0 then Nothing else acc)
                                          else case acc of
                                            Nothing -> Nothing
                                            Just (t0, t1) ->
                                              let t = q / p
                                              in if p < 0
                                                   then if t > t1 then Nothing
                                                        else Just (max t0 t, t1)
                                                   else if t < t0 then Nothing
                                                        else Just (t0, min t1 t))
                       (Just (0, 1))
                       [(-dx, x1 - xMin), (dx, xMax - x1)
                       ,(-dy, y1 - yMin), (dy, yMax - y1)]
        in case ts of
             Just (t0, t1) | t0 < t1 ->
               Just (Point (x1 + t0 * dx) (y1 + t0 * dy),
                     Point (x1 + t1 * dx) (y1 + t1 * dy))
             _ -> Nothing
      envSeg from to = case clipLine from to of
        Just (p1, p2) -> [PLine p1 p2 (solid "#888" 0.8)]
        Nothing       -> []
      envL = envSeg (pp mu 0) (pp (mu - 1.96 * seMax) seMax)
      envR = envSeg (pp mu 0) (pp (mu + 1.96 * seMax) seMax)
  in muLine <> envL <> envR <> points
