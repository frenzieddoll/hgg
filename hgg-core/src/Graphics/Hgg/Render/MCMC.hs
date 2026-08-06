-- |
-- Module      : Graphics.Hgg.Render.MCMC
-- Description : MCMC diagnostic marks (forest, funnel, autocorrelation, ESS)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Render モノリス分割 (出力中立・純粋移動)。
--   [English]: Split off from the Render monolith (output-neutral, pure move).
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
                                      coordXGridIsVertical,
                                      -- Phase 64 A4/A4-b: 参照線と棒を投影層へ通す
                                      projectSegment, CrossLoc (..), BarShape (..),
                                      projectCrossPoint, projectCrossBar)
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

-- | [日本語]: autocorrelation plot: 1 列の時系列から lag-k 自己相関 r(τ)
--   を計算 + bar chart。 ±1.96/√N の significance band も併せて。
--   [English]: Autocorrelation plot: computes the lag-k autocorrelation r(τ)
--   from a single time-series column and draws it as a bar chart, along with
--   the ±1.96/√N significance band.
-- | [日本語]: Autocorrelation plot: encX = 生サンプル列、 lyChain = chain (任意)。
--   chain ごとに ACF ρ(k), k=0..maxLag を計算し、 lag を横軸に chain 別の細い棒で描く
--   (= bayesplot mcmc_acf_bar 流: ACF は plot 内で計算)。 x=lag/y=相関 で軸転置のため
--   Layout scale に頼らず自前マッピング。
--   [English]: Autocorrelation plot: encX is the raw sample column, lyChain
--   is the chain (optional). Computes ACF ρ(k) for k=0..maxLag per chain and
--   draws thin per-chain bars along the lag axis (in the style of
--   bayesplot's mcmc_acf_bar, where the ACF is computed inside the plot).
--   Since x=lag / y=correlation transposes the axes, this uses its own
--   mapping rather than relying on the Layout scale.
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
      -- ★ Phase 64 A4-b: 自前 plotArea マッピングを撤去し投影層へ。 lag は
      --   __離散スロット__ (`CrossAt k`)、 相関は value 軸 (lpYScale) として扱う。
      --   これで coordCartesianX/Y の zoom 指定が効くようになる (A4-b before 実測で
      --   従来は完全に無視されていた)。 chain は slot 内の px offset で横並び。
      coord  = lpCoord layout
      slotW  = catUnitPx coord layout          -- 1 lag ぶんの cross 軸 px
      subW   = slotW / fromIntegral nCh        -- chain 1 本ぶんの sub-slot
      barW   = max 1.5 (subW * 0.7)
      -- chain ci の slot 内 px offset (sub-slot 中心)。 A3 の dodge と同型。
      chainOff ci = negate (slotW / 2) + (fromIntegral ci + 0.5) * subW
      -- 極座標用の data 単位半幅 (1 slot = 1 data 単位)
      halfD  = (0.7 / fromIntegral nCh) / 2
      drawChain ci (_lbl, vs) =
        let col = pal !! (ci `mod` length pal)
            rs  = map (autocorrAt vs) [0 .. maxLag]
            mk k rk = case projectCrossBar coord layout (CrossAt (fromIntegral k))
                                           (chainOff ci) (barW / 2) halfD 0 rk of
              BarRect  rect -> PRect rect (FillStyle col 0.85) (Just (StrokeStyle col 0.5))
              BarWedge segs -> PPath segs (FillStyle col 0.85) (Just (StrokeStyle col 0.5))
        in [ mk k rk | (k, rk) <- zip [0 :: Int ..] rs ]
      bars = concat (zipWith drawChain [0..] groups)
      -- significance band ±1.96/sqrt(N) (= 95% null) + 0 線。 value=t を cross 軸全長に渡す。
      nTot = length xs
      sg = if nTot < 2 then 0 else 1.96 / sqrt (fromIntegral nTot :: Double)
      xLoD = lsDomainLo (lpXScale layout)
      xHiD = lsDomainHi (lpXScale layout)
      valRefPrims t col w =
        let pts = projectSegment coord layout (xLoD, t) (xHiD, t)
        in [ PLine p q (solid col w) | (p, q) <- zip pts (drop 1 pts) ]
      sigBand = valRefPrims 0 (tpAxis thePal) 1.0
             ++ concat [ valRefPrims sg "#888" 0.8 ++ valRefPrims (negate sg) "#888" 0.8
                       | sg > 0 ]
      -- value 軸目盛り (相関 -1..1)。 目盛の向きは coordYAxisPlacement で決める
      -- (= 生の case coord of を持たない)。
      valAtLeft = coordYAxisPlacement coord == AxisLeft
      valAnchor = if valAtLeft then AnchorEnd else AnchorMiddle
      tsY = mkFontTS Nothing thePal TickF valAnchor 0
      tickAnchorPt tv = projectCrossPoint coord layout (CrossAt xLoD) 0 tv
      yTicks = [ p | tv <- [-1.0, -0.5, 0, 0.5, 1.0]
                   , let Point ax ay = tickAnchorPt tv
                   , p <- if valAtLeft
                            then [ PLine (Point ax ay) (Point (ax - 5) ay) (solid (tpAxis thePal) 1.0)
                                 , PText (Point (ax - 8) (ay + 4)) (numToText tv) tsY ]
                            else [ PLine (Point ax ay) (Point ax (ay + 5)) (solid (tpAxis thePal) 1.0)
                                 , PText (Point ax (ay + 18)) (numToText tv) tsY ] ]
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

-- | [日本語]: ESS plot: encX = パラメータ/chain 名、 encY = 計算済み ESS 値。
--   ESS 計算は統計ライブラリの責務、 plot は値を棒にするだけ (= ggplot/bayesplot 流の
--   計算と描画の分離)。 ESS 閾値 (100/400) で色分け (赤=低い/橙=中/緑=高い)。
--   x=名前/y=ESS で軸が転置するため Layout scale に頼らず自前マッピング。
--   [English]: ESS plot: encX is the parameter/chain name, encY is the
--   pre-computed ESS value. ESS computation is the statistics library's
--   responsibility; the plot just turns the values into bars (following the
--   ggplot/bayesplot convention of separating computation from drawing).
--   Colored by the ESS threshold (100/400): red = low, orange = medium,
--   green = high. Since x=name / y=ESS transposes the axes, this uses its
--   own mapping rather than relying on the Layout scale.
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
      -- ★ Phase 64 A4-b: autocorr と同型に投影層へ。 名前 (chain) は __離散スロット__
      --   (`CrossAt i`)、 ESS 値は value 軸 (lpYScale)。 これで coordCartesianX/Y の
      --   zoom 指定が効く (従来の自前マッピングは無視していた)。
      coord = lpCoord layout
      slotW = catUnitPx coord layout          -- 1 名前ぶんの cross 軸 px
      barW  = slotW * 0.6
      halfD = 0.3                              -- 極座標用の data 単位半幅 (0.6 の半分)
      xLoD  = lsDomainLo (lpXScale layout)
      xHiD  = lsDomainHi (lpXScale layout)
      -- 軸の向きは placement helper で決める (= 生の case coord of を持たない)
      valAtLeft = coordYAxisPlacement coord == AxisLeft
      catAtBottom = coordXAxisPlacement coord == AxisBottom
      catAnchor = if catAtBottom then AnchorMiddle else AnchorEnd
      valAnchor = if valAtLeft   then AnchorEnd    else AnchorMiddle
      tsCat = mkFontTS Nothing thePal TickF catAnchor 0
      drawOne i (nm, v) =
        let col | v < 100   = "#d9534f"   -- 低い (要注意)
                | v < 400   = "#f0ad4e"   -- 中
                | otherwise = "#5cb85c"   -- 良い
            barPrim = case projectCrossBar coord layout (CrossAt (fromIntegral i))
                                           0 (barW / 2) halfD 0 v of
              BarRect  rect -> PRect rect (FillStyle col 0.85) (Just (StrokeStyle col 0.5))
              BarWedge segs -> PPath segs (FillStyle col 0.85) (Just (StrokeStyle col 0.5))
            -- 名前ラベルは cross 軸の外側 (Cartesian = panel 下、 Flip = panel 左)
            Point bx by = projectCrossPoint coord layout (CrossAt (fromIntegral i)) 0 0
            lblPt | catAtBottom = Point bx (rY area + rH area + 16)
                  | otherwise   = Point (rX area - 6) (by + 4)
        in [ barPrim, PText lblPt nm tsCat ]
      -- ESS 閾値の参照線 (100 / 400)。 value=t を cross 軸全長に渡す。
      valRefPrims t =
        let pts = projectSegment coord layout (xLoD, t) (xHiD, t)
        in [ PLine p q (solid "#888888" 0.8) | (p, q) <- zip pts (drop 1 pts) ]
      refLines = concat [ valRefPrims t | t <- [100, 400], t <= yMax ]
      -- value 軸目盛り
      tsY = mkFontTS Nothing thePal TickF valAnchor 0
      yTicks =
        [ p | tv <- niceTicks 5 0 yMax
            , let Point ax ay = projectCrossPoint coord layout (CrossAt xLoD) 0 tv
            , p <- if valAtLeft
                     then [ PLine (Point ax ay) (Point (ax - 5) ay) (solid (tpAxis thePal) 1.0)
                          , PText (Point (ax - 8) (ay + 4)) (numToText tv) tsY ]
                     else [ PLine (Point ax ay) (Point ax (ay + 5)) (solid (tpAxis thePal) 1.0)
                          , PText (Point ax (ay + 18)) (numToText tv) tsY ] ]
  in axisFrame layout thePal ++ yTicks ++ refLines
       ++ concatMap (uncurry drawOne) (zip [0..] pairs)

-- ===========================================================================
-- Phase 6 A2: Forest plot
-- ===========================================================================

-- | [日本語]: Forest plot: 各 row が「label + 点推定 + CI」 の horizontal CI bar 群。
--   encY = label index (= 0..n-1)、 encX = estimate、 errorX = ± 半幅。 中央 vertical 線。
--   [English]: Forest plot: each row is a horizontal CI bar showing
--   "label + point estimate + CI". encY is the label index (0..n-1), encX is
--   the estimate, and errorX is the ± half-width. Includes a central
--   vertical line.
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
      -- ★ Phase 64 A4: glyph も参照線も投影層へ。 参照線は「data x=v を y domain 全長に
      --   渡す線分」 として 'projectSegment' に通す。 y scale の range は panel 端に
      --   一致するので直線座標系では旧 px 式と bit 一致し、 polar では radial 線/弧に
      --   なる (flipOnly を撤去できた根拠)。
      coord = lpCoord layout
      pp    = projectPoint coord layout
      yLo   = lsDomainLo (lpYScale layout)
      yHi   = lsDomainHi (lpYScale layout)
      refLinePrims v col =
        let pts = projectSegment coord layout (v, yHi) (v, yLo)
        in [ PLine p q (solid col 1.0) | (p, q) <- zip pts (drop 1 pts) ]
      -- 中央 null line
      nullLine = refLinePrims nullX "#888"
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

-- | [日本語]: Funnel plot: 効果量 vs 標準誤差の散布図 + 95% envelope。
--   encX = effect、 encY = SE。 envelope は y range の最大 SE まで diagonal で描画。
--   [English]: Funnel plot: a scatter of effect size vs. standard error plus
--   a 95% envelope. encX is the effect, encY is the SE. The envelope is
--   drawn diagonally out to the maximum SE in the y range.
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
      -- ★ Phase 64 A4: 点・envelope 端点も mu 参照線も投影層へ (forest と同型)。
      --   参照線は「data x=mu を y domain 全長に渡す線分」 を 'projectSegment' に通す。
      --   直線座標系は旧 px 式と bit 一致、 polar では radial 線/弧になる。
      coord   = lpCoord layout
      pp      = projectPoint coord layout
      yLoF    = lsDomainLo (lpYScale layout)
      yHiF    = lsDomainHi (lpYScale layout)
      points  = [ PCircle (pp eff se) (ptSz / 2)
                          (FillStyle c a) (Just (StrokeStyle c 1.0)) Nothing
                | (eff, se) <- zip effects ses ]
      muLine  = let pts = projectSegment coord layout (mu, yHiF) (mu, yLoF)
                in [ PLine p q (solid "#888" 1.0) | (p, q) <- zip pts (drop 1 pts) ]
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
