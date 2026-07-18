-- |
-- Module      : Hgg.Plot.Render.Statistical
-- Description : 統計 mark (qq/ecdf/rangebar/heatmap/contour/regression/density/statline)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 7 A4: Render モノリス分割 (出力中立・純粋移動)。
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Hgg.Plot.Render.Statistical where

import           Hgg.Plot.Layout (numToText,
                                      Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, resolutionOf,
                                      AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical)
import           Hgg.Plot.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Hgg.Plot.Math.Griddata (gridOf, marchingSegments, innerLevels)  -- Phase 24 A4/A5
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Hgg.Plot.Spec   (Annotation (..), AxisFormat (..),
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
                                      FontSpec (..), orderedCats, histBinning,
                                      HexCell (..), hexbinLayerCells,
                                      colRefName, resolveCol, resolveNum)
import           Data.Maybe          (mapMaybe, isJust)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Hgg.Plot.Spec
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)

import           Hgg.Plot.Primitive
import           Hgg.Plot.Render.Common


-- | Phase 11 A6-2: Q-Q plot (= ggplot geom_qq)。 sample (encY) をソートして
-- order statistic を y、 理論正規分位点 Φ⁻¹((i-0.5)/n) を x に取り点を描く。
-- 理論分位点は 'qqPoints' (RangeOf) を単一情報源として共有 (= x range と一致)。
-- 参照線 (qq line) は ggplot でも別 geom (geom_qq_line) なので本 geom は点のみ。
renderQQ :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderQQ r layout pal ly =
  let sample = V.toList (vecOr (lyEncY ly) r)
      pts    = qqPoints sample  -- [(theoretical x, ordered y)]
      c      = staticColorOr ly (tpDefault pal)
      a      = doubleOr (lyAlpha ly) 0.85
      ptSz   = doubleOr (lySize ly) (mmPt 1.5)
      coord  = lpCoord layout
  in [ PCircle (projectPoint coord layout xt y) (ptSz / 2)
               (FillStyle c a) (Just (StrokeStyle c 1.0)) Nothing
     | (xt, y) <- pts ]

-- | Phase 11 A6-4: ECDF (= ggplot stat_ecdf)。 sample (encX) をソートして右連続の
-- 階段 F(x)=#(≤x)/n を描く。 角点列 'ecdfPoints' を単一情報源とし連続線で結ぶ。
renderEcdf :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderEcdf r layout pal ly =
  let sample = V.toList (vecOr (lyEncX ly) r)
      verts  = ecdfPoints sample
      c      = staticColorOr ly (tpDefault pal)
      w      = doubleOr (lyStroke ly) (mmPt 0.5)
      dash   = maybe [] lineTypeDash (getLast (lyLinetype ly))
      ls     = LineStyle c w dash
      coord  = lpCoord layout
      pp     = projectPoint coord layout
      mkSeg ((x1,y1),(x2,y2)) = PLine (pp x1 y1) (pp x2 y2) ls
  in case verts of
       [] -> []
       _  -> map mkSeg (zip verts (tail verts))

-- | Phase 11 A6-4b: 区間 geom (linerange / pointrange / crossbar)。 各 (x,y) に縦区間
-- y±errorY を描く。 withPoint=中心点を足す (pointrange)、 asBox=幅付き箱+中央線 (crossbar)。
-- 箱の半幅は px 固定 (= error bar cap と同じ px 空間、 連続 x でも安定)。
renderRangeBar :: Resolver -> Layout -> ThemePalette -> Layer -> Bool -> Bool -> [Primitive]
renderRangeBar r layout pal ly withPoint asBox =
  let xs = V.toList (vecOr (lyEncX ly) r)
      ys = V.toList (vecOr (lyEncY ly) r)
      es = V.toList (vecOr (lyErrorY ly) r)
      n  = minimum [length xs, length ys, length es]
      c  = staticColorOr ly (tpDefault pal)
      w  = doubleOr (lyStroke ly) (mmPt 0.5)
      ptSz = doubleOr (lySize ly) (mmPt 1.5)
      ls = solid c w
      coord = lpCoord layout
      pp = projectPoint coord layout
      -- ★ Phase 41: crossbar 箱の半幅を ggplot 同様データ単位化 (width = markWidth × resolution)。
      --   boxplot bw と同型 (catUnitPx × 幅係数)・resolution で連続 x にも追従。 旧 px 固定 10 を置換。
      capWFactor = doubleOr (lyMarkWidth ly) 0.9   -- ggplot geom_crossbar 既定 width = 0.9
      finite v = not (isNaN v) && not (isInfinite v)
      resX = resolutionOf (filter finite xs)
      halfW = 0.5 * capWFactor * resX * catUnitPx coord layout
      mkOne i =
        let x = xs !! i; y = ys !! i; e = es !! i
            Point pcx pcyLo = pp x (y - e)
            Point _   pcyHi = pp x (y + e)
            Point pmx pmy   = pp x y
        in if asBox
             then -- crossbar: 箱 (px 幅) + 中央水平線
               [ PRect (Rect (pmx - halfW) (min pcyLo pcyHi) (2 * halfW) (abs (pcyHi - pcyLo)))
                       (FillStyle c 0.15) (Just (StrokeStyle c w))
               , PLine (Point (pmx - halfW) pmy) (Point (pmx + halfW) pmy) ls ]
             else -- linerange: 縦線。 pointrange は中心点を追加
               PLine (Point pcx pcyLo) (Point pcx pcyHi) ls
               : (if withPoint
                    then [ PCircle (Point pmx pmy) (ptSz / 2)
                                   (FillStyle c 1.0) (Just (StrokeStyle c 1.0)) Nothing ]
                    else [])
  in if n <= 0 then [] else concatMap mkOne [0 .. n - 1]

-- | Phase 11 A6-3: heatmap (= ggplot geom_tile)。 x/y はカテゴリ列、 value (= lyColor の
-- ColorByContinuous) を各 (x,y) セルの連続色 (Viridis) に写して矩形で塗る。 セルは data 空間で
-- カテゴリ中心 ±0.5 の 1 単位四方 (= projectRectData で flip も自動追従)。 cell 間は背景色の
-- 細い枠で区切る (= grid 状)。 同 (x,y) が重複する行は後勝ち (= 描画順で上書き)。
renderHeatmap :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderHeatmap r layout pal ly =
  let toLabels mcr = case getLast mcr of
        Just cr -> case resolveCol r cr of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> V.toList (V.map numToText v)
          Nothing          -> []
        Nothing -> []
      xs = toLabels (lyEncX ly)
      ys = toLabels (lyEncY ly)
      n  = min (length xs) (length ys)
      cs = colorVector r layout pal ly n  -- ColorByContinuous → Viridis 連続色
      xLabels = lpXCategoryLabels layout
      yLabels = lpYCategoryLabels layout
      coord = lpCoord layout
      a  = doubleOr (lyAlpha ly) 1.0
      mkCell i = do
        xi <- elemIndex (xs !! i) xLabels
        yi <- elemIndex (ys !! i) yLabels
        let c  = cs V.! i
            xd = fromIntegral xi; yd = fromIntegral yi
            rc = projectRectData coord layout (xd - 0.5) (xd + 0.5) (yd - 0.5) (yd + 0.5)
        Just (PRect rc (FillStyle c a) (Just (StrokeStyle (tpBackground pal) 1.0)))
  in if n <= 0 then [] else mapMaybe mkCell [0 .. n - 1]

-- | Phase 28 (Ch10 EDA): geom_count (= ggplot @geom_count()@ / @stat_sum@)。
-- x/y はともにカテゴリ列。 各 (x,y) セルの観測件数を集計し、 cell 中心に
-- **面積 ∝ 件数** (= 半径 ∝ √件数) の点を打つ。 最大件数のセルが半径 maxR (px)、
-- 件数 0 のセルは描かない。 maxR は lySize で上書き可 (既定 18 → 半径 9)。
-- heatmap と同じカテゴリ軸 (lpX/YCategoryLabels) を用いるので両軸自動でカテゴリ化。
renderCount :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderCount r layout pal ly =
  let toLabels mcr = case getLast mcr of
        Just cr -> case resolveCol r cr of
          Just (TxtData v) -> V.toList v
          Just (NumData v) -> V.toList (V.map numToText v)
          Nothing          -> []
        Nothing -> []
      xs = toLabels (lyEncX ly)
      ys = toLabels (lyEncY ly)
      n  = min (length xs) (length ys)
      xLabels = lpXCategoryLabels layout
      yLabels = lpYCategoryLabels layout
      coord = lpCoord layout
      c   = staticColorOr ly (tpDefault pal)
      a   = doubleOr (lyAlpha ly) 0.85
      maxR = doubleOr (lySize ly) (mmPt 4.5) / 2   -- 最大件数セルの半径 (既定 4.5mm 径)
      -- (xi, yi) セル → 件数 (カテゴリに無い水準は除外)
      counts = Map.toList $ Map.fromListWith (+)
        [ ((xi, yi), 1 :: Int)
        | i <- [0 .. n - 1]
        , Just xi <- [elemIndex (xs !! i) xLabels]
        , Just yi <- [elemIndex (ys !! i) yLabels] ]
      maxC = maximum (1 : map snd counts)
      mkPt ((xi, yi), cnt) =
        let xd = fromIntegral xi; yd = fromIntegral yi
            rad = maxR * sqrt (fromIntegral cnt / fromIntegral maxC)  -- 面積 ∝ 件数
        in PCircle (projectPoint coord layout xd yd) rad
                   (FillStyle c a) (Just (StrokeStyle c 1.0)) Nothing
  in if n <= 0 then [] else map mkPt counts

-- | contour (= 等高線図、 marching squares)。 連続 x/y/z を正則格子に再標本化
-- (inverse-distance weighting で散布点 → ノード) し、 z 範囲を等分した nLev 段の
-- **等値線**を marching squares で描く。 各等値線は z 値で連続色 (Viridis)。
-- 旧実装は binned heatmap だったが、 「contour = 等高線」 の名に合わせ isolines に
-- (binned heatmap が要るなら 'bin2d')。 HS=PS 同式 (PS renderContour も同型)。
--
-- TODO (Phase 14 繰越、 2026-06-04): 等値線が**ガタつく**。 原因 = ① IDW は各データ点で
-- 尖る (cusp) ため格子データでも滑らかにならない、 ② 32×32 再標本化が粗い、 ③ marching
-- squares の線形補間で階段状になりやすい。 改善案 = (a) IDW を**双線形補間** (元が格子なら
-- 格子直引き) に置換、 (b) 再標本化後に軽い Gaussian smoothing、 (c) 解像度↑。
-- HS/PS 両方に同じ修正が要る (parity 維持)。
renderContour :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderContour r layout _pal ly =
  case contourInput r ly of
    Nothing -> []
    Just (xNodes, yNodes, gridV, zmin, zmax) ->
      let levels = contourLevelsFor ly zmin zmax
          coord  = lpCoord layout
          pp     = projectPoint coord layout
          gridL  = map V.toList (V.toList gridV)   -- 共有核 (Griddata) は [[Double]]
          levelColor lv = continuousColor (lpContinuousPalette layout)
                            (if zmax == zmin then 0.5 else (lv - zmin)/(zmax - zmin))
          drawLevel lv =
            [ PLine (pp ax ay) (pp bx by) (solid (levelColor lv) 1.5)
            | ((ax,ay),(bx,by)) <- marchingSegments xNodes yNodes gridL lv ]
      in if zmax <= zmin then [] else concatMap drawLevel levels

-- | Phase 24 A4: contour / filled contour の共通入力 — (x,y,z) triple を
-- 'gridOf' で格子化する。 ★規則 grid 入力 (計画格子・linspace 由来) は
-- **補間せず直入力** (旧実装は常に全点 IDW で 32x32 再標本化しており、
-- 規則 grid でも等値線が歪む + 隅に偽輪郭が出るバグだった)。
-- 散布入力のみ k 近傍 IDW で 32x32 へ。 格子の向きは grid!!j!!i (行 = y)。
contourInput :: Resolver -> Layer
             -> Maybe ([Double], [Double], V.Vector (V.Vector Double), Double, Double)
contourInput r ly =
  let xs = V.toList (vecOr (lyEncX ly) r)
      ys = V.toList (vecOr (lyEncY ly) r)
      vs = case getLast (lyColor ly) of
        Just (ColorByContinuous cr) -> maybe [] V.toList (resolveNum r cr)
        _                           -> []
      n  = minimum [length xs, length ys, length vs]
  in if n < 4 then Nothing
     else
       let triples = [ (xs !! i, ys !! i, vs !! i) | i <- [0 .. n - 1] ]
           (xNodes, yNodes, grid) = gridOf 32 triples
           gridV = V.fromList (map V.fromList grid)
           allZ  = concat grid
       in Just (xNodes, yNodes, gridV, minimum allZ, maximum allZ)

-- | Phase 24 A4: 等高線レベル。 明示 breaks ('contourBreaks') > 本数指定
-- ('contourLevels'、 既定 8)。 既定は (zmin, zmax) の**内側等間隔**
-- (lv_k = zmin + (zmax-zmin)·k/(n+1)) — 端値ちょうどの退化等値線を避ける
-- (旧実装の 15%-95% クランプは廃止 = 端近くのレベルも出る)。
contourLevelsFor :: Layer -> Double -> Double -> [Double]
contourLevelsFor ly zmin zmax =
  case getLast (lyContourBreaks ly) of
    Just bs -> bs
    Nothing ->
      innerLevels (max 1 (fromMaybe 8 (getLast (lyContourLevels ly)))) zmin zmax

-- | Phase 24 A4: filled contour (等値帯の塗り)。 各セルを「最下帯の色で全塗り →
-- level 昇順に z >= lv の部分多角形を上塗り」 の累積方式で塗る (セル内は
-- marching squares と同じ線形補間の境界 = 'contour' の線と整合)。
-- saddle セル (対角ケース) は頂点巡回順の単一多角形で近似 (v1 既知の限界)。
renderContourFilled :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderContourFilled r layout _pal ly =
  case contourInput r ly of
    Nothing -> []
    Just (xNodes, yNodes, gridV, zmin, zmax)
      | zmax <= zmin -> []
      | otherwise ->
      let levels = contourLevelsFor ly zmin zmax
          coord  = lpCoord layout
          pp     = projectPoint coord layout
          nx     = length xNodes
          ny     = length yNodes
          xAt i  = xNodes !! i
          yAt j  = yNodes !! j
          zAt i j = (gridV V.! j) V.! i
          norm v = (v - zmin) / (zmax - zmin)
          -- 帯色: level 列で区切った各帯の中央値の連続色
          fences   = zmin : levels ++ [zmax]
          bandCol k = continuousColor (lpContinuousPalette layout)
                        (norm ((fences !! k + fences !! (k+1)) / 2))
          -- セルの z >= lv 部分多角形: 角を巡回し、 含まれる角 + 辺の交点を拾う
          polyAbove lv i j =
            let cs = [ ((xAt i,     yAt j),     zAt i j)
                     , ((xAt (i+1), yAt j),     zAt (i+1) j)
                     , ((xAt (i+1), yAt (j+1)), zAt (i+1) (j+1))
                     , ((xAt i,     yAt (j+1)), zAt i (j+1)) ]
                seg ((p1, z1), (p2, z2))
                  | z1 >= lv && z2 >= lv = [p2]
                  | z1 >= lv             = [cross p1 z1 p2 z2]
                  | z2 >= lv             = [cross p1 z1 p2 z2, p2]
                  | otherwise            = []
                cross (ax, ay) za (bx, by) zb =
                  let t = if zb == za then 0.5 else (lv - za) / (zb - za)
                  in (ax + t*(bx-ax), ay + t*(by-ay))
            in concatMap seg (zip cs (drop 1 cs ++ [head cs]))
          -- 塗りと同色の細 stroke = 隣接セル間の anti-alias 白筋 (seam) 埋め
          fillPoly col pts = case map (uncurry pp) pts of
            (p0 : rest@(_ : _ : _)) ->
              [ PPath (MoveTo p0 : map LineTo rest ++ [ClosePath])
                      (FillStyle col 1.0)
                      (Just (StrokeStyle col 0.6)) ]
            _ -> []
          cellPrims i j =
            let base = fillPoly (bandCol 0)
                         [ (xAt i, yAt j), (xAt (i+1), yAt j)
                         , (xAt (i+1), yAt (j+1)), (xAt i, yAt (j+1)) ]
                ups  = concat
                  [ fillPoly (bandCol k) (polyAbove lv i j)
                  | (k, lv) <- zip [1 ..] levels ]
            in base ++ ups
      in concat [ cellPrims i j | i <- [0 .. nx - 2], j <- [0 .. ny - 2] ]

-- | binned heatmap (= ggplot geom_bin2d)。 連続 x/y/z を nBins×nBins の grid に
-- binning し、 各セルの z 平均を連続色 (Viridis) で塗る。 'renderContour' (等高線) の
-- 塗り版。 セルは生 data 範囲 [xLo,xHi]×[yLo,yHi] を等分し projectRectData で投影
-- (flip 自動追従)。 空セルは描かない。 PS と同一式。
renderBin2d :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBin2d r layout _pal ly =
  let xs = V.toList (vecOr (lyEncX ly) r)
      ys = V.toList (vecOr (lyEncY ly) r)
      -- z (ColorByContinuous) があればセル平均 (= stat_summary_2d)、 無ければ
      -- セル**件数** (= ggplot geom_bin2d 既定の fill=count)。
      hasZ = case getLast (lyColor ly) of Just (ColorByContinuous _) -> True; _ -> False
      vs = case getLast (lyColor ly) of
        Just (ColorByContinuous cr) -> maybe [] V.toList (resolveNum r cr)
        _                           -> replicate (min (length xs) (length ys)) 0
      n  = minimum [length xs, length ys, length vs]
  in if n < 3 then []
     else
       let triples = [ (xs !! i, ys !! i, vs !! i) | i <- [0 .. n - 1] ]
           xLo = minimum [x | (x,_,_) <- triples]; xHi = maximum [x | (x,_,_) <- triples]
           yLo = minimum [y | (_,y,_) <- triples]; yHi = maximum [y | (_,y,_) <- triples]
           xSpan = xHi - xLo; ySpan = yHi - yLo
           nBins = 12 :: Int
           binOf lo sp p = max 0 (min (nBins - 1)
                             (floor ((p - lo) / sp * fromIntegral nBins)))
           assigned = [ (binOf xLo xSpan x, binOf yLo ySpan y, v) | (x,y,v) <- triples ]
           cellMean i j = let ms = [ v | (bx,by,v) <- assigned, bx == i, by == j ]
                          in if null ms then Nothing
                             else Just (if hasZ then sum ms / fromIntegral (length ms)
                                                else fromIntegral (length ms))  -- count
           cells = [ (i, j, cellMean i j) | i <- [0 .. nBins - 1], j <- [0 .. nBins - 1] ]
           means = [ m | (_,_,Just m) <- cells ]
           vMin = minimum means; vMax = maximum means
           coord = lpCoord layout
           drawCell (i, j, mm) = case mm of
             Nothing -> []
             Just m  ->
               let xd0 = xLo + fromIntegral i       * xSpan / fromIntegral nBins
                   xd1 = xLo + fromIntegral (i + 1) * xSpan / fromIntegral nBins
                   yd0 = yLo + fromIntegral j       * ySpan / fromIntegral nBins
                   yd1 = yLo + fromIntegral (j + 1) * ySpan / fromIntegral nBins
                   t   = if vMax == vMin then 0.5 else (m - vMin) / (vMax - vMin)
                   col = continuousColor (lpContinuousPalette layout) (max 0 (min 1 t))
                   rc  = projectRectData coord layout xd0 xd1 yd0 yd1
               in [ PRect rc (FillStyle col 1.0) (Just (StrokeStyle "#ffffff" 0.3)) ]
       in if null means then [] else concatMap drawCell cells

-- | geom_tile / geom_raster 相当 (Phase 60)。 __1 行 = 1 セル__。 連続 x/y をセル中心とし、
-- fill (colorBy = 'ColorByCol' 離散 / 'ColorByContinuous' 連続) の色で矩形をベタ塗りする。
-- bin2d と違い**再ビニングしない** (事前計算済みグリッドをそのまま塗る)。 セル幅/高さは
-- sorted unique x/y の隣接差分の最小 = 格子間隔から自動 (ggplot @resolution()@ 相当・隙間なし)。
-- 決定境界の res×res グリッド塗り (縞解消) が主用途。 色/凡例は 'colorVector' + color-enc 駆動
-- guide が自動処理 (categorical なら離散パレット + 離散凡例)。 枠線なし = seamless。
renderTile :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderTile r layout pal ly =
  let xs = V.toList (vecOr (lyEncX ly) r)
      ys = V.toList (vecOr (lyEncY ly) r)
      n  = min (length xs) (length ys)
      cs = colorVector r layout pal ly n   -- ColorByCol=離散 / ColorByContinuous=連続 両対応
      coord = lpCoord layout
      a  = doubleOr (lyAlpha ly) 1.0
      dx = gridStep xs
      dy = gridStep ys
      mkCell i =
        let x = xs !! i; y = ys !! i
            c = cs V.! i
            rc = projectRectData coord layout (x - dx/2) (x + dx/2) (y - dy/2) (y + dy/2)
        in PRect rc (FillStyle c a) Nothing   -- 隙間なし = 枠線なし (seamless)
  in if n <= 0 then [] else map mkCell [0 .. n - 1]

-- | sorted unique 値の隣接差分の最小を格子間隔とする (ggplot @resolution()@)。
-- 単一値 / 差分無しは 1.0 fallback。
gridStep :: [Double] -> Double
gridStep vs =
  let us    = map head (groupBy (==) (sort vs))   -- sorted unique
      diffs = [ b - x | (x, b) <- zip us (drop 1 us), b > x ]
  in if null diffs then 1.0 else minimum diffs

-- | Phase 40: hexbin (= ggplot @geom_hex@ / matplotlib @hexbin@)。 連続 x/y を六角格子に
--   binning し、 各セルの**件数**を連続色 (Viridis) の pointy-top 六角形で塗る。 セル分割数は
--   'lyBinCount' (既定 30)。 binning は純関数 'hexbinCells' (d3-hexbin)、 描画はその 6 頂点を
--   'projectPoint' で screen へ投影して 'PPath' で塗る。 colorbar は count guide (別途) が出す。
renderHexbin :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderHexbin r layout _pal ly =
  case hexbinLayerCells r ly of
    [] -> []
    cells ->
          let counts = map hexCount cells
              vMin = fromIntegral (minimum counts) :: Double
              vMax = fromIntegral (maximum counts) :: Double
              coord = lpCoord layout
              pp    = projectPoint coord layout
              drawCell c =
                let t   = if vMax == vMin then 1.0
                          else (fromIntegral (hexCount c) - vMin) / (vMax - vMin)
                    col = continuousColor (lpContinuousPalette layout) (max 0 (min 1 t))
                in case map (uncurry pp) (hexVerts c) of
                     (p0 : rest@(_ : _ : _)) ->
                       [ PPath (MoveTo p0 : map LineTo rest ++ [ClosePath])
                               (FillStyle col 1.0) (Just (StrokeStyle col 0.5)) ]
                     _ -> []
          in concatMap drawCell cells

renderStatLine :: Resolver -> Layout -> ThemePalette -> Layer
               -> ([Double] -> Double) -> [Primitive]
renderStatLine r layout pal ly statF =
  let vs = V.toList (vecOr (lyEncY ly) r)
  in if null vs then [] else
    let v  = statF vs
        sy = scaleApply (lpYScale layout)
        a  = lpPlotArea layout
        c  = staticColorOr ly (tpAxis pal)
        w  = doubleOr (lyStroke ly) (mmPt 0.5)
    in [ PLine (Point (rX a) (sy v)) (Point (rX a + rW a) (sy v))
               (solid c w) ]

-- | Density plot (= Gaussian KDE 簡易版)。 lyEncX = 値ベクター。
-- bandwidth は Silverman の経験則、 100 grid 点で評価して PPath で曲線描画。
--
-- color/fill aesthetic (= 'ColorByCol') があるときは群ごとに分割し、 各群を
-- 独立に正規化した KDE 曲線を群色で重ねて描く (= ggplot @geom_density(aes(color=g))@)。
-- 各群の peak が異なるので y domain も群対応 (RangeOf.densityYRange と整合)。
renderDensity :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderDensity r layout pal ly =
  let xsFull = V.toList (vecOrFull (lyEncX ly) r)   -- 長さ保持 (群キーと整列するため)
      xs     = filter (not . isNaN) xsFull          -- 単群 KDE 用 (NaN = Maybe の Nothing を除去)
      w      = doubleOr (lyStroke ly) (mmPt 0.5)
      isNorm = getLast (lyDensityNorm ly) == Just True
      -- Phase 28: density 曲線下の塗り (= ggplot geom_density(aes(fill=)))。 alpha と併用。
      doFill = getLast (lyDensityFill ly) == Just True
      fillA  = doubleOr (lyAlpha ly) 0.5
      coord  = lpCoord layout
      nGrid  = 100 :: Int
      -- x grid は data 範囲だけでなく x axis 全体 (= padded range) を評価
      -- (= padded 領域でも density が tail 値で描画される。 matplotlib seaborn 流)
      xDomLo = lsDomainLo (lpXScale layout)
      xDomHi = lsDomainHi (lpXScale layout)
      step   = (xDomHi - xDomLo) / fromIntegral (nGrid - 1)
      gridX  = [ xDomLo + fromIntegral i * step | i <- [0 .. nGrid - 1] ]
      -- 1 群 (= 1 系列) の Silverman KDE を gridX で評価。
      kdeYs gxs =
        let n  = length gxs
            mu = sum gxs / fromIntegral n
            sd = sqrt (sum [(x - mu)^(2 :: Int) | x <- gxs] / fromIntegral (n - 1))
            bw = max (1.06 * sd * fromIntegral n ** (-0.2 :: Double)) 1e-9
            kde x = sum [ exp (negate ((x - xi) ** 2) / (2 * bw ** 2))
                        | xi <- gxs ] / (fromIntegral n * bw * sqrt (2 * pi))
        in map kde gridX
      -- projectPoint 経由の曲線 PPath (通常 density)。
      curveOf col gxs
        | length gxs < 2 = []
        | otherwise =
            let pts  = zipWith (\x y -> projectPoint coord layout x y) gridX (kdeYs gxs)
                segs = case pts of { [] -> []; (p:rest) -> MoveTo p : map LineTo rest }
                -- Phase 28: doFill なら曲線下を col×fillA で塗る (= 曲線 → 右端 base →
                -- 左端 base へ閉じた polygon)。 既定は ggplot 同様 fill=NA = 線のみ。
                fillPrim
                  | not doFill = []
                  | otherwise =
                      let baseR  = projectPoint coord layout (last gridX) 0
                          baseL  = projectPoint coord layout (head gridX) 0
                          fsegs  = segs ++ [LineTo baseR, LineTo baseL]
                      in [ PPath fsegs (FillStyle col fillA) Nothing ]
            in fillPrim ++ [ PPath segs (FillStyle "" 0) (Just (StrokeStyle col w)) ]
      -- 群別 density 判定: lyColor = ColorByCol で群キーが xs と整列。
      -- isNorm (= pairs 対角) は値軸ゆえ群分割の対象外 (従来通り 1 本)。
      grouped = case getLast (lyColor ly) of
        Just (ColorByCol gcr) | not isNorm ->
          case groupKeysOf r gcr of
            -- ★ 群キーは値列と同じ全長で整列。 NaN 値の行を両方から落として対応を保つ
            --   (vecOr で先に縮めると length 不一致で群分割が無効化していた)。
            Just ks | length ks == length xsFull ->
              Just (unzip [ (k, x) | (k, x) <- zip ks xsFull, not (isNaN x) ])
            _ -> Nothing
        _ -> Nothing
  in case grouped of
       -- === 群別: colorVector と同じ規則で群色を割当て、 群ごとに曲線を重ねる ===
       Just (ks, gxsVals) ->
         let distinct = let cats = lyColorCats ly in if null cats then orderedCats ks else cats
             palArr   = lpCategoricalPalette layout
             manual   = lpColorManual layout
             colorOf t = case lookup t manual of
               Just cc -> cc
               Nothing -> case elemIndex t distinct of
                 Just i  -> palArr !! (i `mod` length palArr)
                 Nothing -> tpDefault pal
             grps    = orderedGroups ks gxsVals            -- [(key,[x])] 初出順
             -- distinct (= 凡例) 順で描画し色を一致させる。
             ordered = [ (g, vs) | g <- distinct, Just vs <- [lookup g grps] ]
         in concat [ curveOf (colorOf g) gxs | (g, gxs) <- ordered ]
       -- === 非 group / pairs 対角: 従来どおり全データ 1 本 ===
       Nothing ->
         let c = staticColorOr ly (tpDefault pal)
         in if length xs < 2 then [] else
            let ys     = kdeYs xs
                sx     = scaleApply (lpXScale layout)
                -- Phase 8 B16: densityNorm (pairs 対角) は y 軸 = 値範囲なので panel
                -- 高さに独立正規化して描く (= seaborn pairplot 対角の挙動)。
                area   = lpPlotArea layout
                yPeak  = maximum (1e-12 : ys)
                syNorm v = rY area + rH area - (v / yPeak) * rH area * 0.95
                pts = if isNorm
                        then zipWith (\x y -> Point (sx x) (syNorm y)) gridX ys
                        else zipWith (\x y -> projectPoint coord layout x y) gridX ys
                segs = case pts of { [] -> []; (p:rest) -> MoveTo p : map LineTo rest }
                -- Phase 28: doFill なら曲線下を塗る (isNorm 時は基線が panel 下端)。
                fillPrim
                  | not doFill = []
                  | otherwise =
                      let (baseR, baseL)
                            | isNorm    = ( Point (sx (last gridX)) (rY area + rH area)
                                          , Point (sx (head gridX)) (rY area + rH area) )
                            | otherwise = ( projectPoint coord layout (last gridX) 0
                                          , projectPoint coord layout (head gridX) 0 )
                      in [ PPath (segs ++ [LineTo baseR, LineTo baseL]) (FillStyle c fillA) Nothing ]
            in fillPrim ++ [ PPath segs (FillStyle "" 0) (Just (StrokeStyle c w)) ]

-- | 頻度多角形 (Ch10 EDA, Phase 28): @geom_freqpoly@。 histogram と同じ bin 化
-- ('histBinning') で各 bin の count を求め、 bin 中心 @origin+(i+0.5)*binW@ と count を
-- 折れ線で結ぶ (KDE の 'renderDensity' とは別物 = ビン頻度の生の折れ線)。 空 bin は
-- count 0 として線が底に落ちる (ggplot geom_freqpoly と同じ)。 'lyHistDensity' True で
-- after_stat(density) = count/(群N*binW) に正規化 (面積 1)。 color 群分割
-- (lyColor = ColorByCol) は 'renderDensity' と同方式で群ごとに別色の折れ線を重ねる。
renderFreqPoly :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderFreqPoly r layout pal ly =
  let xs        = V.toList (vecOr (lyEncX ly) r)
      w         = doubleOr (lyStroke ly) (mmPt 0.5)
      isDensity = getLast (lyHistDensity ly) == Just True
      coord     = lpCoord layout
  in if null xs then [] else
    let (origin, binW, nB) = histBinning ly (minimum xs, maximum xs)
        binIx x  = min (nB - 1) (max 0 (floor ((x - origin) / binW)))
        center i = origin + (fromIntegral i + 0.5) * binW
        -- 1 群 (= 1 系列) の bin count → bin 中心折れ線。
        polyOf col gxs
          | null gxs = []
          | otherwise =
              let cs = foldl (\acc x -> let i = binIx x
                                         in take i acc <> [acc !! i + 1] <> drop (i+1) acc)
                              (replicate nB (0 :: Int)) gxs
                  gN  = fromIntegral (length gxs) :: Double
                  toY c = if isDensity && gN > 0 && binW > 0
                            then fromIntegral c / (gN * binW)
                            else fromIntegral c
                  pts  = [ projectPoint coord layout (center i) (toY c)
                         | (i, c) <- zip [0 ..] cs ]
                  segs = case pts of { [] -> []; (p:rest) -> MoveTo p : map LineTo rest }
              in [ PPath segs (FillStyle "" 0) (Just (StrokeStyle col w)) ]
        -- 群分割判定 (= renderDensity と同規則)。
        grouped = case getLast (lyColor ly) of
          Just (ColorByCol gcr) ->
            case groupKeysOf r gcr of
              Just ks | length ks == length xs -> Just ks
              _                                -> Nothing
          _ -> Nothing
    in case grouped of
         Just ks ->
           let distinct = let cats = lyColorCats ly in if null cats then orderedCats ks else cats
               palArr   = lpCategoricalPalette layout
               manual   = lpColorManual layout
               colorOf t = case lookup t manual of
                 Just cc -> cc
                 Nothing -> case elemIndex t distinct of
                   Just i  -> palArr !! (i `mod` length palArr)
                   Nothing -> tpDefault pal
               grps    = orderedGroups ks xs
               ordered = [ (g, vs) | g <- distinct, Just vs <- [lookup g grps] ]
           in concat [ polyOf (colorOf g) gxs | (g, gxs) <- ordered ]
         Nothing ->
           let c = staticColorOr ly (tpDefault pal)
           in polyOf c xs
