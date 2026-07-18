-- |
-- Module      : Hgg.Plot.Math.Griddata
-- Description : 散布 (x,y,z) → 格子化 (Phase 24 A4・contour/surface 共用基盤)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- contour / filled contour / 3D surface が共有する「散布データの格子化」 核。
--
--   * 'detectGrid' — 入力が**規則 grid** (x の固有値 × y の固有値が全組存在)
--     なら補間せず**そのまま**格子に並べ替える (Phase 24 A4 バグ修正の本丸:
--     旧実装は規則 grid 入力でも全点 IDW 再標本化して歪んでいた)
--   * 'resampleKNN' — 真の散布入力のみ **k 近傍 IDW** (逆距離加重・power 2)
--     で格子に補間する (旧実装の全点 IDW は遠方点まで重み付けされ
--     全体平均へ潰れる + 隅に偽値が出る)
--   * 'gridOf' — 上記 2 つの自動切替 (検出成功 = 直入力、 失敗 = k 近傍補間)
--   * 'marchingSegments' / 'innerLevels' — 等高線 (isoline) 抽出核。 marching
--     squares で 1 level 分の線分群を data 座標で返す。 2D 'renderContour' と
--     3D 床面投影 contour (Phase 24 A5) が**同一核を共有**する (parity 保全)。
--
-- 格子の向き規約: @zGrid !! j !! i = z(xNodes !! i, yNodes !! j)@ (行 = y)。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Math.Griddata
  ( detectGrid
  , resampleKNN
  , gridOf
  , marchingSegments
  , innerLevels
  ) where

import           Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- | 規則 grid の検出: x / y の固有値数の積が点数と一致し、 かつ全セルが
-- 埋まっていれば @Just (xNodes, yNodes, zGrid)@。 固有値は完全一致 (==) で
-- 集計する (計画格子・linspace 由来の座標は bit 一致する前提。 ノイズ入り
-- 座標は検出に落ちて 'resampleKNN' へ)。 重複座標 (反復測定) は後勝ち。
detectGrid :: [(Double, Double, Double)] -> Maybe ([Double], [Double], [[Double]])
detectGrid pts =
  let xs = uniqSorted [x | (x, _, _) <- pts]
      ys = uniqSorted [y | (_, y, _) <- pts]
      m  = Map.fromList [ ((x, y), z) | (x, y, z) <- pts ]
      nCell = length xs * length ys
  in if nCell == Map.size m && nCell > 0
       then
         let rows = [ [ Map.lookup (x, y) m | x <- xs ] | y <- ys ]
         in if all (all (/= Nothing)) rows
              then Just (xs, ys, map (map unwrap) rows)
              else Nothing
       else Nothing
  where
    unwrap (Just v) = v
    unwrap Nothing  = 0   -- 到達しない (上で全 Just を確認済)

uniqSorted :: [Double] -> [Double]
uniqSorted = dedup . sort
  where
    dedup (a : b : rest) | a == b    = dedup (b : rest)
                         | otherwise = a : dedup (b : rest)
    dedup xs = xs

-- | k 近傍 IDW (逆距離加重・power 2) で nx×ny 格子に補間する。
-- 旧実装 (全点 IDW) との違い = 各ノードで**最も近い k 点だけ**を重み付け
-- するため、 遠方の点に引っ張られて全体平均へ潰れない。
resampleKNN :: Int  -- ^ 近傍数 k (目安 8)
            -> Int  -- ^ x 方向ノード数
            -> Int  -- ^ y 方向ノード数
            -> [(Double, Double, Double)]
            -> ([Double], [Double], [[Double]])
resampleKNN k nx ny pts =
  let xLo = minimum [x | (x, _, _) <- pts]; xHi = maximum [x | (x, _, _) <- pts]
      yLo = minimum [y | (_, y, _) <- pts]; yHi = maximum [y | (_, y, _) <- pts]
      at lo hi n i | n <= 1    = lo
                   | otherwise = lo + (hi - lo) * fromIntegral i / fromIntegral (n - 1)
      xNodes = [ at xLo xHi nx i | i <- [0 .. nx - 1] ]
      yNodes = [ at yLo yHi ny j | j <- [0 .. ny - 1] ]
      kEff = max 1 (min k (length pts))
      idw px py =
        let near = take kEff (sortOn fst [ ((px-x)^(2::Int) + (py-y)^(2::Int), z)
                                         | (x, y, z) <- pts ])
            ws = [ (1 / (d + 1e-9), z) | (d, z) <- near ]
            sw = sum (map fst ws)
        in sum [ w * z | (w, z) <- ws ] / sw
      grid = [ [ idw px py | px <- xNodes ] | py <- yNodes ]
  in (xNodes, yNodes, grid)

-- | 自動切替: 規則 grid なら直入力 (補間なし)、 散布なら k=8 近傍 IDW で
-- n×n 格子化。 contour / filled contour / 床面投影が共有する入口。
gridOf :: Int  -- ^ 散布時の再標本ノード数 (各軸)
       -> [(Double, Double, Double)]
       -> ([Double], [Double], [[Double]])
gridOf n pts = case detectGrid pts of
  Just g  -> g
  Nothing -> resampleKNN 8 n n pts

-- | marching squares: 1 つの等値 @level@ に対する等高線の線分群 (data 座標)。
-- grid の向きは @grid !! j !! i = z(xNodes !! i, yNodes !! j)@ (行 = y)。
-- セル走査順は @i (外)・j (内)@、 セル内の case 分岐は 2D 'renderContour' の
-- 旧インライン実装と完全一致 (= SVG ビット不変)。 2D contour と 3D 床面投影
-- contour が共有する核 (Phase 24 A5)。
marchingSegments
  :: [Double]    -- ^ xNodes (x 方向ノード)
  -> [Double]    -- ^ yNodes (y 方向ノード)
  -> [[Double]]  -- ^ grid (行 = y、 @grid!!j!!i@)
  -> Double      -- ^ level
  -> [((Double, Double), (Double, Double))]
marchingSegments xNodes yNodes grid lv =
  let nx = length xNodes
      ny = length yNodes
      xv = V.fromList xNodes
      yv = V.fromList yNodes
      gv = V.fromList (map V.fromList grid)
      xAt i = xv V.! i
      yAt j = yv V.! j
      zAt i j = (gv V.! j) V.! i
      cellSegs i j =
        let x0 = xAt i; x1 = xAt (i+1); y0 = yAt j; y1 = yAt (j+1)
            z00 = zAt i j;         z10 = zAt (i+1) j
            z11 = zAt (i+1) (j+1); z01 = zAt i (j+1)
            b = (if z00 >= lv then 1 else 0 :: Int)
              + (if z10 >= lv then 2 else 0)
              + (if z11 >= lv then 4 else 0)
              + (if z01 >= lv then 8 else 0)
            interp a bb (ax,ay) (bx,by) =
              let t = if bb == a then 0.5 else (lv - a) / (bb - a)
              in (ax + t*(bx-ax), ay + t*(by-ay))
            eB = interp z00 z10 (x0,y0) (x1,y0)  -- bottom
            eR = interp z10 z11 (x1,y0) (x1,y1)  -- right
            eT = interp z01 z11 (x0,y1) (x1,y1)  -- top
            eL = interp z00 z01 (x0,y0) (x0,y1)  -- left
        in case b of
             1  -> [(eL,eB)]; 2  -> [(eB,eR)]; 3  -> [(eL,eR)]
             4  -> [(eR,eT)]; 5  -> [(eL,eT),(eB,eR)]
             6  -> [(eB,eT)]; 7  -> [(eL,eT)]; 8  -> [(eT,eL)]
             9  -> [(eT,eB)]; 10 -> [(eL,eB),(eT,eR)]; 11 -> [(eT,eR)]
             12 -> [(eL,eR)]; 13 -> [(eB,eR)]; 14 -> [(eL,eB)]
             _  -> []
  in [ seg | i <- [0 .. nx - 2], j <- [0 .. ny - 2], seg <- cellSegs i j ]

-- | 既定の等高線レベル: @(zmin, zmax)@ の**内側等間隔** @lv_k = zmin +
-- (zmax-zmin)·k/(n+1)@ (k = 1..n)。 端値ちょうどの退化等値線を避ける。
-- 2D 'contourLevelsFor' の既定枝と 3D 床面投影が共有 (Phase 24 A5)。
innerLevels :: Int -> Double -> Double -> [Double]
innerLevels nLev zmin zmax =
  [ zmin + (zmax - zmin) * fromIntegral k / fromIntegral (nLev + 1)
  | k <- [1 .. max 1 nLev] ]
