-- |
-- Module      : Graphics.Hgg.ThreeD.Delaunay
-- Description : 2D Delaunay 三角分割 (Phase 26 A5・trisurf 用)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 不規則 (非 grid) 点群を 'trisurf' で曲面化するための **純 Haskell** 2D Delaunay
-- 三角分割。 外部ライブラリ / バイナリ依存ゼロ (= spec §10.7・graphviz parity と
-- 同じ「自己完結」 方針)。 アルゴリズムは Bowyer-Watson の素朴版 (O(n²) 程度・
-- trisurf の点数は数十〜低数百なので十分)。
--
-- 入力は (x,y) 列。 z は呼び出し側で分割後に各頂点へ持ち上げる (= trisurf は
-- (x,y) 平面の 2D 分割だけで足り、 3D 四面体分割は不要)。 退化 (共円・共線) は
-- incircle 述語の符号で素朴に扱う (demo 用途では実害なし)。
{-# LANGUAGE BangPatterns #-}
module Graphics.Hgg.ThreeD.Delaunay
  ( delaunay2D
  ) where

import           Data.List       (foldl')
import qualified Data.Map.Strict as M

-- | (x,y) 点列を 2D Delaunay 三角分割し、 三角形を元の点配列への index 三つ組
--   @(i,j,k)@ で返す。 点が 3 未満 / 全点共線などで三角形が作れない時は @[]@。
delaunay2D :: [(Double, Double)] -> [(Int, Int, Int)]
delaunay2D pts0
  | n < 3     = []
  | otherwise = [ t | t@(a, b, c) <- finalTris, a < n, b < n, c < n ]
  where
    n = length pts0
    xs = map fst pts0
    ys = map snd pts0
    minx = minimum xs; maxx = maximum xs
    miny = minimum ys; maxy = maximum ys
    dmax = max (maxx - minx) (maxy - miny)
    d    = if dmax <= 0 then 1 else dmax
    midx = (minx + maxx) / 2
    midy = (miny + maxy) / 2
    -- super-triangle = 全点を内包する十分大きな三角形。 頂点 index は n, n+1, n+2。
    super = [ (midx - 20 * d, midy - d)
            , (midx,          midy + 20 * d)
            , (midx + 20 * d, midy - d) ]
    allPts = M.fromList (zip [0 ..] (pts0 ++ super))
    pAt i  = allPts M.! i

    initTris = [(n, n + 1, n + 2)]
    finalTris = foldl' addPoint initTris [0 .. n - 1]

    -- 点 i を挿入: bad triangle (外接円に i を含む) を抜き、 空いた多角形穴の
    -- 境界辺ごとに i と結ぶ三角形を張る。
    addPoint tris i =
      let (bad, good) = partitionBad i tris
          edges       = concatMap triEdges bad
          -- 境界辺 = 逆向き辺が無い (= 2 三角形で共有されていない) 辺
          boundary    = [ (a, b) | (a, b) <- edges, (b, a) `notElem` edges ]
          newTris     = [ (a, b, i) | (a, b) <- boundary ]
      in good ++ newTris

    partitionBad i = foldr step ([], [])
      where
        step t@(a, b, c) (bs, gs)
          | inCircle (pAt a) (pAt b) (pAt c) (pAt i) = (t : bs, gs)
          | otherwise                                = (bs, t : gs)

    triEdges (a, b, c) = [(a, b), (b, c), (c, a)]

-- | 点 d が三角形 (a,b,c) の外接円の内部か。 三角形の向き (CCW/CW) を符号で吸収。
inCircle :: (Double, Double) -> (Double, Double) -> (Double, Double)
         -> (Double, Double) -> Bool
inCircle (ax, ay) (bx, by) (cx, cy) (dx, dy) =
  let orient = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
      adx = ax - dx; ady = ay - dy
      bdx = bx - dx; bdy = by - dy
      cdx = cx - dx; cdy = cy - dy
      det = (adx * adx + ady * ady) * (bdx * cdy - cdx * bdy)
          - (bdx * bdx + bdy * bdy) * (adx * cdy - cdx * ady)
          + (cdx * cdx + cdy * cdy) * (adx * bdy - bdx * ady)
  in if orient > 0 then det > 0 else det < 0
