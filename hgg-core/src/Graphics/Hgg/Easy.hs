-- |
-- Module      : Graphics.Hgg.Easy
-- Description : Layer 1 ─ 入門用 Easy API (= 値直接受け + overlay 既定) + Spec 再 export
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 4 層 API 設計のうち **Easy 層**。
-- grammar API (`Graphics.Hgg.Spec`) を**そのまま再 export** した上で、
-- 「`inline` を書かずに `[Double]` を直接渡す」 専用ヘルパを足す (= 別名方式)。
--
-- @
-- import Graphics.Hgg.Easy
-- import Graphics.Hgg.Quick (quickScatter)   -- IO 保存は svg package 側
--
-- -- 1 行で散布図を保存
-- main = quickScatter "q.svg" [1,2,3] [1,4,9]
--
-- -- overlay を既定にした重畳 (footgun 回避: layer 包み不要)
-- fig = overlay [ points [1,2,3] [1,4,9]
--               , lineXY [1,2,3] [1,4,9] ]
-- @
--
-- 設計: Easy helper は 'Layer' を返す (grammar と同じ合成性)。 重畳は 'overlay' で
-- 包む (= 'scatter' '<>' 'line' の落とし穴を回避、 @design/monoid-semantics.md@ §1)。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Easy
  ( -- * grammar API 全体 (再 export)
    module Graphics.Hgg.Spec
    -- * Easy 値直接受けヘルパ (= inline 不要)
  , points
  , lineXY
  , bars
  , hist
  , plotY
    -- * overlay (= 重畳を既定に)
  , overlay
  , plots
  ) where

import           Graphics.Hgg.Spec

-- | 散布図 (= 'scatter' の値直接受け版)。 @points xs ys@ は @scatter (inline xs) (inline ys)@。
points :: [Double] -> [Double] -> Layer
points xs ys = scatter (inline xs) (inline ys)

-- | 折れ線 (= 'line' の値直接受け版)。
lineXY :: [Double] -> [Double] -> Layer
lineXY xs ys = line (inline xs) (inline ys)

-- | 棒 (= 'bar' の値直接受け版)。
bars :: [Double] -> [Double] -> Layer
bars xs ys = bar (inline xs) (inline ys)

-- | ヒストグラム (= 'histogram' の値直接受け版)。
hist :: [Double] -> Layer
hist xs = histogram (inline xs)

-- | 片軸プロット: index (0,1,2,…) を x に取った散布図。
plotY :: [Double] -> Layer
plotY ys = points (map fromIntegral [0 .. length ys - 1]) ys

-- | layer 群を重ね合わせた 'VisualSpec' (= @foldMap layer@)。 入門者はこれで
-- 重畳を書く (`scatter <> line` の落とし穴を避ける)。
overlay :: [Layer] -> VisualSpec
overlay = foldMap layer

-- | 'overlay' の別名 (= 複数 plot を「並べる」 ニュアンスの短名)。
plots :: [Layer] -> VisualSpec
plots = overlay
