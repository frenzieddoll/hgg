-- |
-- Module      : Graphics.Hgg.Easy
-- Description : Layer 1 (Easy API) — direct-value helpers, overlay by default, Spec re-export
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 4 層 API 設計のうち __Easy 層__。
--   grammar API (`Graphics.Hgg.Spec`) を__そのまま再 export__ した上で、
--   「`inline` を書かずに `[Double]` を直接渡す」 専用ヘルパを足す (= 別名方式)。
--   [English]: The __Easy layer__ of the 4-layer API design. Re-exports the
--   grammar API (`Graphics.Hgg.Spec`) __as-is__, and adds dedicated helpers
--   that let you pass a `[Double]` directly without writing `inline` (an
--   alias-based approach).
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
-- [日本語]: 設計: Easy helper は 'Layer' を返す (grammar と同じ合成性)。 重畳は
--   'overlay' で包む (= 'scatter' '<>' 'line' の落とし穴を回避、
--   @design/monoid-semantics.md@ §1)。
--   [English]: Design: Easy helpers return a 'Layer' (the same composability
--   as grammar). Overlaying is done by wrapping with 'overlay' (avoiding the
--   'scatter' '<>' 'line' pitfall; see @design/monoid-semantics.md@ §1).
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

-- | [日本語]: 散布図 (= 'scatter' の値直接受け版)。 @points xs ys@ は
--   @scatter (inline xs) (inline ys)@。
--   [English]: A scatter plot (the direct-value version of 'scatter').
--   @points xs ys@ is @scatter (inline xs) (inline ys)@.
points :: [Double] -> [Double] -> Layer
points xs ys = scatter (inline xs) (inline ys)

-- | [日本語]: 折れ線 (= 'line' の値直接受け版)。
--   [English]: A line plot (the direct-value version of 'line').
lineXY :: [Double] -> [Double] -> Layer
lineXY xs ys = line (inline xs) (inline ys)

-- | [日本語]: 棒 (= 'bar' の値直接受け版)。
--   [English]: A bar chart (the direct-value version of 'bar').
bars :: [Double] -> [Double] -> Layer
bars xs ys = bar (inline xs) (inline ys)

-- | [日本語]: ヒストグラム (= 'histogram' の値直接受け版)。
--   [English]: A histogram (the direct-value version of 'histogram').
hist :: [Double] -> Layer
hist xs = histogram (inline xs)

-- | [日本語]: 片軸プロット: index (0,1,2,…) を x に取った散布図。
--   [English]: A single-axis plot: a scatter plot using the index
--   (0, 1, 2, …) as x.
plotY :: [Double] -> Layer
plotY ys = points (map fromIntegral [0 .. length ys - 1]) ys

-- | [日本語]: layer 群を重ね合わせた 'VisualSpec' (= @foldMap layer@)。 入門者は
--   これで重畳を書く (`scatter <> line` の落とし穴を避ける)。
--   [English]: A 'VisualSpec' that overlays a group of layers (@foldMap
--   layer@). Beginners should use this to overlay plots, avoiding the
--   `scatter <> line` pitfall.
overlay :: [Layer] -> VisualSpec
overlay = foldMap layer

-- | [日本語]: 'overlay' の別名 (= 複数 plot を「並べる」 ニュアンスの短名)。
--   [English]: An alias for 'overlay', with a shorter name conveying the
--   nuance of "arranging" multiple plots.
plots :: [Layer] -> VisualSpec
plots = overlay
