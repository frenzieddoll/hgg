-- |
-- Module      : Graphics.Hgg.Quick
-- Description : Easy 層の IO ワンショット保存 (= 1 行で SVG 出力)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: core は backend 非依存 (architecture §3.3) のため IO 保存ヘルパは
-- backend 側 (本 module) に置く。 値を渡すだけで SVG が 1 ファイル出る入門用 API。
--
-- [English]: Since core is backend-agnostic (architecture §3.3), the IO save
-- helpers live on the backend side (this module). A beginner-friendly API
-- that produces a single SVG file just by passing in values.
--
-- @
-- import Graphics.Hgg.Quick
--
-- main :: IO ()
-- main = do
--   quickScatter "scatter.svg" [1,2,3,4] [1,4,9,16]
--   quickPlot    "overlay.svg" [ points [1,2,3] [1,4,9]
--                              , lineXY [1,2,3] [1,4,9] ]
-- @
--
-- [日本語]: `Graphics.Hgg.Easy` を再 export するので、 本 module 1 つの
-- import で `points` / `lineXY` / `overlay` 等の Easy ヘルパも揃う。
--
-- [English]: Re-exports `Graphics.Hgg.Easy`, so a single import of this
-- module also brings in the Easy helpers such as `points` / `lineXY` /
-- `overlay`.
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Quick
  ( -- * Easy 層 (再 export)
    module Graphics.Hgg.Easy
    -- * IO ワンショット保存 (= 値直接受け)
  , quickScatter
  , quickLine
  , quickBar
  , quickHist
    -- * layer 群をまとめて保存
  , quickPlot
  ) where

import           Graphics.Hgg.Backend.SVG (saveSVG)
import           Graphics.Hgg.Easy

-- | [日本語]: 散布図を 1 行で SVG 保存。 @quickScatter path xs ys@。
--   [English]: Saves a scatter plot to SVG in one line. @quickScatter path xs ys@.
quickScatter :: FilePath -> [Double] -> [Double] -> IO ()
quickScatter path xs ys = quickPlot path [points xs ys]

-- | [日本語]: 折れ線を 1 行で SVG 保存。
--   [English]: Saves a line plot to SVG in one line.
quickLine :: FilePath -> [Double] -> [Double] -> IO ()
quickLine path xs ys = quickPlot path [lineXY xs ys]

-- | [日本語]: 棒グラフを 1 行で SVG 保存。
--   [English]: Saves a bar chart to SVG in one line.
quickBar :: FilePath -> [Double] -> [Double] -> IO ()
quickBar path xs ys = quickPlot path [bars xs ys]

-- | [日本語]: ヒストグラムを 1 行で SVG 保存。
--   [English]: Saves a histogram to SVG in one line.
quickHist :: FilePath -> [Double] -> IO ()
quickHist path xs = quickPlot path [hist xs]

-- | [日本語]: layer 群を 'overlay' で重畳して SVG 保存 (= 最も汎用な Easy 保存)。
--   [English]: Overlays a list of layers with 'overlay' and saves the result
--   to SVG (the most general Easy-layer save).
quickPlot :: FilePath -> [Layer] -> IO ()
quickPlot path = saveSVG path . overlay
