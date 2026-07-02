-- |
-- Module      : Hgg.Plot.Quick
-- Description : Easy 層の IO ワンショット保存 (= 1 行で SVG 出力、 Phase 11 A3)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- core は backend 非依存 (architecture §3.3) のため IO 保存ヘルパは backend 側
-- (本 module) に置く。 値を渡すだけで SVG が 1 ファイル出る入門用 API。
--
-- @
-- import Hgg.Plot.Quick
--
-- main :: IO ()
-- main = do
--   quickScatter "scatter.svg" [1,2,3,4] [1,4,9,16]
--   quickPlot    "overlay.svg" [ points [1,2,3] [1,4,9]
--                              , lineXY [1,2,3] [1,4,9] ]
-- @
--
-- `Hgg.Plot.Easy` を再 export するので、 本 module 1 つの import で
-- `points` / `lineXY` / `overlay` 等の Easy ヘルパも揃う。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Quick
  ( -- * Easy 層 (再 export)
    module Hgg.Plot.Easy
    -- * IO ワンショット保存 (= 値直接受け)
  , quickScatter
  , quickLine
  , quickBar
  , quickHist
    -- * layer 群をまとめて保存
  , quickPlot
  ) where

import           Hgg.Plot.Backend.SVG (saveSVG)
import           Hgg.Plot.Easy

-- | 散布図を 1 行で SVG 保存。 @quickScatter path xs ys@。
quickScatter :: FilePath -> [Double] -> [Double] -> IO ()
quickScatter path xs ys = quickPlot path [points xs ys]

-- | 折れ線を 1 行で SVG 保存。
quickLine :: FilePath -> [Double] -> [Double] -> IO ()
quickLine path xs ys = quickPlot path [lineXY xs ys]

-- | 棒グラフを 1 行で SVG 保存。
quickBar :: FilePath -> [Double] -> [Double] -> IO ()
quickBar path xs ys = quickPlot path [bars xs ys]

-- | ヒストグラムを 1 行で SVG 保存。
quickHist :: FilePath -> [Double] -> IO ()
quickHist path xs = quickPlot path [hist xs]

-- | layer 群を 'overlay' で重畳して SVG 保存 (= 最も汎用な Easy 保存)。
quickPlot :: FilePath -> [Layer] -> IO ()
quickPlot path = saveSVG path . overlay
