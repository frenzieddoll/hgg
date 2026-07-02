-- |
-- Module      : Hgg.Plot.ThreeD.Browser
-- Description : HS-side browser display driver (Phase 5 A5)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- @
-- import Hgg.Plot.ThreeD.Spec
-- import Hgg.Plot.ThreeD.Browser
-- main = showBrowser $ purePlot3D <> layer3D (scatter3D pts) <> camera (defaultCameraZUp 3)
-- @
--
-- 中身: spec を aeson で JSON 化、 PS WebGL bundle (= data-files) と inline 結合した
-- self-contained HTML を tmp に出力、 OS 別 browser-open コマンド (xdg-open / open / start) で起動。
--
-- 設計判断 (= phase-5 計画 md §2.4):
--
--   * **bundle embed 方式**: cabal data-files 採用 (= dev iteration 速さ重視、
--     PS 側更新時 HS 再 build 不要)。 将来 TH 'embedFile' 切替時は 'getBundleJS'
--     の中身だけ差替えで済む。 詳細 → @design\/bundle-embed-choice.md@
--   * **3 つの出力経路**: 'showBrowser' (= tmp + open)、 'saveHTML3D' (= 配布用、 単一 HTML)、
--     'Hgg.Plot.ThreeD.Easy.saveSVG3D' (= 静的 SVG、 Phase 3 CPU projection 経路)
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.ThreeD.Browser
  ( -- * 主要 API
    showBrowser
  , saveHTML3D
    -- * 内部 (= bundle 取得、 swap 用に export)
  , getBundleJS
  ) where

import           Data.Aeson                   (encode)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBS8
import           Paths_hgg_plot_3d        (getDataFileName)
import           System.Directory             (getTemporaryDirectory)
import           System.FilePath              ((</>))
import           System.Info                  (os)
import           System.Process               (callCommand)

import           Hgg.Plot.ThreeD.Spec     (VisualSpec3D)

-- ===========================================================================
-- bundle 取得 (= 絶縁レイヤ、 後で TH に swap 可能)
-- ===========================================================================

-- | PS WebGL bundle (= data/webgl-spec.js) を ByteString で取得。
--
-- 現在は @cabal data-files@ 経由 ('Paths_hgg_plot_3d.getDataFileName')。
-- 将来 TH 'embedFile' に切替時はこの 1 関数のみ差替えれば済むよう設計
-- (= 切替手順は @design\/bundle-embed-choice.md@)。
getBundleJS :: IO BS.ByteString
getBundleJS = do
  path <- getDataFileName "data/webgl-spec.js"
  BS.readFile path

-- ===========================================================================
-- showBrowser / saveHTML3D
-- ===========================================================================

-- | spec を **ブラウザで interactive 表示**。 tmp HTML 生成 + OS 別 browser-open。
--
-- WebGL2 backend で描画、 mouse drag で camera orbit、 wheel で zoom、
-- 右 drag で pan。 操作仕様は Phase 4 demo と同一。
showBrowser :: VisualSpec3D -> IO ()
showBrowser spec = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "hgg-3d-tmp.html"
  saveHTML3D path spec
  openInBrowser path

-- | spec を **self-contained HTML として保存** (= 配布用、 bundle inline 埋込)。
--
-- 出力ファイルは外部依存無し、 ブラウザで直接開ける。
saveHTML3D :: FilePath -> VisualSpec3D -> IO ()
saveHTML3D path spec = do
  bundleJS <- getBundleJS
  let specJSON = encode spec
      html     = mkHTML bundleJS specJSON
  LBS.writeFile path html

-- ===========================================================================
-- HTML 生成
-- ===========================================================================

-- | bundle JS + spec JSON を 1 HTML に埋込。 ESM ではなく通常 script 経路
-- (= spago bundle 出力は IIFE、 ESM import すると export named 'main' 不在で失敗、
--    Phase 4 で確認済の罠)。
mkHTML :: BS.ByteString -> LBS.ByteString -> LBS.ByteString
mkHTML bundleJS specJSON = LBS.concat
  [ "<!DOCTYPE html>\n"
  , "<html lang=\"ja\">\n"
  , "<head>\n"
  , "<meta charset=\"UTF-8\">\n"
  , "<title>hgg 3D</title>\n"
  , "<style>\n"
  -- Phase 27 A8: 2D / CPU SVG plot (tpBackground=#ffffff) と背景を揃える (白)
  , "  body { margin: 0; background: #ffffff; color: #333333;\n"
  , "         font-family: -apple-system, \"Hiragino Sans\", \"Yu Gothic UI\", sans-serif; }\n"
  , "  #plot-container { width: 100vw; height: 100vh; display: flex;\n"
  , "                    flex-direction: column; }\n"
  , "  #plot { flex: 1; cursor: grab; background: #ffffff; }\n"
  , "  #plot:active { cursor: grabbing; }\n"
  , "  #info { padding: 8px 16px; font-size: 12px; color: #666666;\n"
  , "          border-top: 1px solid #dddddd; }\n"
  , "</style>\n"
  , "</head>\n"
  , "<body>\n"
  , "<div id=\"plot-container\">\n"
  , "  <canvas id=\"plot\" width=\"1024\" height=\"720\"></canvas>\n"
  , "  <div id=\"info\">左ドラッグ orbit / 右ドラッグ pan / wheel zoom</div>\n"
  , "</div>\n"
  , "<script>\n"
  , LBS.fromStrict bundleJS
  , "\n</script>\n"
  , "<script>\n"
  , "  // spec JSON を inline 埋込、 PS Examples.WebGLPlot.renderFromSpec で描画\n"
  , "  var SPEC = "
  , specJSON
  , ";\n"
  , "  if (window.hggPlot && window.hggPlot.renderFromSpec) {\n"
  , "    window.hggPlot.renderFromSpec(\"plot\", SPEC);\n"
  , "  } else {\n"
  , "    console.error(\"[hgg] window.hggPlot not initialized\");\n"
  , "  }\n"
  , "</script>\n"
  , "</body>\n"
  , "</html>\n"
  ]
  where
    _unused = LBS8.pack ""  -- LBS8 import を保持 (= 将来 debug println で使用)

-- ===========================================================================
-- OS 別 browser-open
-- ===========================================================================

-- | OS 検出して xdg-open (Linux) / open (macOS) / start (Windows) を呼ぶ。
--
-- WSL は @os == \"linux\"@ で xdg-open 経路。 wslview などインストール済なら動作。
-- 失敗時は path を stdout に出すので手動で開ける。
openInBrowser :: FilePath -> IO ()
openInBrowser path = case os of
  "linux"   -> safeCall ("xdg-open " <> quote path)
  "darwin"  -> safeCall ("open "     <> quote path)
  "mingw32" -> safeCall ("start "    <> quote path)  -- Windows GHC
  other     -> do
    putStrLn $ "[hgg] unknown OS '" <> other <> "', open manually: " <> path
  where
    safeCall cmd = do
      putStrLn $ "[hgg] " <> cmd
      callCommand cmd

    -- path にスペース含むケースに備えて double quote。 簡略化のため escape 無し
    -- (= tmp dir path に通常スペースは無いが念のため)
    quote p = "\"" <> p <> "\""
