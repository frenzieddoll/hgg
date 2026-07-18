-- | api-guide 図ジェネレータの共通基盤 (P3.1 = api-guide 再構成)。
--   ページ別モジュール (DocFig.Quickstart / DocFig.Layers / DocFig.Decoration) が
--   'Figure' のリストを公開し、 Main がまとめて 'renderFigure' する。各 'Figure' の
--   ファイル名は静的リテラルなので、 orphan ゲート (md 参照 ↔ emit 突合) が build 無しで
--   emit 名を列挙できる。
{-# LANGUAGE OverloadedStrings #-}
module DocFig.Common
  ( Figure (..)
  , outDir
  , fig, figW, figR
  , renderFigure
  , linFit, lcg
  , module Graphics.Hgg.Easy
  ) where

import           Graphics.Hgg.Easy
import           Graphics.Hgg.Unit        (px, (*~))
import           Graphics.Hgg.Backend.SVG (saveSVG, saveSVGWith)

-- | 出力先 (repo root から)。
outDir :: FilePath
outDir = "docs/api-guide/images/"

-- ===================================================================
-- 図の宣言

-- | 1 枚の図 = 出力ファイル名 + (任意) 'Resolver' + 最終 spec (サイズ適用済)。
data Figure = Figure
  { figFile     :: FilePath
  , figResolver :: Maybe Resolver
  , figSpec     :: VisualSpec
  }

-- | 既定サイズ (640×420 px) を付けた図。
fig :: FilePath -> VisualSpec -> Figure
fig name spec = Figure name Nothing
  (spec <> widthUnit (640 *~ px) <> heightUnit (420 *~ px))

-- | サイズ明示の図 (subplots / gallery 等で横長・大判)。
figW :: FilePath -> Int -> Int -> VisualSpec -> Figure
figW name w h spec = Figure name Nothing
  (spec <> widthUnit (fromIntegral w *~ px) <> heightUnit (fromIntegral h *~ px))

-- | 列名 ('ColByName') の解決に 'Resolver' が要る図用 (facet 等)。既定サイズ。
figR :: FilePath -> Resolver -> VisualSpec -> Figure
figR name r spec = Figure name (Just r)
  (spec <> widthUnit (640 *~ px) <> heightUnit (420 *~ px))

renderFigure :: Figure -> IO ()
renderFigure (Figure name Nothing  spec) = saveSVG     (outDir ++ name) spec
renderFigure (Figure name (Just r) spec) = saveSVGWith (outDir ++ name) r spec

-- ===================================================================
-- 図を安定させる小道具

-- | 最小二乗 (傾き, 切片)。
linFit :: [Double] -> [Double] -> (Double, Double)
linFit xs ys =
  let m  = fromIntegral (length xs)
      sx = sum xs; sy = sum ys
      sxx = sum (map (^ (2 :: Int)) xs)
      sxy = sum (zipWith (*) xs ys)
      a  = (m * sxy - sx * sy) / (m * sxx - sx * sx)
      b  = (sy - a * sx) / m
  in (a, b)

-- | 0..1 の決定的擬似乱数列 (線形合同法、 図を安定させる)。
lcg :: Int -> [Double]
lcg seed =
  let next s = (1103515245 * s + 12345) `mod` 2147483648
      go s = let s' = next s
             in fromIntegral s' / 2147483648 : go s'
  in go seed
