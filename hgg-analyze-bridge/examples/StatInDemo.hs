{-# LANGUAGE OverloadedStrings #-}

-- | 系統 B (ggplot 風スタット・イン) デモ (Phase 16)。
--
--   df を 1 回参照し、 stat layer (statLm / statSmooth) を通常 geom と同じく @<>@ で重ねる。
--   描画は 'saveSVGBoundStats' = bpResolver で resolveStats してから SVG 保存 (回帰計算は
--   hanalyze に委譲、 = ggplot @geom_smooth(method="lm")@ 相当)。 装飾も通常 geom と同じ。
--   出力 = design/stat-in/*.svg (gitignore 想定)。 rsvg で PNG 化して目視。
module Main where

import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Vector              as V

import           Graphics.Hgg.Bridge.Stat (saveSVGBoundStats)
import           Graphics.Hgg.Frame       ((|>>))
import           Graphics.Hgg.Spec        (ColData (..), color, colorBy, fromHex,
                                           layer, scatter, statLm, statLmLevel,
                                           statPoly, statResid, statSmooth,
                                           statSmoothCI, stroke, title)
import           System.Directory         (createDirectoryIfMissing)

-- 線形データ: y = 2x + 3 + 軽いノイズ。
linX, linY :: V.Vector Double
linX = V.fromList [1 .. 30]
linY = V.fromList [ 2 * x + 3 + noise i | (i, x) <- zip [0 ..] [1 .. 30] ]
  where noise i = 2.5 * sin (fromIntegral (i :: Int) * 1.3)

-- 非線形データ: y = sin(x) 波形 + ノイズ (smooth 用)。
smX, smY :: V.Vector Double
smX = V.fromList [ fromIntegral i * 0.3 | i <- [0 .. 40 :: Int] ]
smY = V.fromList [ sin (fromIntegral i * 0.3) + 0.15 * c i | i <- [0 .. 40 :: Int] ]
  where c i = sin (fromIntegral i * 2.7)

-- 二次データ: y = 0.5x² - 4x + 10 + ノイズ (poly 用)。
quX, quY :: V.Vector Double
quX = V.fromList [ fromIntegral i * 0.5 | i <- [0 .. 30 :: Int] ]
quY = V.fromList [ 0.5 * x * x - 4 * x + 10 + 3 * sin (fromIntegral i * 1.7)
                 | (i, x) <- zip [0 :: Int ..] [ fromIntegral j * 0.5 | j <- [0 .. 30 :: Int] ] ]

-- 2 群データ (B2 group 別 fit 用): g="A" 傾き 1.5 / g="B" 傾き 3.5、 各 20 点。
grpX, grpY :: V.Vector Double
grpG :: V.Vector Text
grpX = V.fromList (xa ++ xa) where xa = [1 .. 20]
grpY = V.fromList (map (\x -> 1.5 * x + 2 + nz x) xa
                ++ map (\x -> 3.5 * x + 2 + nz x) xa)
  where xa = [1 .. 20]; nz x = 2.0 * sin (x * 1.9)
grpG = V.fromList (replicate 20 "A" ++ replicate 20 "B")

main :: IO ()
main = do
  createDirectoryIfMissing True "design/stat-in"

  -- (1) lm: 散布図 + 回帰線 + 95% 信頼帯。 df 1 回参照・装飾込み (ggplot geom_smooth(method="lm") 相当)
  let dfLin = M.fromList [ ("x", NumData linX), ("y", NumData linY) ] :: M.Map Text ColData
  saveSVGBoundStats "design/stat-in/lm-stat-in.svg" $
    dfLin |>> ( layer (scatter "x" "y")
              <> layer (statLm "x" "y" <> color (fromHex "#d62728") <> stroke 2)
              <> title "stat-in: lm (df 1 回・装飾込み)" )

  -- (2) smooth: 散布図 + B-spline 平滑曲線 (帯なし)
  let dfSm = M.fromList [ ("x", NumData smX), ("y", NumData smY) ] :: M.Map Text ColData
  saveSVGBoundStats "design/stat-in/smooth-stat-in.svg" $
    dfSm |>> ( layer (scatter "x" "y")
             <> layer (statSmooth "x" "y" 6 <> color (fromHex "#1f77b4") <> stroke 2)
             <> title "stat-in: smooth (B-spline)" )

  -- (3) B1: statLmLevel 0.99 = 信頼水準を可変に (帯が 95% より広い)
  saveSVGBoundStats "design/stat-in/lm-level99-stat-in.svg" $
    dfLin |>> ( layer (scatter "x" "y")
              <> layer (statLmLevel "x" "y" 0.99 <> color (fromHex "#d62728") <> stroke 2)
              <> title "stat-in: statLmLevel 0.99 (帯が広い)" )

  -- (4) B1: statSmoothCI = B-spline 平滑 + 信頼帯 (statSmooth は帯なし)
  saveSVGBoundStats "design/stat-in/smooth-ci-stat-in.svg" $
    dfSm |>> ( layer (scatter "x" "y")
             <> layer (statSmoothCI "x" "y" 6 <> color (fromHex "#1f77b4") <> stroke 2)
             <> title "stat-in: statSmoothCI (B-spline + 帯)" )

  -- (5) B3: statPoly = 多項式回帰 (deg=2) + 信頼帯
  let dfQu = M.fromList [ ("x", NumData quX), ("y", NumData quY) ] :: M.Map Text ColData
  saveSVGBoundStats "design/stat-in/poly-stat-in.svg" $
    dfQu |>> ( layer (scatter "x" "y")
             <> layer (statPoly "x" "y" 2 <> color (fromHex "#2ca02c") <> stroke 2)
             <> title "stat-in: statPoly deg=2 (二次回帰 + 帯)" )

  -- (6) B3: statResid = 残差 vs fitted 診断散布 (= base R plot(lm) #1)
  saveSVGBoundStats "design/stat-in/resid-stat-in.svg" $
    dfQu |>> ( layer (statResid "x" "y" <> color (fromHex "#9467bd"))
             <> title "stat-in: statResid (残差 vs fitted)" )

  -- (7) B2: group 別 fit = statLm <> colorBy "g" (群ごとに回帰線+帯を ggplotHue で色分け)
  let dfGrp = M.fromList [ ("x", NumData grpX), ("y", NumData grpY)
                         , ("g", TxtData grpG) ] :: M.Map Text ColData
  saveSVGBoundStats "design/stat-in/group-lm-stat-in.svg" $
    dfGrp |>> ( layer (scatter "x" "y" <> colorBy "g")
              <> layer (statLm "x" "y" <> colorBy "g" <> stroke 2)
              <> title "stat-in: group 別 lm (statLm <> colorBy g)" )

  putStrLn "wrote design/stat-in/*.svg (7 files incl group-lm)"
