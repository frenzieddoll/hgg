-- | Phase 12 iHaskell backend の単体 test。
-- ghci 相当の確認: 'display' が SVG を含む 'Display' を返すこと。
-- DisplayPlot は Phase 14 で deprecated だが後方互換確認のため残す → 警告抑制。
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main (main) where

import           Graphics.Hgg.Easy     (layer, points, scatter, title)
import           Graphics.Hgg.Frame    (BoundPlot, (|>>))
import           Graphics.Hgg.IHaskell (DisplayPlot (..))
import           Graphics.Hgg.Spec     (ColData (..), ColRef (..), Resolver,
                                        VisualSpec)
import           Control.Monad         (unless)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Vector           as V
import           IHaskell.Display      (Display (..), display)
import           System.Exit           (exitFailure)

-- inline 列のみの図 (Resolver 不要)
demoSpec :: VisualSpec
demoSpec = layer (points [0, 1, 2, 3] [0, 1, 4, 9]) <> title "demo"

-- ColByName を含む図 (= Resolver 必須)
namedSpec :: VisualSpec
namedSpec = layer (scatter (ColByName "x") (ColByName "y")) <> title "named"

-- 列名 → 実 Vector を返す Resolver
demoResolver :: Resolver
demoResolver "x" = Just (NumData (V.fromList [0, 1, 2, 3]))
demoResolver "y" = Just (NumData (V.fromList [0, 1, 4, 9]))
demoResolver _   = Nothing

-- df (Map) → BoundPlot (Phase 14 A6 の正規ルート)
demoDF :: Map Text ColData
demoDF = M.fromList
  [ ("x", NumData (V.fromList [0, 1, 2, 3]))
  , ("y", NumData (V.fromList [0, 1, 4, 9])) ]

boundPlot :: BoundPlot
boundPlot = demoDF |>> (layer (scatter (ColByName "x") (ColByName "y")) <> title "bound")

displayLen :: Display -> Int
displayLen (Display ds)    = length ds
displayLen (ManyDisplay _) = -1

main :: IO ()
main = do
  -- A2 完了条件: inline 図 display が Display [<one DisplayData>] を返す
  dInline <- display demoSpec
  check "inline: display returns Display with one DisplayData"
        (displayLen dInline == 1)
  -- Phase 14 A6: BoundPlot (df |>> spec) を display できる (正規ルート)
  dBound <- display boundPlot
  check "bound: BoundPlot display returns Display with one DisplayData"
        (displayLen dBound == 1)
  -- 後方互換: 旧 DisplayPlot も依然 display できる (deprecated だが機能維持)
  dNamed <- display (DisplayPlot (demoResolver, namedSpec))
  check "named: DisplayPlot (deprecated) display still returns one DisplayData"
        (displayLen dNamed == 1)
  putStrLn "hgg-ihaskell: all tests passed"

check :: String -> Bool -> IO ()
check label ok = unless ok $ do
  putStrLn ("FAIL: " ++ label)
  exitFailure
