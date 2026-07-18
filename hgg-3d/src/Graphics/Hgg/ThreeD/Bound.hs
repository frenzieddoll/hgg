-- |
-- Module      : Graphics.Hgg.ThreeD.Bound
-- Description : 3D の df バインド (Phase 24 A6 = 2D `df |>> spec` の 3D 対応)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 2D と同じ書き味で 3D に列名バインドを通す:
--
-- > df |>> (layer3D (scatter3D "x" "y" "z" <> colormap3D) <> title3D "...")
-- >   :: BoundPlot3D
-- > saveSVG3DBound "out.svg" bound
--
-- 'BindableSpec' (hgg-frame) の 'VisualSpec3D' instance を本 module
-- で与える (型の定義 package 側 = 非 orphan)。 検証は 2D と同方針で
-- **バインド時に値として** 'PlotDiagnostic' に格納し、 例外は投げない。
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Graphics.Hgg.ThreeD.Bound
  ( BoundPlot3D (..)
  , saveSVG3DBound
  , saveHTML3DBound
  , showBrowser3DBound
  , unBound3D
  ) where

import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (Text)
import           Data.Monoid                    (Last (..))

import           Graphics.Hgg.Spec              (ColRef (..), Resolver)
import           Graphics.Hgg.Validate          (DiagnosticContext (..),
                                                 PlotDiagnostic (..),
                                                 PlotErrorKind (..), suggest)
import           Graphics.Hgg.Frame             (BindableSpec (..),
                                                 PlotData (..),
                                                 emptyDfDiagnostics)

import           Graphics.Hgg.ThreeD.Easy       (saveSVG3D)
import           Graphics.Hgg.ThreeD.Browser    (saveHTML3D, showBrowser)
import           Graphics.Hgg.ThreeD.Spec       (Layer3D, VisualSpec3D (..),
                                                 lyr3EncX, lyr3EncY, lyr3EncZ,
                                                 lyr3TextBy, resolveSpec3D)

-- | df バインド済の 3D plot (2D 'Graphics.Hgg.Frame.BoundPlot' の 3D 版)。
data BoundPlot3D = BoundPlot3D
  { bp3Resolver    :: Resolver
  , bp3Spec        :: VisualSpec3D
  , bp3Diagnostics :: [PlotDiagnostic]
  }

instance BindableSpec VisualSpec3D where
  type BoundOf VisualSpec3D = BoundPlot3D
  bindData df spec =
    BoundPlot3D (toResolver df) spec
                (emptyDfDiagnostics df ++ checkColumns (columnNames df) spec)

-- | spec 中の 'ColByName' を df の列名と突合する (編集距離 suggestion 付き)。
checkColumns :: [Text] -> VisualSpec3D -> [PlotDiagnostic]
checkColumns known spec =
  [ PlotError (ColumnNotFound nm (suggest known nm)) (DiagnosticContext (Just i) Nothing)
  | (i, l) <- zip [0 ..] (vs3Layers spec)
  , nm <- mapMaybe byName [ getLast (lyr3EncX l)
                          , getLast (lyr3EncY l)
                          , getLast (lyr3EncZ l)
                          , getLast (lyr3TextBy l) ]
  , nm `notElem` known
  ]
 where
  byName (Just (ColByName nm)) = Just nm
  byName _                     = Nothing

-- | バインド済 3D plot を SVG 保存 (列参照を解決してから 'saveSVG3D')。
saveSVG3DBound :: FilePath -> BoundPlot3D -> IO ()
saveSVG3DBound path b = saveSVG3D path (resolveSpec3D (bp3Resolver b) (bp3Spec b))

-- | バインド済 3D plot を **WebGL self-contained HTML** として保存
--   (= 'saveSVG3DBound' の interactive 版・列参照と resolve 産物を解決してから
--   'saveHTML3D')。 df 連携 3D の browser 経路欠落を埋める (Phase 27 A2)。
saveHTML3DBound :: FilePath -> BoundPlot3D -> IO ()
saveHTML3DBound path b = saveHTML3D path (resolveSpec3D (bp3Resolver b) (bp3Spec b))

-- | バインド済 3D plot を **ブラウザで interactive 表示** ('showBrowser' の df 連携版)。
showBrowser3DBound :: BoundPlot3D -> IO ()
showBrowser3DBound b = showBrowser (resolveSpec3D (bp3Resolver b) (bp3Spec b))

-- | (Resolver, 解決済み spec) を取り出す raw 経路 (2D 'unBound' 同型)。
unBound3D :: BoundPlot3D -> (Resolver, VisualSpec3D)
unBound3D b = (bp3Resolver b, resolveSpec3D (bp3Resolver b) (bp3Spec b))
