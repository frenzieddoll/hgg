-- |
-- Module      : Hgg.Plot.Render
-- Description : Layer 1 ─ Renderer 抽象 (Phase 26 §A-5 Resolver 対応版)
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- 'VisualSpec' + 'Resolver' + 'Layout' → backend 非依存 'Primitive' 列。
-- 各 backend (SVG / PDF / Canvas / Rasterific) は 'drawPrimitives' のみ実装。
--
-- Phase 7 A4: 旧 3850 行モノリスを責務別 module に分割。 本 module は
-- 後方互換 shim = 従来の公開名を 'Render.Common' / 'Render.Special' /
-- 'Render.Layer' から re-export するのみ (出力中立・純粋移動)。
--   * "Hgg.Plot.Render.Common"       — 型 / theme / projection / axis / color / shape / stat helper
--   * "Hgg.Plot.Render.Basic"        — scatter/line/bar/histogram/band/step/stem
--   * "Hgg.Plot.Render.Distribution" — box/violin/strip/swarm/raincloud/ridge
--   * "Hgg.Plot.Render.Statistical"  — qq/ecdf/rangebar/heatmap/contour/regression/density/statline
--   * "Hgg.Plot.Render.MCMC"         — forest/funnel/autocorr/ess
--   * "Hgg.Plot.Render.Special"      — pie/waterfall/parallel/text/DAG
--   * "Hgg.Plot.Render.Layer"        — orchestration + renderLayer dispatch + facet/legend/inset/marginal
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Render
  ( -- * Geometry / Style
    Point(..)
  , LineStyle(..)
  , solid
  , FillStyle(..)
  , StrokeStyle(..)
  , TextStyle(..)
  , TextAnchor(..)
  , Transform(..)
  , PathSegment(..)
    -- * Theme palette
  , ThemePalette(..)
  , themePalette
    -- * Primitive
  , Primitive(..)
    -- * Phase 33 B5: pt→device scale (backend の唯一の dpi 適用点)
  , scalePrimitives
    -- * 変換
  , renderToPrimitives
    -- * Backend interface
  , Renderer(..)
    -- * Phase 1 A7: edge port
  , edgePortPoint
  ) where

import           Hgg.Plot.Render.Common
import           Hgg.Plot.Render.Layer    (renderToPrimitives)
import           Hgg.Plot.Render.EdgeRoute (edgePortPoint)
