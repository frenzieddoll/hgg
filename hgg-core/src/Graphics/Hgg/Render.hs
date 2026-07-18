-- |
-- Module      : Graphics.Hgg.Render
-- Description : Layer 1 ─ Renderer 抽象 (Phase 26 §A-5 Resolver 対応版)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 'VisualSpec' + 'Resolver' + 'Layout' → backend 非依存 'Primitive' 列。
-- 各 backend (SVG / PDF / Canvas / Rasterific) は 'drawPrimitives' のみ実装。
--
-- Phase 7 A4: 旧 3850 行モノリスを責務別 module に分割。 本 module は
-- 後方互換 shim = 従来の公開名を 'Render.Common' / 'Render.Special' /
-- 'Render.Layer' から re-export するのみ (出力中立・純粋移動)。
--   * "Graphics.Hgg.Render.Common"       — 型 / theme / projection / axis / color / shape / stat helper
--   * "Graphics.Hgg.Render.Basic"        — scatter/line/bar/histogram/band/step/stem
--   * "Graphics.Hgg.Render.Distribution" — box/violin/strip/swarm/raincloud/ridge
--   * "Graphics.Hgg.Render.Statistical"  — qq/ecdf/rangebar/heatmap/contour/regression/density/statline
--   * "Graphics.Hgg.Render.MCMC"         — forest/funnel/autocorr/ess
--   * "Graphics.Hgg.Render.Special"      — pie/waterfall/parallel/text/DAG
--   * "Graphics.Hgg.Render.Layer"        — orchestration + renderLayer dispatch + facet/legend/inset/marginal
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Render
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

import           Graphics.Hgg.Primitive       -- Phase 51: geometry/style/Primitive (leaf・re-export)
import           Graphics.Hgg.Render.Common
import           Graphics.Hgg.Render.Layer    (renderToPrimitives)
import           Graphics.Hgg.Render.EdgeRoute (edgePortPoint)
