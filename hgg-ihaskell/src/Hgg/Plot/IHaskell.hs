-- |
-- Module      : Hgg.Plot.IHaskell
-- Description : iHaskell (Jupyter) inline display 配線 (Phase 12)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Jupyter (iHaskell kernel) のセルで hgg の図をインライン描画する
-- ための薄い配線。 描画は SVG backend の純関数 'renderSVG' をそのまま使い、
-- iHaskell の 'svg' display helper に 'Text' を渡すだけ (新規描画コード無し)。
--
-- == 使い方 (Jupyter セル)
--
-- @
-- import Hgg.Plot.Easy
-- import Hgg.Plot.IHaskell ()   -- インスタンスを見せるだけで良い
--
-- layer (points [0,1,2,3] [0,1,4,9]) <> title \"demo\"   -- ← セル評価でインライン描画
-- @
--
-- 'ColByName' を含む図 (= Resolver 必須) は 'BoundPlot' (@df |>> spec@) を使う。
-- 旧 'DisplayPlot' は Phase 14 で 'BoundPlot' に統合され deprecated。

-- orphan instance を意図的に許可: 'IHaskellDisplay' (ihaskell) を 'VisualSpec' /
-- 'BoundPlot' (core / frame) に与えるのは、 ihaskell 依存を本 package に隔離する
-- 設計上必須。
{-# OPTIONS_GHC -Wno-orphans #-}
module Hgg.Plot.IHaskell
  ( -- * df バインド済図 (= 'ColByName' を含む図、 Phase 14 で正規化)
    BoundPlot (..)
    -- * 旧 Resolver 同伴図 (deprecated → 'BoundPlot' へ)
  , DisplayPlot (..)
    -- * 明示 helper (インスタンスに頼らず関数で出す場合)
  , displaySVG
  ) where

import           Hgg.Plot.Backend.SVG (renderSVG, renderSVGWith)
import           Hgg.Plot.Frame       (BoundPlot (..))
import           Hgg.Plot.Spec        (Resolver, VisualSpec)
import           IHaskell.Display         (Display (..), IHaskellDisplay (..),
                                           svg)

-- ============================================================================
-- inline 図 (Resolver 不要 = Easy 層 / tutorial 系)
-- ============================================================================

-- | inline 列のみで構成された図 ('ColByName' を含まない) をセル評価値として
-- 直接インライン描画する。 'renderSVG' に 'emptyResolver' を渡す。
--
-- orphan instance (型は core、 class は ihaskell) だが、 ihaskell 表示配線は
-- 本 package に隔離する設計上意図的なもの。
instance IHaskellDisplay VisualSpec where
  display spec = pure (Display [svg (renderSVG spec)])

-- ============================================================================
-- df バインド済図 (= ColByName を含む図、 Phase 14 で正規化)
-- ============================================================================

-- | 'BoundPlot' (= @df |>> spec@ の結果) をセル評価値として描画する。
-- 'bpResolver' で 'ColByName' を解決し 'renderSVG' で SVG 化する。
-- 検証診断 ('bpDiagnostics') はインライン表示では無視する (Jupyter の stderr
-- には出ない経路ゆえ。 必要なら利用者が 'bpDiagnostics' を直接見る)。
instance IHaskellDisplay BoundPlot where
  display (BoundPlot r spec _) = pure (Display [svg (renderSVGWith r spec)])

-- ============================================================================
-- 旧 Resolver 同伴図 (deprecated → BoundPlot)
-- ============================================================================

-- | 'Resolver' と 'VisualSpec' を束ねた表示用 newtype。
--
-- __Deprecated__: Phase 14 で 'BoundPlot' (@df |>> spec@) に統合された。
-- 新規コードは 'BoundPlot' を使うこと。 当面は前方互換のため併存する。
newtype DisplayPlot = DisplayPlot (Resolver, VisualSpec)
{-# DEPRECATED DisplayPlot
      "Phase 14 で BoundPlot (Hgg.Plot.Frame、 df |>> spec) に統合されました。 BoundPlot を使ってください。" #-}

instance IHaskellDisplay DisplayPlot where
  display (DisplayPlot (r, spec)) = pure (Display [svg (renderSVGWith r spec)])

-- ============================================================================
-- 明示 helper
-- ============================================================================

-- | 'Resolver' を明示して図を 'Display' にする。 inline 図なら
-- @displaySVG emptyResolver spec@、 'ColByName' を含む図なら実 resolver を渡す。
displaySVG :: Resolver -> VisualSpec -> Display
displaySVG r spec = Display [svg (renderSVGWith r spec)]
