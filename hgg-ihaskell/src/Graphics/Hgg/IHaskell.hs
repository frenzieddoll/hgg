-- |
-- Module      : Graphics.Hgg.IHaskell
-- Description : iHaskell (Jupyter) inline display wiring
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Jupyter (iHaskell kernel) のセルで hgg の図をインライン描画する
-- ための薄い配線。 描画は SVG backend の純関数 'renderSVG' をそのまま使い、
-- iHaskell の 'svg' display helper に @Text@ を渡すだけ (新規描画コード無し)。
--
-- [English]: Thin wiring that renders hgg figures inline in Jupyter
-- (iHaskell kernel) cells. Rendering reuses the SVG backend's pure function
-- 'renderSVG' as-is and merely hands a @Text@ to iHaskell's 'svg' display
-- helper (no new rendering code).
--
-- == 使い方 (Jupyter セル) / Usage (Jupyter cell)
--
-- @
-- import Graphics.Hgg.Easy
-- import Graphics.Hgg.IHaskell ()   -- インスタンスを見せるだけで良い
--
-- layer (points [0,1,2,3] [0,1,4,9]) <> title \"demo\"   -- ← セル評価でインライン描画
-- @
--
-- [日本語]: @ColByName@ を含む図 (= Resolver 必須) は 'BoundPlot' (@df |>> spec@) を使う。
-- 旧 'DisplayPlot' は 'BoundPlot' に統合され deprecated。
--
-- [English]: For figures that contain @ColByName@ (and therefore need a
-- Resolver), use 'BoundPlot' (@df |>> spec@). The older 'DisplayPlot' has
-- been folded into 'BoundPlot' and is deprecated.

-- orphan instance を意図的に許可: 'IHaskellDisplay' (ihaskell) を 'VisualSpec' /
-- 'BoundPlot' (core / frame) に与えるのは、 ihaskell 依存を本 package に隔離する
-- 設計上必須。
{-# OPTIONS_GHC -Wno-orphans #-}
module Graphics.Hgg.IHaskell
  ( -- * df バインド済図 (= @ColByName@ を含む図、 正規化済)
    BoundPlot (..)
    -- * 旧 Resolver 同伴図 (deprecated → 'BoundPlot' へ)
  , DisplayPlot (..)
    -- * 明示 helper (インスタンスに頼らず関数で出す場合)
  , displaySVG
  ) where

import           Graphics.Hgg.Backend.SVG (renderSVG, renderSVGWith)
import           Graphics.Hgg.Frame       (BoundPlot (..))
import           Graphics.Hgg.Spec        (Resolver, VisualSpec)
import           IHaskell.Display         (Display (..), IHaskellDisplay (..),
                                           svg)

-- ============================================================================
-- inline 図 (Resolver 不要 = Easy 層 / tutorial 系)
-- ============================================================================

-- | [日本語]: inline 列のみで構成された図 (@ColByName@ を含まない) をセル評価値として
--   直接インライン描画する。 'renderSVG' に @emptyResolver@ を渡す。
--
--   orphan instance (型は core、 class は ihaskell) だが、 ihaskell 表示配線は
--   本 package に隔離する設計上意図的なもの。
--   [English]: Renders a figure composed only of inline columns (containing
--   no @ColByName@) directly inline as a cell evaluation value, by passing
--   @emptyResolver@ to 'renderSVG'.
--
--   This is an orphan instance (the type lives in core, the class in
--   ihaskell), but isolating the ihaskell display wiring into this package
--   is deliberate by design.
instance IHaskellDisplay VisualSpec where
  display spec = pure (Display [svg (renderSVG spec)])

-- ============================================================================
-- df バインド済図 (= ColByName を含む図、 正規化済)
-- ============================================================================

-- | [日本語]: 'BoundPlot' (= @df |>> spec@ の結果) をセル評価値として描画する。
--   'bpResolver' で @ColByName@ を解決し 'renderSVG' で SVG 化する。
--   検証診断 ('bpDiagnostics') はインライン表示では無視する (Jupyter の stderr
--   には出ない経路ゆえ。 必要なら利用者が 'bpDiagnostics' を直接見る)。
--   [English]: Renders a 'BoundPlot' (the result of @df |>> spec@) as a cell
--   evaluation value. Resolves @ColByName@ via 'bpResolver' and renders to
--   SVG via 'renderSVG'. Validation diagnostics ('bpDiagnostics') are
--   ignored by the inline display (there is no path to Jupyter's stderr; if
--   needed, the caller can inspect 'bpDiagnostics' directly).
instance IHaskellDisplay BoundPlot where
  display (BoundPlot r spec _) = pure (Display [svg (renderSVGWith r spec)])

-- ============================================================================
-- 旧 Resolver 同伴図 (deprecated → BoundPlot)
-- ============================================================================

-- | [日本語]: 'Resolver' と 'VisualSpec' を束ねた表示用 newtype。
--
--   __Deprecated__: 'BoundPlot' (@df |>> spec@) に統合された。
--   新規コードは 'BoundPlot' を使うこと。 当面は前方互換のため併存する。
--   [English]: A newtype for display that bundles a 'Resolver' with a
--   'VisualSpec'.
--
--   __Deprecated__: superseded by 'BoundPlot' (@df |>> spec@). Use
--   'BoundPlot' in new code. Retained for now for backward compatibility.
newtype DisplayPlot = DisplayPlot (Resolver, VisualSpec)
{-# DEPRECATED DisplayPlot
      "Phase 14 で BoundPlot (Graphics.Hgg.Frame、 df |>> spec) に統合されました。 BoundPlot を使ってください。" #-}

instance IHaskellDisplay DisplayPlot where
  display (DisplayPlot (r, spec)) = pure (Display [svg (renderSVGWith r spec)])

-- ============================================================================
-- 明示 helper
-- ============================================================================

-- | [日本語]: 'Resolver' を明示して図を 'Display' にする。 inline 図なら
--   @displaySVG emptyResolver spec@、 @ColByName@ を含む図なら実 resolver を渡す。
--   [English]: Turns a figure into a 'Display' with an explicit 'Resolver'.
--   For an inline figure, use @displaySVG emptyResolver spec@; for a figure
--   containing @ColByName@, pass a real resolver.
displaySVG :: Resolver -> VisualSpec -> Display
displaySVG r spec = Display [svg (renderSVGWith r spec)]
