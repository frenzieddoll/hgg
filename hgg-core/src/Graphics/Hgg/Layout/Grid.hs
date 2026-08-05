-- |
-- Module      : Graphics.Hgg.Layout.Grid
-- Description : Flattens nested subplots / <-> / <:> into a single unified grid
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: subplots / @<->@ / @<:>@ のネストを __単一の統一グリッド__へ
--   平坦化する純関数。 patchwork 流 gtable 配置の前段で、 任意の深さの
--   入れ子を各 leaf パネルに @(rowStart, rowSpan, colStart, colSpan)@ を割り当てた
--   フラットなグリッドへ落とす。 描画 (Render/Layer) はこのグリッド 1 枚に対して
--   「列ごと左右帯・行ごと上下帯」 を 1 回確保するだけになり、 ネスト境界をまたいだ
--   パネル本体の整列が保証される。
--
--   ★方針 (計画書 §設計): ツリーの寸法を整数グリッド単位で再帰計算する。
--     * leaf            : @w=1, h=1@
--     * hbox (横並び)   : @w=Σ child.w@, @h=max child.h@。 各 child は自分の幅 ×
--                         グループ高 (行) を span (縦を揃える)。
--     * vbox (縦並び)   : @h=Σ child.h@, @w=max child.w@。 各 child はグループ幅 (列) ×
--                         自分の高さを span (横を揃える)。
--   leaf を span 方向 (hbox なら縦・vbox なら横) いっぱいに伸ばすことで、
--   @(a<->b<->c)<:>d@ の @d@ が上段 3 列を colSpan=3 で全幅 span し、 上段左端と
--   下段左端が col0 で一致する。
-- [English]: A pure function that flattens nested subplots / @<->@ / @<:>@
--   into __a single unified grid__. As a step before patchwork-style gtable
--   placement, it reduces arbitrarily deep nesting into a flat grid, giving
--   each leaf panel a @(rowStart, rowSpan, colStart, colSpan)@. Rendering
--   (Render/Layer) then only needs to reserve "left/right bands per column,
--   top/bottom bands per row" once for this single grid, which guarantees
--   panel-body alignment across nesting boundaries.
--
--   Approach (see the design section of the plan): recursively computes
--   tree dimensions in integer grid units.
--     * leaf: @w=1, h=1@.
--     * hbox (side-by-side): @w=Σ child.w@, @h=max child.h@. Each child spans
--       its own width × the group height (row), keeping rows aligned.
--     * vbox (stacked): @h=Σ child.h@, @w=max child.w@. Each child spans the
--       group width (column) × its own height, keeping columns aligned.
--   Stretching a leaf to fill the span direction (vertical for hbox,
--   horizontal for vbox) means @d@ in @(a<->b<->c)<:>d@ spans the top row's
--   3 columns at colSpan=3, so the top row's left edge lines up with the
--   bottom row's left edge at col0.
{-# LANGUAGE BangPatterns #-}

module Graphics.Hgg.Layout.Grid
  ( GridCell(..)
  , GridPlacement(..)
  , PTree(..)
  , toPTree
  , gridDims
  , flattenSubplots
  ) where

import           Graphics.Hgg.Spec (VisualSpec, selectedSubplots, vsSubplotCols)
import           Data.List         (mapAccumL)
import           Data.Monoid       (Last (..))

-- ===========================================================================
-- 型
-- ===========================================================================

-- | [日本語]: 統一グリッド上の 1 パネルの占有矩形 (整数セル単位)。
--   [English]: The rectangle a single panel occupies on the unified grid (in
--   integer cell units).
data GridCell = GridCell
  { gcRow     :: !Int  -- ^ [日本語]: 開始行 (0 始まり)。 [English]: The starting row (0-based).
  , gcRowSpan :: !Int  -- ^ [日本語]: またぐ行数 (>= 1)。 [English]: The number of rows spanned (>= 1).
  , gcCol     :: !Int  -- ^ [日本語]: 開始列 (0 始まり)。 [English]: The starting column (0-based).
  , gcColSpan :: !Int  -- ^ [日本語]: またぐ列数 (>= 1)。 [English]: The number of columns spanned (>= 1).
  } deriving (Eq, Show)

-- | [日本語]: 平坦化結果。 グリッド総寸法 + leaf パネルとその占有セル。
--   [English]: The flattening result: overall grid dimensions plus each leaf
--   panel and its occupied cell.
data GridPlacement = GridPlacement
  { gpCols   :: !Int                        -- ^ [日本語]: 統一グリッドの総列数。 [English]: The total column count of the unified grid.
  , gpRows   :: !Int                        -- ^ [日本語]: 統一グリッドの総行数。 [English]: The total row count of the unified grid.
  , gpPanels :: ![(VisualSpec, GridCell)]   -- ^ [日本語]: leaf パネル (描画対象) とセル。 [English]: The leaf panels (render targets) with their cells.
  }

-- | [日本語]: subplots ツリーの中間表現。 @<->@ は 'PH'、 @<:>@ は 'PV'、 単一
--   プロットは 'PLeaf'。 汎用 subplots (cols が 1 でも要素数でもない wrap grid)
--   は @PV [PH ...]@ へ正規化する。
--   [English]: An intermediate representation of the subplots tree. @<->@ is
--   'PH', @<:>@ is 'PV', and a single plot is 'PLeaf'. A general subplots
--   layout (a wrap grid whose cols is neither 1 nor the element count) is
--   normalized to @PV [PH ...]@.
data PTree
  = PLeaf VisualSpec
  | PH    [PTree]   -- ^ [日本語]: 横並び (hconcat / @<->@)。 [English]: Side-by-side (hconcat / @<->@).
  | PV    [PTree]   -- ^ [日本語]: 縦並び (vconcat / @<:>@)。 [English]: Stacked (vconcat / @<:>@).

-- ===========================================================================
-- VisualSpec → PTree
-- ===========================================================================

-- | [日本語]: subplots ネストを 'PTree' へ。 cols でグループ方向を判定:
--   @cols<=1@ → 縦・@cols>=n@ → 横・それ以外 → cols 列の wrap grid (=縦に並ぶ横行)。
--   既定 cols は @renderSubplots@ と同じ @min n 3@ (parity 維持)。
--   [English]: Converts subplots nesting to a 'PTree'. cols determines the
--   grouping direction: @cols<=1@ means stacked; @cols>=n@ means
--   side-by-side; otherwise it is a wrap grid of cols columns (rows of
--   side-by-side panels, stacked). The default cols matches
--   @renderSubplots@, @min n 3@ (preserving parity).
toPTree :: VisualSpec -> PTree
toPTree s =
  case selectedSubplots s of
    []   -> PLeaf s
    subs ->
      let n    = length subs
          cols = maybe (min n 3) id (getLast (vsSubplotCols s))
          kids = map toPTree subs
      in if cols <= 1   then PV kids
         else if cols >= n then PH kids
         else PV [ PH chunk | chunk <- chunksOf cols kids ]

-- | [日本語]: リストを長さ n ずつに分割 (最後は端数)。
--   [English]: Splits a list into chunks of length n (the last chunk may be
--   shorter).
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = [xs]
  | null xs   = []
  | otherwise = let (a, b) = splitAt n xs in a : chunksOf n b

-- ===========================================================================
-- 寸法 (整数グリッド単位) と配置
-- ===========================================================================

-- | [日本語]: サブツリーのグリッド寸法 @(cols, rows)@。
--   [English]: The grid dimensions of a subtree, @(cols, rows)@.
gridDims :: PTree -> (Int, Int)
gridDims (PLeaf _) = (1, 1)
gridDims (PH ts)   = ( sum     (map (fst . gridDims) ts)
                     , maximum (1 : map (snd . gridDims) ts) )
gridDims (PV ts)   = ( maximum (1 : map (fst . gridDims) ts)
                     , sum     (map (snd . gridDims) ts) )

-- | [日本語]: @placeT availRows availCols row0 col0 tree@:
--   左上 @(row0,col0)@ から @availRows × availCols@ の領域にツリーを配置し、
--   leaf パネルとその占有セルを返す。 leaf は与えられた領域いっぱいを span する。
--   [English]: @placeT availRows availCols row0 col0 tree@: places the tree
--   in the @availRows × availCols@ region starting at the top-left
--   @(row0,col0)@, and returns the leaf panels with their occupied cells.
--   Each leaf spans the entire region given to it.
placeT :: Int -> Int -> Int -> Int -> PTree -> [(VisualSpec, GridCell)]
placeT !ar !ac !r0 !c0 (PLeaf s) =
  [(s, GridCell r0 ar c0 ac)]
placeT !ar _   !r0 !c0 (PH ts) =
  -- [日本語]: 各 child は自分の幅 × グループ高 (= ar 行) を span。 列を順に消費。
  -- [English]: Each child spans its own width × the group height (ar rows); columns are consumed in order.
  concat . snd $ mapAccumL
    (\cAcc t -> let w = fst (gridDims t)
                in (cAcc + w, placeT ar w r0 cAcc t))
    c0 ts
placeT _   !ac !r0 !c0 (PV ts) =
  -- [日本語]: 各 child はグループ幅 (= ac 列) × 自分の高さ を span。 行を順に消費。
  -- [English]: Each child spans the group width (ac columns) × its own height; rows are consumed in order.
  concat . snd $ mapAccumL
    (\rAcc t -> let h = snd (gridDims t)
                in (rAcc + h, placeT h ac rAcc c0 t))
    r0 ts

-- | [日本語]: VisualSpec の subplots ツリーを統一グリッドへ平坦化する。
--   [English]: Flattens a VisualSpec's subplots tree into a unified grid.
flattenSubplots :: VisualSpec -> GridPlacement
flattenSubplots s =
  let t            = toPTree s
      (cols, rows) = gridDims t
  in GridPlacement cols rows (placeT rows cols 0 0 t)
