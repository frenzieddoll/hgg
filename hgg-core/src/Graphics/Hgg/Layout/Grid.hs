-- |
-- Module      : Graphics.Hgg.Layout.Grid
-- Description : subplots / <-> / <:> のネストを単一統一グリッドへ平坦化 (Phase 37 A2)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 37 A2: subplots / @<->@ / @<:>@ のネストを **単一の統一グリッド**へ
--   平坦化する純関数。 patchwork 流 gtable 配置 (A3) の前段で、 任意の深さの
--   入れ子を各 leaf パネルに @(rowStart, rowSpan, colStart, colSpan)@ を割り当てた
--   フラットなグリッドへ落とす。 描画 (Render/Layer) はこのグリッド 1 枚に対して
--   「列ごと左右帯・行ごと上下帯」 を 1 回確保するだけになり、 ネスト境界をまたいだ
--   パネル本体の整列が保証される。
--
-- ★方針 (計画書 §設計): ツリーの寸法を整数グリッド単位で再帰計算する。
--   * leaf            : @w=1, h=1@
--   * hbox (横並び)   : @w=Σ child.w@, @h=max child.h@。 各 child は自分の幅 ×
--                       グループ高 (行) を span (縦を揃える)。
--   * vbox (縦並び)   : @h=Σ child.h@, @w=max child.w@。 各 child はグループ幅 (列) ×
--                       自分の高さを span (横を揃える)。
--   leaf を span 方向 (hbox なら縦・vbox なら横) いっぱいに伸ばすことで、
--   @(a<->b<->c)<:>d@ の @d@ が上段 3 列を colSpan=3 で全幅 span し、 上段左端と
--   下段左端が col0 で一致する。
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

-- | 統一グリッド上の 1 パネルの占有矩形 (整数セル単位)。
data GridCell = GridCell
  { gcRow     :: !Int  -- ^ 開始行 (0 始まり)
  , gcRowSpan :: !Int  -- ^ またぐ行数 (>= 1)
  , gcCol     :: !Int  -- ^ 開始列 (0 始まり)
  , gcColSpan :: !Int  -- ^ またぐ列数 (>= 1)
  } deriving (Eq, Show)

-- | 平坦化結果。 グリッド総寸法 + leaf パネルとその占有セル。
data GridPlacement = GridPlacement
  { gpCols   :: !Int                        -- ^ 統一グリッドの総列数
  , gpRows   :: !Int                        -- ^ 統一グリッドの総行数
  , gpPanels :: ![(VisualSpec, GridCell)]   -- ^ leaf パネル (描画対象) とセル
  }

-- | subplots ツリーの中間表現。 @<->@ は 'PH'、 @<:>@ は 'PV'、 単一プロットは 'PLeaf'。
--   汎用 subplots (cols が 1 でも要素数でもない wrap grid) は @PV [PH ...]@ へ正規化する。
data PTree
  = PLeaf VisualSpec
  | PH    [PTree]   -- ^ 横並び (hconcat / @<->@)
  | PV    [PTree]   -- ^ 縦並び (vconcat / @<:>@)

-- ===========================================================================
-- VisualSpec → PTree
-- ===========================================================================

-- | subplots ネストを 'PTree' へ。 cols でグループ方向を判定:
--   @cols<=1@ → 縦・@cols>=n@ → 横・それ以外 → cols 列の wrap grid (=縦に並ぶ横行)。
--   既定 cols は 'renderSubplots' と同じ @min n 3@ (parity 維持)。
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

-- | リストを長さ n ずつに分割 (最後は端数)。
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = [xs]
  | null xs   = []
  | otherwise = let (a, b) = splitAt n xs in a : chunksOf n b

-- ===========================================================================
-- 寸法 (整数グリッド単位) と配置
-- ===========================================================================

-- | サブツリーのグリッド寸法 @(cols, rows)@。
gridDims :: PTree -> (Int, Int)
gridDims (PLeaf _) = (1, 1)
gridDims (PH ts)   = ( sum     (map (fst . gridDims) ts)
                     , maximum (1 : map (snd . gridDims) ts) )
gridDims (PV ts)   = ( maximum (1 : map (fst . gridDims) ts)
                     , sum     (map (snd . gridDims) ts) )

-- | @placeT availRows availCols row0 col0 tree@:
--   左上 @(row0,col0)@ から @availRows × availCols@ の領域にツリーを配置し、
--   leaf パネルとその占有セルを返す。 leaf は与えられた領域いっぱいを span する。
placeT :: Int -> Int -> Int -> Int -> PTree -> [(VisualSpec, GridCell)]
placeT !ar !ac !r0 !c0 (PLeaf s) =
  [(s, GridCell r0 ar c0 ac)]
placeT !ar _   !r0 !c0 (PH ts) =
  -- 各 child は自分の幅 × グループ高 (= ar 行) を span。 列を順に消費。
  concat . snd $ mapAccumL
    (\cAcc t -> let w = fst (gridDims t)
                in (cAcc + w, placeT ar w r0 cAcc t))
    c0 ts
placeT _   !ac !r0 !c0 (PV ts) =
  -- 各 child はグループ幅 (= ac 列) × 自分の高さ を span。 行を順に消費。
  concat . snd $ mapAccumL
    (\rAcc t -> let h = snd (gridDims t)
                in (rAcc + h, placeT h ac rAcc c0 t))
    r0 ts

-- | VisualSpec の subplots ツリーを統一グリッドへ平坦化する。
flattenSubplots :: VisualSpec -> GridPlacement
flattenSubplots s =
  let t            = toPTree s
      (cols, rows) = gridDims t
  in GridPlacement cols rows (placeT rows cols 0 0 t)
