-- |
-- Module      : Hgg.Plot.Frame
-- Description : DataFrame 抽象 (class PlotData) ─ 列名で図を書くための df 非依存橋
-- Copyright   : (c) 2026 Hgg
-- License     : BSD-3-Clause
--
-- ggplot2 のように「データフレーム + 列名」 で図を書くための抽象。 Haskell に
-- 統一 df ライブラリが無い事情に対応し、 **df 型に依存しない** typeclass
-- 'PlotData' で「列名 → 実ベクタ」 (= 既存の 'Resolver') を取り出す。
--
-- @
-- import           Hgg.Plot.Easy   (scatter, layer)
-- import           Hgg.Plot.Frame
-- import qualified Data.Map.Strict as M
--
-- df = M.fromList [(\"x\", inline [1,2,3]), (\"y\", inline [4,5,6])]
-- -- df |>> layer (scatter \"x\" \"y\")   -- バインドは A3 ((|>>)) で
-- @
--
-- 本 module はゼロ依存 instance (assoc-list / 'Data.Map.Map') のみを持つ。
-- Hackage @dataframe@ 等の外部 df 型の instance は各橋 package が所有する
-- (orphan 回避、 proposal spec-2 §3.1)。
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Hgg.Plot.Frame
  ( -- * df 抽象
    PlotData (..)
    -- * バインド (df → spec)
  , BoundPlot (..)
  , (|>>)
  , BindableSpec (..)
  , emptyDfDiagnostics
  , unBound
    -- * 補助
  , colLen
  ) where

import           Hgg.Plot.Spec     (ColData (..), Resolver, VisualSpec)
import           Hgg.Plot.Validate (PlotDiagnostic (..), validatePlotWith)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Vector           as V

-- ===========================================================================
-- class PlotData (リッチ版: toResolver + columnNames + nrows)
-- ===========================================================================

-- | 任意の df 型を hgg の描画系に橋渡しする typeclass。
--
-- リッチ版 (proposal spec-2 §3.1 のユーザ決定): 'toResolver' だけでも描画は
-- できるが、 'columnNames' で **バインド時の列存在検証** (Phase 14 A5) と将来の
-- pairs / auto-aes が、 'nrows' で **空 df 検出**が可能になる。
class PlotData df where
  -- | 列名 → 'ColData'。 既存の 'Resolver' 型をそのまま再利用する
  --   (= @saveSVG@ 等が第 2 引数に取るのと同じ「実質 df」)。
  toResolver  :: df -> Resolver

  -- | 全列名。 列存在チェック・将来の auto-aes / pairs に使う。
  columnNames :: df -> [Text]

  -- | 行数。 空 df (@nrows == 0@) 検出に使う。 列ごとに長さが違う場合は
  --   **最長の列の長さ**を返す (= 描画が要求しうる最大 index)。
  nrows       :: df -> Int

-- | 'ColData' (数値列 / 文字列列) の要素数。
colLen :: ColData -> Int
colLen (NumData v) = V.length v
colLen (TxtData v) = V.length v

-- ===========================================================================
-- ゼロ依存 instance ─ df ライブラリ無しでも使える最小実装
-- ===========================================================================

-- | assoc-list (列名と列の対の並び)。 列名重複時は **先勝ち** ('lookup' 準拠)。
instance PlotData [(Text, ColData)] where
  toResolver  pairs = \name -> lookup name pairs
  columnNames       = map fst
  nrows       pairs = maximum (0 : map (colLen . snd) pairs)

-- | 'Data.Map.Strict.Map' 版。 列名重複は Map が解決済 (一意)。
instance PlotData (Map Text ColData) where
  toResolver  m = \name -> M.lookup name m
  columnNames   = M.keys
  nrows       m = maximum (0 : map colLen (M.elems m))

-- ===========================================================================
-- バインド境界 ─ df を spec に結びつけた純値 'BoundPlot'
-- ===========================================================================

-- | df バインド済の plot。 描画関数 (saveSVGBound 等、 backend package 側) が
-- 消費する束。
--
-- 'bpDiagnostics' は '(|>>)' バインド時の検証結果を **値として**運ぶ
-- (proposal spec-2 §3.3 / Phase 14 検証案1)。 '(|>>)' 自身は例外を投げない
-- 純関数なので、 @let p = df |>> spec@ を list に詰める・テストで比較するが
-- 成り立つ (= 「plot は値」)。 Error severity の報告は描画関数が実行時に行う。
data BoundPlot = BoundPlot
  { bpResolver    :: Resolver
  , bpSpec        :: VisualSpec
  , bpDiagnostics :: [PlotDiagnostic]
  }

-- | df を spec にバインド。 **純関数** (例外を投げない)。
--
-- 演算子が @|>@ でなく @|>>@ なのは、 Hackage @dataframe@ が @|>@ を public
-- export しており衝突するため (proposal spec-2 / Phase 14 計画書 §設計判断)。
-- 'infixl' 1 で @<>@ (infixr 6) より弱く、 @df |>> (layer a <> layer b)@ を
-- カッコ無しで書ける。
--
-- 検証 (A5): バインド時に 'validatePlotWith' で spec の 'ColByName' を df の
-- 'columnNames' と突合し、 結果を 'bpDiagnostics' に **値として**格納する
-- (存在しない列 → 'ColumnNotFound' + 編集距離 suggestion、 型不一致 →
-- 'ColumnTypeMismatch'、 必須 aesthetic 欠落、 空 plot を検出)。 空 df
-- (@nrows == 0@) は専用 error kind が無いため 'PlotInfo' で残す (lenient)。
-- 例外は一切投げない。 Error の報告は描画関数 ('saveSVGBound') が実行時に行う。
-- Phase 24 A6: spec 型で束の型を選ぶ (2D = 'BoundPlot'、 3D = BoundPlot3D)。
-- 2D の意味論は従来と同一 (instance が旧実装そのもの)。 3D instance は
-- hgg-3d 側 (型の定義 package = 非 orphan)。
class BindableSpec spec where
  type BoundOf spec
  bindData :: PlotData df => df -> spec -> BoundOf spec

instance BindableSpec VisualSpec where
  type BoundOf VisualSpec = BoundPlot
  bindData df spec =
    BoundPlot r spec (emptyDfDiagnostics df ++ validatePlotWith (columnNames df) r spec)
   where
    r = toResolver df

-- | 空 df の共通診断 (2D/3D instance で共有)。
emptyDfDiagnostics :: PlotData df => df -> [PlotDiagnostic]
emptyDfDiagnostics df
  | nrows df == 0 = [PlotInfo "DataFrame が空です (nrows == 0)。 描画は空になります。"]
  | otherwise     = []

(|>>) :: (PlotData df, BindableSpec spec) => df -> spec -> BoundOf spec
(|>>) = bindData
infixl 1 |>>

-- | 検証を逃がす raw 経路。 'BoundPlot' から (Resolver, VisualSpec) を取り出し、
-- 既存の @saveSVGWith@ / @renderSVGWith@ に直接渡せる。
unBound :: BoundPlot -> (Resolver, VisualSpec)
unBound (BoundPlot r s _) = (r, s)
