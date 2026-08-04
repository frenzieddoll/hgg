-- |
-- Module      : Graphics.Hgg.Frame
-- Description : DataFrame 抽象 (class PlotData) ─ 列名で図を書くための df 非依存橋
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- ggplot2 のように「データフレーム + 列名」 で図を書くための抽象。 Haskell に
-- 統一 df ライブラリが無い事情に対応し、 __df 型に依存しない__ typeclass
-- 'PlotData' で「列名 → 実ベクタ」 (= 既存の 'Resolver') を取り出す。
--
-- @
-- import           Graphics.Hgg.Easy   (scatter, layer)
-- import           Graphics.Hgg.Frame
-- import qualified Data.Map.Strict as M
--
-- df = M.fromList [(\"x\", inline [1,2,3]), (\"y\", inline [4,5,6])]
-- -- df |>> layer (scatter \"x\" \"y\")
-- @
--
-- 本 module はゼロ依存 instance (assoc-list / 'Data.Map.Map') のみを持つ。
-- Hackage @dataframe@ 等の外部 df 型の instance は各橋 package が所有する
-- (orphan 回避、 proposal spec-2 §3.1)。
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.Hgg.Frame
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

import           Graphics.Hgg.Spec     (ColData (..), Resolver, VisualSpec)
import           Graphics.Hgg.Validate (PlotDiagnostic (..), validatePlotWith)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Vector           as V

-- ===========================================================================
-- class PlotData (リッチ版: toResolver + columnNames + nrows)
-- ===========================================================================

-- | [日本語]: 任意の df 型を hgg の描画系に橋渡しする typeclass。
--   リッチ版 (proposal spec-2 §3.1 のユーザ決定): 'toResolver' だけでも描画は
--   できるが、 'columnNames' で __バインド時の列存在検証__ と将来の
--   pairs / auto-aes が、 'nrows' で __空 df 検出__ が可能になる。
--   [English]: Bridges an arbitrary dataframe type to the plotting system.
--   'toResolver' alone is enough to draw, but 'columnNames' additionally
--   enables __column-existence checking at bind time__ (and future
--   pairs / auto-aes), while 'nrows' enables __empty-dataframe detection__.
class PlotData df where
  -- | [日本語]: 列名 → 'ColData'。 既存の 'Resolver' 型をそのまま再利用する
  --   (= @saveSVG@ 等が第 2 引数に取るのと同じ「実質 df」)。
  --   [English]: Column name to 'ColData'. Reuses the existing 'Resolver' type
  --   as-is (the same "effective dataframe" that @saveSVG@ takes as its
  --   second argument).
  toResolver  :: df -> Resolver

  -- | [日本語]: 全列名。 列存在チェック・将来の auto-aes / pairs に使う。
  --   [English]: All column names. Used for column-existence checks and for
  --   future auto-aes / pairs support.
  columnNames :: df -> [Text]

  -- | [日本語]: 行数。 空 df (@nrows == 0@) 検出に使う。 列ごとに長さが違う
  --   場合は __最長の列の長さ__ を返す (= 描画が要求しうる最大 index)。
  --   [English]: Number of rows, used to detect an empty dataframe
  --   (@nrows == 0@). When columns differ in length, returns
  --   __the length of the longest column__ (the largest index that rendering
  --   may demand).
  nrows       :: df -> Int

-- | [日本語]: 'ColData' (数値列 / 文字列列) の要素数。
--   [English]: Element count of a 'ColData' (a numeric or textual column).
colLen :: ColData -> Int
colLen (NumData v) = V.length v
colLen (TxtData v) = V.length v

-- ===========================================================================
-- ゼロ依存 instance ─ df ライブラリ無しでも使える最小実装
-- ===========================================================================

-- | [日本語]: assoc-list (列名と列の対の並び)。 列名重複時は __先勝ち__
--   ('lookup' 準拠)。
--   [English]: An association list of column name / column pairs. On duplicate
--   column names __the first wins__ (following 'lookup').
instance PlotData [(Text, ColData)] where
  toResolver  pairs = \name -> lookup name pairs
  columnNames       = map fst
  nrows       pairs = maximum (0 : map (colLen . snd) pairs)

-- | [日本語]: 'Data.Map.Strict.Map' 版。 列名重複は Map が解決済 (一意)。
--   [English]: The 'Data.Map.Strict.Map' variant. Duplicate column names are
--   already resolved by the map itself (keys are unique).
instance PlotData (Map Text ColData) where
  toResolver  m = \name -> M.lookup name m
  columnNames   = M.keys
  nrows       m = maximum (0 : map colLen (M.elems m))

-- ===========================================================================
-- バインド境界 ─ df を spec に結びつけた純値 'BoundPlot'
-- ===========================================================================

-- | [日本語]: df バインド済の plot。 描画関数 (@saveSVGBound@ 等、 backend
--   package 側) が消費する束。 'bpDiagnostics' は '(|>>)' バインド時の検証結果を
--   __値として__ 運ぶ (proposal spec-2 §3.3)。 '(|>>)' 自身は例外を投げない
--   純関数なので、 @let p = df |>> spec@ を list に詰める・テストで比較するが
--   成り立つ (= 「plot は値」)。 Error severity の報告は描画関数が実行時に行う。
--   [English]: A plot with its dataframe already bound — the bundle consumed by
--   the rendering functions (@saveSVGBound@ and friends, which live in the
--   backend packages). 'bpDiagnostics' carries the validation result produced
--   at '(|>>)' bind time __as a value__. '(|>>)' itself is a pure function that
--   never throws, so @let p = df |>> spec@ can be put in a list or compared in
--   tests (that is, "a plot is a value"). Reporting of error-severity
--   diagnostics is left to the rendering function at run time.
data BoundPlot = BoundPlot
  { bpResolver    :: Resolver
  , bpSpec        :: VisualSpec
  , bpDiagnostics :: [PlotDiagnostic]
  }

-- | [日本語]: df を spec にバインド。 __純関数__ (例外を投げない)。
--   演算子が @|>@ でなく @|>>@ なのは、 Hackage @dataframe@ が @|>@ を public
--   export しており衝突するため。 @infixl 1@ で @<>@ (@infixr 6@) より弱く、
--   @df |>> (layer a <> layer b)@ をカッコ無しで書ける。
--   バインド時に 'validatePlotWith' で spec の列参照を df の 'columnNames' と
--   突合し、 結果を 'bpDiagnostics' に __値として__ 格納する (存在しない列 →
--   編集距離つき suggestion、 型不一致、 必須 aesthetic 欠落、 空 plot を検出)。
--   空 df (@nrows == 0@) は専用 error kind が無いため 'PlotInfo' で残す
--   (lenient)。 例外は一切投げない。 束の型は spec 型で選ぶ
--   (2D = 'BoundPlot'、 3D = @BoundPlot3D@。 3D instance は hgg-3d 側 =
--   型の定義 package なので非 orphan)。
--   [English]: Binds a dataframe to a spec. __Pure__ — it never throws. The
--   operator is @|>>@ rather than @|>@ because Hackage @dataframe@ exports
--   @|>@ publicly and the two would clash. At @infixl 1@ it binds more weakly
--   than @<>@ (@infixr 6@), so @df |>> (layer a <> layer b)@ needs no
--   parentheses. At bind time 'validatePlotWith' checks the spec's column
--   references against the dataframe's 'columnNames' and stores the result in
--   'bpDiagnostics' __as a value__ (missing columns with an edit-distance
--   suggestion, type mismatches, missing required aesthetics and empty plots
--   are all detected). An empty dataframe (@nrows == 0@) is reported as
--   'PlotInfo' because there is no dedicated error kind for it (lenient).
--   The bound type is selected by the spec type (2D gives 'BoundPlot', 3D
--   gives @BoundPlot3D@, whose instance lives in hgg-3d — the package
--   that defines the type, so it is not an orphan).
class BindableSpec spec where
  type BoundOf spec
  bindData :: PlotData df => df -> spec -> BoundOf spec

instance BindableSpec VisualSpec where
  type BoundOf VisualSpec = BoundPlot
  bindData df spec =
    BoundPlot r spec (emptyDfDiagnostics df ++ validatePlotWith (columnNames df) r spec)
   where
    r = toResolver df

-- | [日本語]: 空 df の共通診断 (2D/3D instance で共有)。
--   [English]: The shared diagnostic for an empty dataframe (used by both the
--   2D and 3D instances).
emptyDfDiagnostics :: PlotData df => df -> [PlotDiagnostic]
emptyDfDiagnostics df
  | nrows df == 0 = [PlotInfo "DataFrame が空です (nrows == 0)。 描画は空になります。"]
  | otherwise     = []

-- | [日本語]: df を spec にバインドする演算子 ('bindData' の別名)。
--   [English]: The operator form of 'bindData', binding a dataframe to a spec.
(|>>) :: (PlotData df, BindableSpec spec) => df -> spec -> BoundOf spec
(|>>) = bindData
infixl 1 |>>

-- | [日本語]: 検証を逃がす raw 経路。 'BoundPlot' から @(Resolver, VisualSpec)@ を
--   取り出し、 既存の @saveSVGWith@ / @renderSVGWith@ に直接渡せる。
--   [English]: The raw escape hatch that bypasses validation. Extracts
--   @(Resolver, VisualSpec)@ from a 'BoundPlot' so that it can be passed
--   straight to the existing @saveSVGWith@ / @renderSVGWith@.
unBound :: BoundPlot -> (Resolver, VisualSpec)
unBound (BoundPlot r s _) = (r, s)
