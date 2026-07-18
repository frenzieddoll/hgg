-- |
-- Module      : Hgg.Plot.Spec.Concat
-- Description : 図の合成 (hconcat / vconcat / <-> / <:> + pairs、 patchwork 風)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- Phase 55: 'Hgg.Plot.Spec' の module 分割で切り出し。 複数 'VisualSpec' を
-- 1 枚に並べる合成 (Vega-Lite hconcat/vconcat 相当・patchwork 風演算子) と
-- 'pairs' (散布図行列) を持つ。 subplots + subplotCols の純粋な薄ラッパで、
-- レンダリングは既存 subplots 経路を使う。 公開 API は従来どおり
-- 'Hgg.Plot.Spec' (facade) が re-export する。 挙動・出力は完全に不変。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Spec.Concat
  ( hconcat
  , vconcat
  , (<->)
  , (<:>)
  , asHGroup
  , asVGroup
  , pairs
  ) where

import           Data.Monoid     (Last (..))

import           Hgg.Plot.Spec.Axis (hideTicks)
import           Hgg.Plot.Spec.Column (ColRef, colRefName)
import           Hgg.Plot.Spec.Constructors (densityNorm, scatter)
import           Hgg.Plot.Spec.Layer (alpha, size)
import           Hgg.Plot.Spec.Setters
import           Hgg.Plot.Spec.Visual

-- ===========================================================================
-- concat 合成 (Vega-Lite hconcat/vconcat 相当・patchwork 風演算子)
-- ===========================================================================
-- subplots + subplotCols の純粋な薄ラッパ。 レンダリングは既存 subplots 経路を
-- そのまま使う (render/parity への影響ゼロ)。
--
-- 演算子 '<->' (横) / '<:>' (縦) は同方向チェーンを **平坦化** する:
--   (a <-> b <-> c) は subplots [a,b,c] (3 等分列) になり、 二項ネスト
--   (= 2 列で左セルが a,b に分割) にはならない。 平坦化は「左辺が cols==列数の
--   水平グループなら末尾追加、 そうでなければ新規 2 要素」 で行う。 leaf プロットは
--   vsSubplots=[] ゆえ必ず新規開始 = 通常チャートを誤って取り込まない。
-- 例: @(a <-> b <-> c) <:> d@ = 1 行目 3 列 + 2 行目を全幅 (1 行目セルの 3 倍幅)。
--     これは @vconcat [hconcat [a,b,c], d]@ と同値。
--
-- ★演算子の選定: '<->'(横)・'<:>'(縦) は Prelude/標準ライブラリと衝突しない
--   (旧案 '<|>' は Control.Applicative の Alternative と衝突したため回避した)。

-- | 横並び (= Vega-Lite hconcat): n 要素を 1 行 n 列に。
hconcat :: [VisualSpec] -> VisualSpec
hconcat ss = subplots ss <> subplotCols (length ss)

-- | 縦並び (= Vega-Lite vconcat): n 要素を n 行 1 列に。
vconcat :: [VisualSpec] -> VisualSpec
vconcat ss = subplots ss <> subplotCols 1

-- ★ fixity: '<->' (横) と '<:>' (縦) を同 precedence infixl 6 に揃える (Phase 59)。
--   '<>' (Semigroup、 infixr 6) と同 precedence・異 associativity なので、
--   @a <:> b <> opt@ / @a <-> b <> opt@ は無括弧だと**コンパイルエラー**
--   (Haskell 2010 §10.6 = 同 precedence の infixl/infixr 混在は構文エラー) になり、
--   @(a <:> b) <> opt@ と括弧を強制できる = 全体オプションが右パネルだけに付く
--   サイレント誤りを防ぐ。 旧 '<:>'=infixl 5 は '<>' より緩く @a <:> (b <> opt)@ と
--   黙って結合してしまう footgun だった (横 '<->' は元から infixl 6 でエラー = 安全側。
--   縦だけ非対称に危険だったのを解消)。
infixl 6 <->
infixl 6 <:>

-- | 横結合演算子 (= hconcat の二項・同方向チェーンを平坦化)。
(<->) :: VisualSpec -> VisualSpec -> VisualSpec
a <-> b = case asHGroup a of
  Just xs -> hconcat (xs ++ [b])
  Nothing -> hconcat [a, b]

-- | 縦結合演算子 (= vconcat の二項・同方向チェーンを平坦化)。
(<:>) :: VisualSpec -> VisualSpec -> VisualSpec
a <:> b = case asVGroup a of
  Just xs -> vconcat (xs ++ [b])
  Nothing -> vconcat [a, b]

-- | spec が「純粋な水平グループ (subplots=xs (>1 要素)・cols==要素数)」 なら xs。
asHGroup :: VisualSpec -> Maybe [VisualSpec]
asHGroup s = case getLast (vsSubplotCols s) of
  Just c | let xs = vsSubplots s, length xs > 1, c == length xs -> Just (vsSubplots s)
  _ -> Nothing

-- | spec が「純粋な垂直グループ (subplots=xs (>1 要素)・cols==1)」 なら xs。
asVGroup :: VisualSpec -> Maybe [VisualSpec]
asVGroup s = case getLast (vsSubplotCols s) of
  Just 1 | length (vsSubplots s) > 1 -> Just (vsSubplots s)
  _ -> Nothing

-- | P18: pairs plot (= N 列の posterior 等を N×N grid で対角は density、
-- |   非対角は scatter)。
pairs :: [ColRef] -> VisualSpec
pairs cols =
  let n = length cols
      -- Phase 7 A6: 内側パネルの軸目盛りを抑制 (= seaborn/ggpairs 流)。
      --   x tick は最下段 (i == n-1) のみ、 y tick は左端列 (j == 0) のみ表示。
      --   対角 (i == j) は density で y = count スケールのため、 左端の (0,0) も y は抑制。
      -- ※ axShowTicks (Bool) は HS が真の JSON boolean で出力する。 PS Codec の
      --   decodeBoolean を boolean 両対応にして HS→PS decode を通るようにした。
      mkPanel i j =
        let showXAxis = i == n - 1
            -- 左端列 (j==0) は対角も含め y 軸目盛りを表示 (= その行の変数値スケール)。
            showYAxis = j == 0
            axisCfg = (if showXAxis then mempty else xAxis hideTicks)
                   <> (if showYAxis then mempty else yAxis hideTicks)
            -- seaborn/ggpairs 流: 軸ラベル (変数名) は最下段 x・左端 y のみ。
            -- inline 列は名前を持たない (placeholder) ので、 その場合はラベルを付けない
            -- (= xLabel/yLabel を呼ばず mempty。 空ラベルの margin 予約も避けて詰める)。
            axName c = let nm = colRefName c
                       in if nm == "<inline-num>" || nm == "<inline-txt>" then "" else nm
            xLab c = if i == n - 1 && axName c /= "" then xLabel (axName c) else mempty
            yLab c = if j == 0     && axName c /= "" then yLabel (axName c) else mempty
            base
              -- 対角 (i==j): densityNorm (= y 軸 = 値範囲、 KDE は panel 高さに正規化、
              -- seaborn pairplot 対角)。 左端列なら y タイトルも。
              | i == j    = case cols !? i of
                  Just c  -> purePlot <> layer (densityNorm c) <> xLab c <> yLab c
                  Nothing -> purePlot
              | otherwise = case (cols !? j, cols !? i) of
                  (Just xc, Just yc) -> purePlot
                    <> layer (scatter xc yc <> alpha 0.3 <> size 2.5)
                    <> xLab xc <> yLab yc
                  _ -> purePlot
        in base <> axisCfg
      panels = [ mkPanel i j | i <- [0..n-1], j <- [0..n-1] ]
  in subplots panels <> subplotCols n <> title "Pairs plot"
  where
    (!?) :: [a] -> Int -> Maybe a
    xs !? i = if i < 0 || i >= length xs then Nothing else Just (xs !! i)

