-- |
-- Module      : Graphics.Hgg.DataFrame
-- Description : DataFrame ↔ hgg Resolver bridge (Phase 26 §A 拡張)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- @
-- import qualified DataFrame.IO.CSV            as DF
-- import           Graphics.Hgg.Easy
-- import           Graphics.Hgg.Backend.SVG    (saveSVGWith)
-- import           Graphics.Hgg.DataFrame      (dfResolver, plotDF)
--
-- main = do
--   df <- DF.readCsv "data.csv"
--   plotDF "out.svg" df $
--     purePlot
--       <> layer (scatter "weight" "mpg" <> colorBy "origin")
--       <> title  "燃費 vs 重量"
-- @
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | 'PlotData' instance for Hackage @dataframe@ は orphan (型もクラスも当 package
-- が所有しないが、 spec-2 §3.1/§6 の設計で **橋 package が所有**する正当な配置)。
{-# OPTIONS_GHC -Wno-orphans #-}
module Graphics.Hgg.DataFrame
  ( dfResolver
  , plotDF
    -- * Phase 14: PlotData instance (df |>> spec を使えるように)
    -- $plotdata
  ) where

import           Graphics.Hgg.Backend.SVG (saveSVGWith)
import           Graphics.Hgg.Frame       (PlotData (..))
import           Graphics.Hgg.Spec        (ColData (..), Resolver, VisualSpec)
import           Control.Applicative      ((<|>))
import           Data.Text                (Text)
import qualified Data.Vector              as V
import qualified DataFrame.Internal.Column as DFC
import qualified DataFrame.Internal.DataFrame as DFI
import qualified DataFrame.Operations.Core as DF
import qualified DataFrame.Operators      as DF

-- | 'DataFrame' から hgg の 'Resolver' を作る。
-- 列名で `Double` または `Text` の列を抽出、 数値列なら 'NumData' / 文字列列
-- なら 'TxtData' を返す。 失敗 (= 列不在 / 型不一致) は 'Nothing'。
--
-- 注意: DF の `columnAsList` は 例外ベースなので 'unsafePerformIO + try' で
-- 純関数化している (= hanalyze の Convert.hs と同じパターン)。
dfResolver :: DFI.DataFrame -> Resolver
dfResolver df name =
   -- nullable (Maybe) 列対応: 欠損 (NA) は NaN で運び **長さを保つ** (行整列を壊さない・
   -- ggplot が na.rm で列の欠損を内部処理するのと同方針)。 消費側 (range / binning /
   -- 点描画) が NaN を弾く。 ★Maybe 版を plain より先に試す: DF.fromList で作った
   -- Maybe Int 列は plain @Int が成功して NA→0 と読んでしまう (誤り) ため、 Maybe を
   -- 優先して NA→NaN を保証する。 非 NULL 列は Maybe 版でも全要素 Just ゆえ同値 (無害)。
       fmap NumData (tryMaybeDoubleCol name df)
   <|> fmap NumData (tryMaybeIntCol    name df)
   <|> fmap NumData (tryDoubleCol      name df)
   <|> fmap NumData (tryIntCol         name df)
   <|> fmap TxtData (tryTextCol        name df)

tryDoubleCol :: Text -> DFI.DataFrame -> Maybe (V.Vector Double)
tryDoubleCol n df = safeColumnAs @Double n df

tryIntCol :: Text -> DFI.DataFrame -> Maybe (V.Vector Double)
tryIntCol n df =
  fmap (V.map fromIntegral) (safeColumnAs @Int n df)

-- | @Maybe Double@ 列: NA → NaN。
tryMaybeDoubleCol :: Text -> DFI.DataFrame -> Maybe (V.Vector Double)
tryMaybeDoubleCol n df =
  fmap (V.map (maybe (0/0) id)) (safeColumnAs @(Maybe Double) n df)

-- | @Maybe Int@ 列: NA → NaN。
tryMaybeIntCol :: Text -> DFI.DataFrame -> Maybe (V.Vector Double)
tryMaybeIntCol n df =
  fmap (V.map (maybe (0/0) fromIntegral)) (safeColumnAs @(Maybe Int) n df)

tryTextCol :: Text -> DFI.DataFrame -> Maybe (V.Vector Text)
tryTextCol n df = safeColumnAs @Text n df

-- | DF.columnAsList を例外セーフに呼び出して Vector に。
safeColumnAs
  :: forall a. (DFC.Columnable a)
  => Text -> DFI.DataFrame -> Maybe (V.Vector a)
safeColumnAs name df = either (const Nothing) Just (DF.columnAsVector (DF.col @a name) df)

-- | DF + spec を 1 行で SVG 出力 (= matplotlib `plt.savefig` 感)。
plotDF :: FilePath -> DFI.DataFrame -> VisualSpec -> IO ()
plotDF path df spec = saveSVGWith path (dfResolver df) spec

-- $plotdata
-- Phase 14: Hackage @dataframe@ の 'DFI.DataFrame' を 'PlotData' instance に
-- することで、 @df |>> layer (scatter "x" "y")@ (df-first バインド) が使える。
-- @toResolver@ は既存 'dfResolver' を再利用、 @columnNames@/@nrows@ は dataframe
-- の API (@DFI.columnNames@ / @DFI.dataframeDimensions@) で実装する。

-- | df-first バインド ('(|>>)') / 列名検証のための instance。
--
-- * @columnNames@ = @DataFrame.columnNames@ (= 全列名)
-- * @nrows@       = @dataframeDimensions@ の第 1 要素 (= 行数。 実測: @(rows, cols)@)
instance PlotData DFI.DataFrame where
  toResolver  = dfResolver
  columnNames = DFI.columnNames
  nrows       = fst . DFI.dataframeDimensions
