-- | 03-encoding-scale.md の図。channel (列→視覚属性の写像) を 1 枚に集約した
--   カタログ図を所有する。scale / palette / 軸 (position scale) のデモ図は
--   共有束縛の都合で当面 DocFig.Decoration が emit する (orphan ゲートは全 *.hs を
--   横断するため所在は問わない)。
{-# LANGUAGE OverloadedStrings #-}
module DocFig.EncodingScale (figures) where

import           Data.Text     (Text)
import           DocFig.Common

figures :: [Figure]
figures =
  -- §1 channel カタログ: 同じ散布を 4 つの channel で写し分ける
  -- (colorBy / sizeBy / shapeBy / linetypeBy)。mark カタログ (02-layers) は
  -- mark ごとの実例を持つので、ここは「列→視覚属性」の対比に専念する。
  [ figW "encoding-channels.svg" 960 760 $
         subplots
           [ layer (scatter xs ys <> colorBy g  <> size 6)  <> title "colorBy (カテゴリ→色)"
           , layer (scatter xs ys <> sizeBy sz)             <> title "sizeBy (数値→点サイズ)"
           , layer (scatter xs ys <> shapeBy g <> size 6)   <> title "shapeBy (カテゴリ→形)"
           , layer (line lx ly <> linetypeBy lg <> stroke 2) <> title "linetypeBy (カテゴリ→線種)" ]
      <> subplotCols 2
      <> legend
      <> title "channel ─ 列を視覚属性へ写像する"
  ]
  where
    xs = inline    [1,2,3,4, 1,2,3,4]
    ys = inline    [2,3,1,4, 3,1,4,2]
    g  = inlineCat (replicate 4 "a" ++ replicate 4 "b" :: [Text])
    sz = inline    [1,2,3,4, 4,3,2,1]

    lx = inline    [1,2,3,4,5, 1,2,3,4,5]
    ly = inline    [1,2,3,4,5, 2,3,3,4,5]
    lg = inlineCat (replicate 5 "p" ++ replicate 5 "q" :: [Text])
