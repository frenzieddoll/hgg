-- | Phase 51: custom mark (拡張可能な描画語彙) の最小作例。
--
-- core (@MarkKind@ の閉列挙) を **一切触らず**、@customMark id drawFn@ + @encX@/@encY@ で
-- 新しいプロット型 (ここでは簡易 dendrogram) を定義し、 @dendrogram "leaf" "height"@ と
-- 組み込み mark (@scatter x y@) と同じ書き味で使えるようにする。 draw 関数は @"leaf"@/@"height"@
-- 列を @rcResolver@ で読み、 'Primitive' 列を返すだけ。
--
-- @
-- cabal run custom-mark-demo
-- @
-- → カレントディレクトリに @custom-mark-demo.svg@ を生成。
--
-- 詳細な拡張ガイドは @docs/api-guide/10-custom-marks.md@。 canvas parity が欲しい時は同じ id で
-- PS registry (@Graphics.Hgg.Custom.registerMark@) に draw 関数を手登録する。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Graphics.Hgg.Backend.SVG (saveSVGWith)
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Primitive   (Primitive (..), Point (..),
                                           TextStyle (..), TextAnchor (..), solid)
import           Graphics.Hgg.Spec        (RenderCtx (..), ColRef, ColData (..),
                                           customMark, encX, encY, resolveNum)
import           Graphics.Hgg.Unit        (px, (*~))
import qualified Data.Vector              as V

-- | ① draw: @"leaf"@ (葉の x 位置)・@"height"@ (節の高さ) 列を 'rcResolver' で読み、 併合を
--   「Π 字」の elbow で描く (簡単のため 4 葉固定トポロジ)。 座標は data 空間で作り 'rcProjectXY'
--   で px 化するので、 軸 scale/coord に自動追従する。
dendroDraw :: RenderCtx -> [Primitive]
dendroDraw ctx =
  let num nm = maybe [] V.toList (resolveNum (rcResolver ctx) nm)  -- 列を読む
      p x y  = uncurry Point (rcProjectXY ctx x y)                 -- data→px
      ls     = solid (rcColor ctx) 1.5
      -- 子を高さ hy で併合する Π 字 (子の基準高さ by1/by2 から立てる)
      elbow cx1 by1 cx2 by2 hy =
        [ PLine (p cx1 by1) (p cx1 hy) ls
        , PLine (p cx2 by2) (p cx2 hy) ls
        , PLine (p cx1 hy)  (p cx2 hy) ls ]
      ts = TextStyle (rcTextColor ctx) 10 "sans-serif" AnchorMiddle 0 "normal" False
  in case (num "leaf", num "height") of
       ([x0, x1, x2, x3], [_, h1, h2]) ->            -- leaf=[0,1,2,3], height=[0,1,2]
         let m01 = (x0 + x1) / 2
             m23 = (x2 + x3) / 2
         in concat
              [ elbow x0 0 x1 0 h1                    -- 葉0,1 → 高さ h1
              , elbow x2 0 x3 0 h1                    -- 葉2,3 → 高さ h1
              , elbow m01 h1 m23 h1 h2                -- (0,1) と (2,3) → 高さ h2 = root
              , [ PText (p x (-0.12)) nm ts
                | (x, nm) <- zip [x0, x1, x2, x3] ["A", "B", "C", "D"] ] ]
       _ -> []

-- | ② 名前付き combinator = 普通の mark 化。 x/y 列を束ねて軸 range を自動化する。
dendrogram :: ColRef -> ColRef -> Layer
dendrogram x y = customMark "dendrogram" dendroDraw <> encX x <> encY y

main :: IO ()
main = do
  -- ③ 使う側 = scatter x y と同じ。 列は resolver (dat) / df |>> で供給。
  let dat "leaf"   = Just (NumData (V.fromList [0, 1, 2, 3]))  -- 葉の x 位置
      dat "height" = Just (NumData (V.fromList [0, 1, 2]))     -- 節の高さ (葉基準0 / 1段 / root)
      dat _        = Nothing
      spec = purePlot
        <> layer (dendrogram "leaf" "height")   -- ★ core 無改造・組み込み mark と同じ書き味
        <> title  "Custom mark demo (簡易 dendrogram)"
        <> xLabel "leaf" <> yLabel "merge height"
        <> widthUnit (600 *~ px) <> heightUnit (400 *~ px)
  saveSVGWith "custom-mark-demo.svg" dat spec
  putStrLn "wrote custom-mark-demo.svg (custom mark = 簡易 dendrogram・core 無改造)"
