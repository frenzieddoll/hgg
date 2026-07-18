-- | api-guide 05-dataframe (DataFrame 連携) 用の図生成デモ。
--   ゼロ依存の Map ベース df を @(|>>)@ でバインドし、 列名だけで図を書く。
--
--   @cabal run df-plot-demo@ → @design/df-plot/*.svg@ を生成。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.Hgg.Backend.SVG (saveSVGBound)
import           Graphics.Hgg.Easy
import           Graphics.Hgg.Frame       ((|>>), BoundPlot, bpDiagnostics)
import           Data.Text                (Text)
import qualified Data.Map.Strict          as M
import qualified Data.Vector              as V
import           System.Directory         (createDirectoryIfMissing)

-- df の列値は ColData (NumData/TxtData)。 数値列・文字列列のヘルパ。
num :: [Double] -> ColData
num = NumData . V.fromList

txt :: [Text] -> ColData
txt = TxtData . V.fromList

out :: FilePath -> BoundPlot -> IO ()
out name = saveSVGBound ("design/df-plot/" <> name <> ".svg")

main :: IO ()
main = do
  createDirectoryIfMissing True "design/df-plot"

  -- 1 枚の df を用意 (Map Text ColData。 列名で参照する)
  let df = M.fromList
        [ ("x",     num [1,2,3,4,5,6,7,8,9,10])
        , ("y",     num [2.1,3.9,6.0,7.7,10.2,11.8,14.1,15.9,18.2,20.0])
        , ("size",  num [2,8,3,9,4,7,5,6,3,8])
        , ("group", txt (take 10 (cycle ["A","B"]))) ] :: M.Map Text ColData

  -- (a) 散布図 + group で色分け + size で大きさ (全部「列名」で指定)
  out "01-scatter-color-size" $
    df |>> ( layer (scatter "x" "y" <> colorBy "group" <> sizeBy "size" <> alpha 0.85)
           <> title "df |>> scatter (color/size を列名で)" )

  -- (b) 重畳: 散布図 + 折れ線 (同じ df の別列を別 layer に)
  out "02-overlay" $
    df |>> ( layer (scatter "x" "y" <> size 6)
           <> layer (line "x" "y" <> color (fromHex "#d62728") <> stroke 1)
           <> title "df |>> (scatter <> line)" )

  -- (c) facet: group 列で小分け
  out "03-facet" $
    df |>> ( layer (scatter "x" "y" <> colorBy "group" <> size 6)
           <> facet "group"
           <> title "df |>> scatter <> facet \"group\"" )

  -- (d) 棒グラフ: カテゴリ列 + 群 + position (dodge)
  let dfB = M.fromList
        [ ("cat", txt (concatMap (replicate 3) ["A","B","C"]))
        , ("grp", txt (take 9 (cycle ["x","y","z"])))
        , ("val", num [3,5,2, 4,1,6, 2,3,4]) ] :: M.Map Text ColData
  out "04-bar-dodge" $
    dfB |>> ( layer (bar "cat" "val" <> colorBy "grp" <> position PosDodge)
            <> title "df |>> bar <> color <> position PosDodge" )

  -- (e) Phase 26 A2: vector field (quiver)。 格子点 (x,y) に渦巻き風の場 (u,v) の矢印。
  let gridPts = [ (gx, gy) | gx <- [-3,-2..3], gy <- [-3,-2..3::Double] ]
      uOf x y = -y/3 - x/6        -- 回転 + 内向き
      vOf x y =  x/3 - y/6
      dfQ = M.fromList
        [ ("x", num [ x | (x,_) <- gridPts ])
        , ("y", num [ y | (_,y) <- gridPts ])
        , ("u", num [ uOf x y | (x,y) <- gridPts ])
        , ("v", num [ vOf x y | (x,y) <- gridPts ]) ] :: M.Map Text ColData
  out "05-quiver" $
    dfQ |>> ( layer (quiver "x" "y" "u" "v" <> color (fromHex "#1f77b4"))
            <> title "df |>> quiver \"x\" \"y\" \"u\" \"v\" (vector field)" )

  -- (f) Phase 26 A2: 同じ場を magnitude で連続色マップ (arrowColorByMagnitude)。
  out "06-quiver-magnitude" $
    dfQ |>> ( layer (quiver "x" "y" "u" "v" <> arrowColorByMagnitude <> arrowScale 1.2)
            <> title "df |>> quiver <> arrowColorByMagnitude (|u,v| -> viridis)" )

  -- バインド時の列名検証 (純関数・例外なし。 診断は値として載る)
  let bad = df |>> layer (scatter "x" "wieght")   -- typo
  putStrLn ("診断 (typo 列): " <> show (bpDiagnostics bad))

  putStrLn "wrote design/df-plot/*.svg (6 examples)"
