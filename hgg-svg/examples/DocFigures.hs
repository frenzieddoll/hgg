-- | docs/api-guide/ に埋め込む図を生成する (P3.1 = api-guide 再構成)。
--   図はページ別モジュール (DocFig.Quickstart / Layers / Decoration) が 'Figure' の
--   リストとして宣言し、 ここはそれらをまとめて出力するだけの薄い入口。
--   @cabal run doc-figures@ (repo root から) で docs/api-guide/images/*.svg を生成。
--   ※ 05 (df/) / 06 (analyze-integration/) / 07 (3d/) の図は別ジェネレータ
--     (df-plot-demo / 各 bridge demo / plot3d-demo → design/* を api-guide へコピー) が担当。
module Main (main) where

import           System.Directory (createDirectoryIfMissing)

import           DocFig.Common      (outDir, renderFigure)
import qualified DocFig.Quickstart    as Quickstart
import qualified DocFig.Layers        as Layers
import qualified DocFig.EncodingScale as EncodingScale
import qualified DocFig.Decoration    as Decoration

main :: IO ()
main = do
  createDirectoryIfMissing True outDir
  let figs = Quickstart.figures ++ Layers.figures
             ++ EncodingScale.figures ++ Decoration.figures
  mapM_ renderFigure figs
  putStrLn ("doc figures written to " ++ outDir
            ++ " (" ++ show (length figs) ++ " figures)")
