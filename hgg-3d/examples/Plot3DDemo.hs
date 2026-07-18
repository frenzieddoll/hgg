-- | Phase 3 demo: hgg-3d を使った 3D plot SVG 生成 (= 全 5 case)。
--
-- @
-- cabal run plot3d-demo
-- @
-- → design/3d/ に 5 SVG (scatter helix/cube、 line helix、 wireframe cube、 surface sinc)。
--
-- A8 で hgg-svg の savePrimitivesSVG (= 新公開 helper) に統合済。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Directory                (createDirectoryIfMissing)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Hgg.Plot.Backend.SVG        (savePrimitivesSVG)

import           Hgg.Plot.ThreeD.Axes        (defaultAxes3D, Axes3D (..),
                                                  renderAxes3D)
import           Hgg.Plot.ThreeD.Projection  (Viewport (..))
import           Hgg.Plot.ThreeD.Line        (defaultLine3D, defaultWireframe3D,
                                                  renderLine3D, renderWireframe3D)
import           Hgg.Plot.ThreeD.Scatter     (defaultScatter3D, Scatter3D (..),
                                                  renderScatter3D)
import           Hgg.Plot.ThreeD.Surface     (renderSurface3D, surfaceFromFunction,
                                                  sf3Grid, sf3XRange, sf3YRange)
import           Hgg.Plot.ThreeD.Spec        (layer3D, surface3DGrid, colormap3D,
                                                  surfaceWire, color3D, xRange3D, yRange3D, layerToSurface,
                                                  contourX, contourY, contourZ,
                                                  axisTitles3D, zAspect3D,
                                                  scatter3DPoints, scatter3D, colorBy3D,
                                                  colorContinuousBy3D, sizeBy3D, sizeRange3D,
                                                  bar3D, errorBar3D, barWidth3D,
                                                  stem3D, stemBaseZ,
                                                  quiver3D, vecScale3D,
                                                  trisurf,
                                                  text3DPoints, annotate3D,
                                                  line3DPoints, logScale3D, xAspect3D,
                                                  color3D, size3D, alpha3D,
                                                  xRange3D, yRange3D, camera, title3D,
                                                  axes3D, pane3D, width3DV, height3DV)
import           Hgg.Plot.ThreeD.Easy        (saveSVG3D, saveSVG3DFacet,
                                                  savePDF3D, savePNG3D)
import           Hgg.Plot.ThreeD.Bound       (saveSVG3DBound)
import           Hgg.Plot.Frame              ((|>>))
import           Hgg.Plot.Spec               (ColData (..))
import           Hgg.Plot.Color              (fromHex)
import qualified Data.Vector                     as V
import           Hgg.Plot.ThreeD.Types

main :: IO ()
main = do
  let outDir = "design/3d"
  createDirectoryIfMissing True outDir

  -- z-up convention (= defaultCameraZUp helper、 mplot3d 慣例)
  let cam   = defaultCameraZUp 3        -- (3, -3, 1.8) → origin
      proj  = defaultPerspective        -- fov 45° / aspect 1:1
      vp    = Viewport 50 50 600 600

  -- ===========================================================================
  -- Case 1: scatter3D helix (= 螺旋曲線 200 点)
  -- ===========================================================================
  let axes  = defaultAxes3D { axesXMin = -1, axesXMax = 1
                            , axesYMin = -1, axesYMax = 1
                            , axesZMin = 0,  axesZMax = 2 }
      helixPts =
        [ Point3 (cos (t * 4 * pi) * 0.8)
                 (sin (t * 4 * pi) * 0.8)
                 (t * 2)
        | i <- [0 .. 199 :: Int]
        , let t = fromIntegral i / 199.0 ]
      sc = defaultScatter3D helixPts
      prims = renderAxes3D cam proj vp axes
           <> renderScatter3D cam proj vp sc
  savePrimitivesSVG (outDir <> "/scatter3d-helix.svg") 700 700
    "3D scatter: helix (200 pts)" prims
  putStrLn "  helix → scatter3d-helix.svg"

  -- ===========================================================================
  -- Case 2: scatter3D cube random (= 立方体内 500 点)
  -- ===========================================================================
  let cubePts =
        [ Point3 (frac (i * 0.317) * 2 - 1)
                 (frac (i * 0.793) * 2 - 1)
                 (frac (i * 0.491) * 2 - 1)
        | i <- [1 .. 500] :: [Double] ]
      sc2 = (defaultScatter3D cubePts) { sc3Color = "#d62728", sc3Size = 4 }
      prims2 = renderAxes3D cam proj vp defaultAxes3D
            <> renderScatter3D cam proj vp sc2
  savePrimitivesSVG (outDir <> "/scatter3d-cube.svg") 700 700
    "3D scatter: cube random (500 pts)" prims2
  putStrLn "  cube → scatter3d-cube.svg"

  -- ===========================================================================
  -- Case 3: line3D helix
  -- ===========================================================================
  let ln = defaultLine3D helixPts
      prims3 = renderAxes3D cam proj vp axes
            <> renderLine3D cam proj vp ln
  savePrimitivesSVG (outDir <> "/line3d-helix.svg") 700 700
    "3D line: helix (200 pts as continuous line)" prims3
  putStrLn "  line helix → line3d-helix.svg"

  -- ===========================================================================
  -- Case 4: wireframe3D cube (= 8 corners + 12 edges)
  -- ===========================================================================
  let cubeCorners =
        [ Point3 (-1) (-1) (-1), Point3 1 (-1) (-1), Point3 1 1 (-1), Point3 (-1) 1 (-1)
        , Point3 (-1) (-1)   1 , Point3 1 (-1)   1 , Point3 1 1   1 , Point3 (-1) 1   1
        ]
      cubeEdges = [(0,1),(1,2),(2,3),(3,0),(4,5),(5,6),(6,7),(7,4),(0,4),(1,5),(2,6),(3,7)]
      wf = defaultWireframe3D cubeCorners cubeEdges
      prims4 = renderAxes3D cam proj vp defaultAxes3D
            <> renderWireframe3D cam proj vp wf
  savePrimitivesSVG (outDir <> "/wireframe3d-cube.svg") 700 700
    "3D wireframe: cube (8 corners, 12 edges)" prims4
  putStrLn "  wireframe cube → wireframe3d-cube.svg"

  -- ===========================================================================
  -- Case 5: surface3DGrid sinc-like (= z = sin(3r)/r / 2、 40x40 grid + Lambert)
  -- z-up: 膜が +z 方向に立ち上がる (= defaultCameraZUp 経由)
  -- ===========================================================================
  let surfCam = defaultCameraZUp 5      -- (5, -5, 3) → origin
      surfAxes = defaultAxes3D { axesXMin = -3, axesXMax = 3
                               , axesYMin = -3, axesYMax = 3
                               , axesZMin = -0.3, axesZMax = 1 }
      surf = surfaceFromFunction 40 40 (-3) 3 (-3) 3
               (\x y -> let r = sqrt (x*x + y*y) + 1e-6 in sin (r * 3) / r * 0.5)
      prims5 = renderAxes3D surfCam proj vp surfAxes
            <> renderSurface3D surfCam proj vp surf
  savePrimitivesSVG (outDir <> "/surface3d-sinc.svg") 700 700
    "3D surface: z = sin(3r)/r / 2 (40x40 grid, Lambert shading, z-up)" prims5
  putStrLn "  surface sinc (z-up) → surface3d-sinc.svg"

  -- Case 5b: 同 surface を surfaceWire で線メッシュ描画 (plot_wireframe 相当・spec 経路)
  let wN      = 18
      wAxis   = [ -3 + 6 * fromIntegral i / fromIntegral wN | i <- [0 .. wN] ]
      wGrid   = [ [ let r = sqrt (x*x + y*y) + 1e-6 in sin (r * 3) / r * 0.5 | x <- wAxis ] | y <- wAxis ]
      wireSurf = layerToSurface ( surface3DGrid wGrid <> surfaceWire <> color3D (fromHex "#2563eb")
                                  <> xRange3D (-3, 3) <> yRange3D (-3, 3) )
      prims5b = renderAxes3D surfCam proj vp surfAxes
             <> renderSurface3D surfCam proj vp wireSurf
  savePrimitivesSVG (outDir <> "/surface3d-wire.svg") 700 700
    "3D surface wireframe (surfaceWire = plot_wireframe 相当)" prims5b
  putStrLn "  surface wireframe → surface3d-wire.svg"

  -- ===========================================================================
  -- Case 6: 同 surface を y-up convention で描画 (= OpenGL 慣例の比較用)
  -- 同データだが camera up = (0, 1, 0) → 「z が水平、 y が縦」 の見え方
  -- ===========================================================================
  let surfCamYUp = defaultCameraYUp 5    -- (5, 3, 5) → origin、 y = up
      prims6 = renderAxes3D surfCamYUp proj vp surfAxes
            <> renderSurface3D surfCamYUp proj vp surf
  savePrimitivesSVG (outDir <> "/surface3d-sinc-yup.svg") 700 700
    "Same surface but y-up convention (OpenGL/Unity, for comparison)" prims6
  putStrLn "  surface sinc (y-up) → surface3d-sinc-yup.svg"

  -- ===========================================================================
  -- Case 7 (Phase 24 A2): 同 surface を z colormap (viridis) + colorbar で。
  -- saveSVG3D (Spec Monoid 経路) を使う = colorbar が自動で付く
  -- ===========================================================================
  saveSVG3D (outDir <> "/surface3d-sinc-colormap.svg")
    (layer3D (surface3DGrid (sf3Grid surf)
                <> xRange3D (sf3XRange surf) <> yRange3D (sf3YRange surf)
                <> colormap3D)
       <> camera surfCam
       <> title3D "3D surface: z colormap (viridis) + colorbar")
  putStrLn "  surface sinc (colormap) → surface3d-sinc-colormap.svg"

  -- ===========================================================================
  -- Case 8 (Phase 24 A5): colormap surface + 床面投影 contour (plotly contours_z)
  -- floorContour3D を足すだけで床に等高線が落ちる
  -- ===========================================================================
  saveSVG3D (outDir <> "/surface3d-sinc-floor-contour.svg")
    (layer3D (surface3DGrid (sf3Grid surf)
                <> xRange3D (sf3XRange surf) <> yRange3D (sf3YRange surf)
                <> colormap3D <> contourZ 8)
       <> camera surfCam
       <> title3D "3D surface + floor-projected contour (contourZ)")
  putStrLn "  surface sinc (floor contour) → surface3d-sinc-floor-contour.svg"

  -- ===========================================================================
  -- Case 8b (#2): 壁面投影 contour (matplotlib contour zdir='x'/'y'/'z' 同型)。
  -- 1 つの surface に contourX/Y/Z を <> で合成するだけ。 投影壁はカメラから
  -- 遠い面に自動固定 (床=z 等値面・左右/前後壁=断面プロファイル)。
  -- ===========================================================================
  -- 軸を data (±3) より広く取り、 壁を曲面から離して断面を見やすく。
  let wallAxes = defaultAxes3D { axesXMin = -4.5, axesXMax = 4.5
                               , axesYMin = -4.5, axesYMax = 4.5
                               , axesZMin = -0.6, axesZMax = 1.8 }
  saveSVG3D (outDir <> "/surface3d-wall-contour.svg")
    (  layer3D ( surface3DGrid (sf3Grid surf)
              <> xRange3D (sf3XRange surf) <> yRange3D (sf3YRange surf)
              <> colormap3D
              <> contourX 8 <> contourY 8 <> contourZ 8 )
    <> axes3D wallAxes
    <> camera surfCam
    <> title3D "3D surface + wall-projected contour (zdir x/y/z)")
  putStrLn "  surface sinc (wall contour x/y/z) → surface3d-wall-contour.svg"

  -- ===========================================================================
  -- Case 9 (Phase 24 A7): 群別曲面の並置 (facet)。 2 群 (peak 鋭い / 緩い) の
  -- 応答曲面を colormap + colorbar 付きで並置
  -- ===========================================================================
  let mkSurf k = surfaceFromFunction 40 40 (-3) 3 (-3) 3
                   (\x y -> let r = sqrt (x*x + y*y) + 1e-6 in sin (r * 3) / r * k)
      panel label k =
        ( label
        , layer3D (surface3DGrid (sf3Grid (mkSurf k))
                     <> xRange3D (-3, 3) <> yRange3D (-3, 3) <> colormap3D)
            <> camera surfCam )
  saveSVG3DFacet (outDir <> "/surface3d-facet.svg")
    [ panel "group A (k=0.5)" 0.5, panel "group B (k=0.25)" 0.25 ]
  putStrLn "  surface facet (2 群) → surface3d-facet.svg"

  -- ===========================================================================
  -- Case 10 (Phase 24 A8): 軸タイトル + z aspect + 視点 preset。 RSM 風の応答に
  -- 物理量の軸名を付け、 z を 1.6 倍に縦伸ばし、 iso 視点で
  -- ===========================================================================
  let rsm = layer3D (surface3DGrid (sf3Grid surf)
                       <> xRange3D (sf3XRange surf) <> yRange3D (sf3YRange surf)
                       <> colormap3D)
              <> camera (cameraIso 5)
              <> axisTitles3D "温度 [°C]" "圧力 [bar]" "収率 [%]"
              <> zAspect3D 1.6
              <> title3D "応答曲面: 軸名 + z aspect 1.6"
  saveSVG3D    (outDir <> "/surface3d-a8-axes-aspect.svg") rsm
  savePNG3D    (outDir <> "/surface3d-a8-axes-aspect.png") rsm   -- 日本語ラベル可
  putStrLn "  surface A8 (軸名+aspect+PNG) → surface3d-a8-axes-aspect.{svg,png}"

  -- Case 11 (Phase 24 A8): PDF glue (Latin ラベルで). 視点 preset = top
  let rsmEn = layer3D (surface3DGrid (sf3Grid surf)
                         <> xRange3D (sf3XRange surf) <> yRange3D (sf3YRange surf)
                         <> colormap3D)
                <> camera (cameraIso 5)
                <> axisTitles3D "temp" "pressure" "yield"
                <> title3D "Response surface (PDF)"
  savePDF3D (outDir <> "/surface3d-a8.pdf") rsmEn
  putStrLn "  surface A8 (PDF) → surface3d-a8.pdf"

  -- ===========================================================================
  -- Case 12 (Phase 24 A8): 層間 depth 統合。 surface (半透明) + 実測点。
  -- 膜の奥の点は隠れ、 手前の点は前に出る (旧実装は全点が膜の前に浮いていた)
  -- ===========================================================================
  let dome = surfaceFromFunction 30 30 (-2) 2 (-2) 2
               (\x y -> 1.2 - 0.3 * (x*x + y*y))   -- 上に膨らむドーム
      -- r=1.2 の輪に沿って z を膜 (= domeZ 0.77) の上下に大きく振る:
      -- 膜より上の点は見え、 下の点は膜に隠れる
      obsPts = [ Point3 (cos t * 1.2) (sin t * 1.2) (0.77 + 0.6 * sin (t * 2))
               | k <- [0 .. 23 :: Int], let t = fromIntegral k / 24 * 2 * pi ]
      depthSpec = layer3D (surface3DGrid (sf3Grid dome)
                             <> xRange3D (-2, 2) <> yRange3D (-2, 2)
                             <> colormap3D)
               <> layer3D (scatter3DPoints obsPts <> color3D (fromHex "#d62728") <> size3D 6)
               <> camera (cameraIso 5)
               <> title3D "depth 統合: surface 膜の奥の点は隠れ手前は前に"
  saveSVG3D (outDir <> "/surface3d-a8-depth.svg") depthSpec
  putStrLn "  surface A8 (depth 統合) → surface3d-a8-depth.svg"

  -- ===========================================================================
  -- Case 13 (Phase 25 A2): 群色分け 3D 散布 + 離散凡例。 3 群のクラスタを
  -- df |>> (scatter3D ColRef + colorBy3D) で色分け + 自動凡例
  -- ===========================================================================
  let clusterAt cx cy cz k =
        [ ( cx + 0.6 * cos (t*5) * frac (fromIntegral k * 1.7 + t)
          , cy + 0.6 * sin (t*5) * frac (fromIntegral k * 2.3 + t)
          , cz + 0.5 * (frac (fromIntegral k * 3.1 + t) - 0.5) )
        | i <- [0 .. 14 :: Int], let t = fromIntegral i / 15 ]
      clA = clusterAt 0   0   0   1
      clB = clusterAt 2   1.5 1   2
      clC = clusterAt 1   2.5 (-1) 3
      allPts = clA ++ clB ++ clC
      labels = replicate (length clA) "cluster A"
            ++ replicate (length clB) "cluster B"
            ++ replicate (length clC) "cluster C"
      gdf = [ ("x",     NumData (V.fromList [ x | (x,_,_) <- allPts ]))
            , ("y",     NumData (V.fromList [ y | (_,y,_) <- allPts ]))
            , ("z",     NumData (V.fromList [ z | (_,_,z) <- allPts ]))
            , ("group", TxtData (V.fromList labels)) ] :: [(Text, ColData)]
      gspec = layer3D (scatter3D "x" "y" "z" <> colorBy3D "group" <> size3D 6)
           <> camera (defaultCameraZUp 5)
           <> axisTitles3D "PC1" "PC2" "PC3"
           <> width3DV 640 <> height3DV 600
           <> title3D "群色分け 3D 散布 + 離散凡例"
  saveSVG3DBound (outDir <> "/scatter3d-groups.svg") (gdf |>> gspec)
  putStrLn "  scatter 群色分け + 凡例 → scatter3d-groups.svg"

  -- ===========================================================================
  -- Case 14 (Phase 25 A3): 連続色マップ + size マップ (bubble)。 螺旋上の点を
  -- 値列 v で colorContinuousBy3D (viridis + colorbar)、 m で sizeBy3D (bubble)。
  -- ===========================================================================
  let nB    = 120 :: Int
      bt i  = fromIntegral i / fromIntegral nB
      bxs   = [ 1.4 * cos (bt i * 6.0) | i <- [0 .. nB - 1] ]
      bys   = [ 1.4 * sin (bt i * 6.0) | i <- [0 .. nB - 1] ]
      bzs   = [ 2.0 * bt i - 1.0       | i <- [0 .. nB - 1] ]
      bvs   = [ bt i                   | i <- [0 .. nB - 1] ]   -- 高さ方向の連続値
      bms   = [ 0.5 + frac (fromIntegral i * 1.3) | i <- [0 .. nB - 1] ]  -- size 値
      bdf   = [ ("x", NumData (V.fromList bxs))
              , ("y", NumData (V.fromList bys))
              , ("z", NumData (V.fromList bzs))
              , ("v", NumData (V.fromList bvs))
              , ("m", NumData (V.fromList bms)) ] :: [(Text, ColData)]
      bspec = layer3D (scatter3D "x" "y" "z"
                        <> colorContinuousBy3D "v"
                        <> sizeBy3D "m" <> sizeRange3D (3, 16))
           <> camera (defaultCameraZUp 4)
           <> axisTitles3D "x" "y" "height"
           <> width3DV 680 <> height3DV 600
           <> title3D "連続色 + size マップ bubble"
  saveSVG3DBound (outDir <> "/scatter3d-value-bubble.svg") (bdf |>> bspec)
  putStrLn "  連続色 + bubble → scatter3d-value-bubble.svg"

  -- ===========================================================================
  -- Case 15 (Phase 25 A4): 半透明 surface。 Case 12 と同じドーム + 点群だが
  -- surface を alpha3D 0.45 にして、 膜の奥の点が「透けて薄く」見える
  -- (A8 の不透明膜は奥の点を完全に隠す。 alpha で中間表現が可能に)。
  -- ===========================================================================
  let domeA  = surfaceFromFunction 30 30 (-2) 2 (-2) 2
                 (\x y -> 1.2 - 0.3 * (x*x + y*y))
      obsA   = [ Point3 (cos t * 1.2) (sin t * 1.2) (0.77 + 0.6 * sin (t * 2))
               | k <- [0 .. 23 :: Int], let t = fromIntegral k / 24 * 2 * pi ]
      alphaSpec = layer3D (surface3DGrid (sf3Grid domeA)
                            <> xRange3D (-2, 2) <> yRange3D (-2, 2)
                            <> colormap3D <> alpha3D 0.45)
               <> layer3D (scatter3DPoints obsA <> color3D (fromHex "#d62728") <> size3D 6)
               <> camera (cameraIso 5)
               <> title3D "半透明 surface (alpha 0.45): 奥の点が透ける"
  saveSVG3D (outDir <> "/surface3d-a4-translucent.svg") alphaSpec
  putStrLn "  半透明 surface → surface3d-a4-translucent.svg"

  -- ===========================================================================
  -- Case 16 (Phase 25 A5): DoE 風 3D 棒グラフ + 誤差棒。 2 因子 (3x3) の応答を
  -- 直方体 bar で、 標準誤差を z 方向 ±se の誤差棒で。 df |>> (bar3D + errorBar3D)。
  -- ===========================================================================
  let factorX = [0, 1, 2] :: [Double]
      factorY = [0, 1, 2] :: [Double]
      cellsXY = [ (fx, fy) | fy <- factorY, fx <- factorX ]
      respAt fx fy = 4 + 2 * fx + 1.5 * fy - 0.4 * fx * fy   -- 応答 (交互作用あり)
      seAt   fx fy = 0.4 + 0.15 * (fx + fy)                  -- ばらつき (右奥ほど大)
      bardf = [ ("fx", NumData (V.fromList [ fx       | (fx, _ ) <- cellsXY ]))
              , ("fy", NumData (V.fromList [ fy       | (_,  fy) <- cellsXY ]))
              , ("resp", NumData (V.fromList [ respAt fx fy | (fx, fy) <- cellsXY ]))
              , ("se",   NumData (V.fromList [ seAt   fx fy | (fx, fy) <- cellsXY ])) ]
              :: [(Text, ColData)]
      barSpec = layer3D (bar3D "fx" "fy" "resp"
                          <> color3D (fromHex "#5b9bd5") <> barWidth3D 0.08
                          <> errorBar3D "se")
             <> camera (cameraIso 5)
             <> axisTitles3D "factor X" "factor Y" "response"
             <> width3DV 680 <> height3DV 600
             <> title3D "DoE 応答の 3D 棒 + 標準誤差棒"
  saveSVG3DBound (outDir <> "/bar3d-doe-errorbar.svg") (bardf |>> barSpec)
  putStrLn "  DoE 3D 棒 + 誤差棒 → bar3d-doe-errorbar.svg"

  -- ===========================================================================
  -- Case 17 (Phase 25 A6): 壁面 pane + gridline (mplot3d 標準の axes box)。
  -- 既定 ON。 同じ surface+点群を pane ON / OFF で並べ、 奥 3 壁の薄灰 pane +
  -- 白格子線が背面に出ること (回転追従) を確認。 PNG も出す (目視用)。
  -- ===========================================================================
  let domeP  = surfaceFromFunction 30 30 (-2) 2 (-2) 2
                 (\x y -> 1.2 - 0.3 * (x*x + y*y))
      obsP   = [ Point3 (cos t * 1.4) (sin t * 1.4) (0.3 + 0.6 * sin (t * 2))
               | k <- [0 .. 29 :: Int], let t = fromIntegral k / 30 * 2 * pi ]
      paneBase = layer3D (surface3DGrid (sf3Grid domeP)
                           <> xRange3D (-2, 2) <> yRange3D (-2, 2) <> colormap3D)
              <> layer3D (scatter3DPoints obsP <> color3D (fromHex "#d62728") <> size3D 5)
              <> camera (cameraIso 5)
              <> axisTitles3D "x" "y" "z"
      paneOnSpec  = paneBase
                 <> title3D "壁面 pane ON (mplot3d 標準・既定): 背面 3 壁 + 白格子"
      paneOffSpec = paneBase <> pane3D False
                 <> title3D "壁面 pane OFF: 従来の cube wireframe + tick のみ"
  saveSVG3D (outDir <> "/surface3d-a6-pane-on.svg")  paneOnSpec
  savePNG3D (outDir <> "/surface3d-a6-pane-on.png")  paneOnSpec
  saveSVG3D (outDir <> "/surface3d-a6-pane-off.svg") paneOffSpec
  putStrLn "  壁面 pane ON/OFF → surface3d-a6-pane-{on,off}.svg (+ on.png)"

  -- ===========================================================================
  -- Case 18 (Phase 25 A7): 3D テキスト注釈 (text3D / annotate3D)。 ドーム surface の
  -- 頂点と 3 コーナーにラベルを置く。 annotate3D で頂点を赤・大きめに強調。
  -- ===========================================================================
  let domeT = surfaceFromFunction 30 30 (-2) 2 (-2) 2
                (\x y -> 1.2 - 0.3 * (x*x + y*y))
      annSpec = layer3D (surface3DGrid (sf3Grid domeT)
                          <> xRange3D (-2, 2) <> yRange3D (-2, 2) <> colormap3D)
             <> layer3D (annotate3D (Point3 0 0 1.2) "peak"
                          <> color3D (fromHex "#d62728") <> size3D 14)
             <> layer3D (text3DPoints [ (Point3 (-2) (-2) (-1.2), "(-2,-2)")
                                 , (Point3 2 2 (-1.2), "(2,2)")
                                 , (Point3 (-2) 2 (-1.2), "(-2,2)") ]
                          <> size3D 10)
             <> camera (cameraIso 5)
             <> axisTitles3D "x" "y" "z"
             <> title3D "3D テキスト注釈: 頂点 + コーナーラベル"
  saveSVG3D (outDir <> "/surface3d-a7-annotate.svg") annSpec
  savePNG3D (outDir <> "/surface3d-a7-annotate.png") annSpec   -- 日本語ラベル可
  putStrLn "  3D テキスト注釈 → surface3d-a7-annotate.svg (+ png)"

  -- ===========================================================================
  -- Case 19 (Phase 25 A8): log-z 軸。 z が decade (1..1000) を跨ぐ指数的データを
  -- 片対数 (logScale3D False False True) で。 z tick は 10 の冪・等間隔に並ぶ。
  -- ===========================================================================
  let logPts = [ Point3 (fromIntegral k / 5) (fromIntegral k / 5)
                        (10 ** (fromIntegral k / 5 - 1))   -- z: 0.1 .. 1000
               | k <- [0 .. 20 :: Int] ]
      logSpec = layer3D (line3DPoints logPts <> color3D (fromHex "#1f77b4"))
             <> layer3D (scatter3DPoints logPts <> color3D (fromHex "#d62728") <> size3D 5)
             <> logScale3D False False True
             <> camera (cameraIso 5)
             <> axisTitles3D "x" "y" "z (log)"
             <> title3D "log-z 軸 (片対数): z tick が 10 の冪で等間隔"
  saveSVG3D (outDir <> "/scatter3d-a8-logz.svg") logSpec
  savePNG3D (outDir <> "/scatter3d-a8-logz.png") logSpec
  putStrLn "  log-z 軸 → scatter3d-a8-logz.svg (+ png)"

  -- ===========================================================================
  -- Case 20 (Phase 25 A8): box アスペクト。 同じドームを x 方向に 1.6 倍・z を
  -- 0.6 倍に潰した box で (xAspect3D / zAspect3D)。 mplot3d set_box_aspect 相当。
  -- ===========================================================================
  let domeAsp = surfaceFromFunction 30 30 (-2) 2 (-2) 2
                  (\x y -> 1.2 - 0.3 * (x*x + y*y))
      aspSpec = layer3D (surface3DGrid (sf3Grid domeAsp)
                          <> xRange3D (-2, 2) <> yRange3D (-2, 2) <> colormap3D)
             <> xAspect3D 1.6 <> zAspect3D 0.6
             <> camera (cameraIso 5)
             <> axisTitles3D "x" "y" "z"
             <> title3D "box アスペクト: x×1.6 / z×0.6"
  saveSVG3D (outDir <> "/surface3d-a8-aspect.svg") aspSpec
  savePNG3D (outDir <> "/surface3d-a8-aspect.png") aspSpec
  putStrLn "  box アスペクト → surface3d-a8-aspect.svg (+ png)"

  -- ===========================================================================
  -- Case 21 (Phase 25 A9 統合): クラスタ分類の 3D 散布。 群色 (colorBy3D) +
  -- 各クラスタ重心に text3D ラベル + pane。 「分類結果の 3D 可視化」 の定番。
  -- ===========================================================================
  let cluster cx cy cz n = [ Point3 (cx + 0.5 * cos t) (cy + 0.5 * sin t) (cz + 0.4 * cos (2*t))
                           | i <- [0 .. n-1 :: Int], let t = fromIntegral i / fromIntegral n * 2 * pi ]
      cA = cluster 0    0    0    14
      cB = cluster 2.2  1.5  1.0  14
      cC = cluster 1.0  2.5 (-1.0) 14
      clusterDf = [ ("x", NumData (V.fromList [ x | Point3 x _ _ <- cA++cB++cC ]))
                  , ("y", NumData (V.fromList [ y | Point3 _ y _ <- cA++cB++cC ]))
                  , ("z", NumData (V.fromList [ z | Point3 _ _ z <- cA++cB++cC ]))
                  , ("cluster", TxtData (V.fromList (replicate 14 "A" ++ replicate 14 "B" ++ replicate 14 "C"))) ]
                  :: [(Text, ColData)]
      clusterSpec = layer3D (scatter3D "x" "y" "z" <> colorBy3D "cluster" <> size3D 5)
                 <> layer3D (text3DPoints [ (Point3 0 0 0.7, "A"), (Point3 2.2 1.5 1.7, "B")
                                    , (Point3 1.0 2.5 (-0.3), "C") ]
                              <> size3D 16 <> color3D (fromHex "#222222"))
                 <> camera (cameraIso 5)
                 <> axisTitles3D "PC1" "PC2" "PC3"
                 <> title3D "クラスタ分類の 3D 散布 (群色 + 重心ラベル)"
  saveSVG3DBound (outDir <> "/scatter3d-a9-clusters.svg") (clusterDf |>> clusterSpec)
  putStrLn "  クラスタ散布 → scatter3d-a9-clusters.svg"

  -- ===========================================================================
  -- Case 22 (Phase 25 A9 統合): 濃度-応答の bubble。 連続色 (colorContinuousBy3D) +
  -- size マップ (sizeBy3D) + log-x 軸 (濃度が decade を跨ぐ)。 用量反応の定番。
  -- ===========================================================================
  let doses  = [ 10 ** (fromIntegral k / 4 - 1) | k <- [0 .. 12 :: Int] ]  -- 0.1 .. ~1000
      respD  = [ 100 / (1 + (50 / d)) | d <- doses ]                        -- Hill 風
      doseDf = [ ("dose", NumData (V.fromList doses))
               , ("time", NumData (V.fromList [ fromIntegral k | k <- [0 .. 12 :: Int] ]))
               , ("resp", NumData (V.fromList respD))
               , ("effect", NumData (V.fromList respD)) ]
               :: [(Text, ColData)]
      doseSpec = layer3D (scatter3D "dose" "time" "resp"
                           <> colorContinuousBy3D "effect" <> sizeBy3D "effect" <> sizeRange3D (5, 22))
              <> logScale3D True False False        -- x (濃度) を log
              <> camera (cameraIso 5)
              <> axisTitles3D "dose (log)" "time" "response"
              <> title3D "用量反応 bubble (連続色 + size + log-dose)"
  saveSVG3DBound (outDir <> "/scatter3d-a9-dose-bubble.svg") (doseDf |>> doseSpec)
  putStrLn "  用量反応 bubble → scatter3d-a9-dose-bubble.svg"

  -- ===========================================================================
  -- Case 23 (Phase 25 A9 統合): DoE 応答の 3D 棒 + 各セルに応答値ラベル (text3D)。
  -- bar3d + 誤差棒 + 数値注釈 + 軸名。 実務的な要因配置実験のまとめ図。
  -- ===========================================================================
  let cells   = [ (fx, fy) | fy <- [0,1,2::Double], fx <- [0,1,2::Double] ]
      respB fx fy = 4 + 2*fx + 1.5*fy - 0.4*fx*fy
      barDf2  = [ ("fx", NumData (V.fromList [ fx | (fx,_) <- cells ]))
                , ("fy", NumData (V.fromList [ fy | (_,fy) <- cells ]))
                , ("r",  NumData (V.fromList [ respB fx fy | (fx,fy) <- cells ]))
                , ("se", NumData (V.fromList [ 0.3 + 0.1*(fx+fy) | (fx,fy) <- cells ])) ]
                :: [(Text, ColData)]
      labels23 = [ (Point3 fx fy (respB fx fy + 1.2), tShow (respB fx fy)) | (fx,fy) <- cells ]
      barSpec2 = layer3D (bar3D "fx" "fy" "r" <> color3D (fromHex "#5b9bd5") <> barWidth3D 0.08 <> errorBar3D "se")
              <> layer3D (text3DPoints labels23 <> size3D 10 <> color3D (fromHex "#222222"))
              <> camera (cameraIso 5)
              <> axisTitles3D "factor X" "factor Y" "response"
              <> width3DV 700 <> height3DV 620
              <> title3D "DoE 応答 3D 棒 + 誤差棒 + 値ラベル"
  saveSVG3DBound (outDir <> "/bar3d-a9-doe-labeled.svg") (barDf2 |>> barSpec2)
  putStrLn "  DoE 棒 + 値ラベル → bar3d-a9-doe-labeled.svg"

  -- ===========================================================================
  -- Case 24 (Phase 26 A4): 3D stem (lollipop)。 離散系列を底面 z0 への細い垂線 +
  -- 先端マーカーで。 1 群は単色 stem3D、 もう 1 群は群色 + 別 baseZ で重畳。
  -- ===========================================================================
  let stemXs  = [0,0.5..3] :: [Double]
      stemY1  = 1.0 :: Double
      stemY2  = 2.2 :: Double
      respS x = 2 + 1.5 * sin (x * 1.3)
      stemDf  = [ ("x",  NumData (V.fromList stemXs))
                , ("y1", NumData (V.fromList (replicate (length stemXs) stemY1)))
                , ("z1", NumData (V.fromList (map respS stemXs)))
                , ("y2", NumData (V.fromList (replicate (length stemXs) stemY2)))
                , ("z2", NumData (V.fromList (map (\x -> 1 + cos (x * 0.9)) stemXs))) ]
                :: [(Text, ColData)]
      stemSpec = layer3D (stem3D "x" "y1" "z1" <> color3D (fromHex "#d62728") <> size3D 5 <> stemBaseZ 0)
              <> layer3D (stem3D "x" "y2" "z2" <> color3D (fromHex "#1f77b4") <> size3D 5 <> stemBaseZ 0)
              <> camera (cameraIso 5)
              <> axisTitles3D "t" "series" "value"
              <> width3DV 700 <> height3DV 600
              <> title3D "3D stem (lollipop) — 2 系列"
  saveSVG3DBound (outDir <> "/stem3d-a4.svg") (stemDf |>> stemSpec)
  putStrLn "  3D stem → stem3d-a4.svg"

  -- ===========================================================================
  -- Case 25 (Phase 26 A3): 3D vector field (quiver3D)。 3D 格子点に渦巻き状の
  -- 流れ場ベクトルの矢印 (投影後 2D 矢じり)。 magnitude で色は付けず単色。
  -- ===========================================================================
  let qGrid = [ Point3 gx gy gz | gx <- [-1,0,1], gy <- [-1,0,1], gz <- [-1,0,1::Double] ]
      qVecF (Point3 x y z) = Vec3 (-y) x (0.3 * z)   -- z 軸まわりの回転 + 上昇
      qItems = [ (p, qVecF p) | p <- qGrid ]
      quiverSpec = layer3D (quiver3D qItems <> color3D (fromHex "#1f77b4") <> vecScale3D 1.0)
                <> camera (cameraIso 5)
                <> axisTitles3D "x" "y" "z"
                <> width3DV 680 <> height3DV 620
                <> title3D "3D vector field (quiver3D) — 回転流"
  saveSVG3D (outDir <> "/quiver3d-a3.svg") quiverSpec
  putStrLn "  3D vector field → quiver3d-a3.svg"

  -- ===========================================================================
  -- Case 26 (Phase 26 A5): trisurf。 不規則 (非 grid) なサンプル点を Delaunay
  -- 三角分割して曲面化。 z 連続色 (colormap3D)。 散布点位置は擬似乱数で散らす。
  -- ===========================================================================
  let triN = 90 :: Int
      triPts = [ Point3 px py (peak px py)
               | i <- [0 .. triN - 1]
               , let px = frac (fromIntegral i * 0.6131) * 4 - 2
               , let py = frac (fromIntegral i * 0.2971 + 0.13) * 4 - 2
               , let peak x y = exp (negate ((x - 0.3)**2 + (y + 0.2)**2) / 1.2) ]
      triSpec = layer3D (trisurf triPts <> colormap3D)
             <> camera (cameraIso 5)
             <> axisTitles3D "x" "y" "z"
             <> width3DV 680 <> height3DV 620
             <> title3D "trisurf — 不規則点群を Delaunay 三角分割"
  saveSVG3D (outDir <> "/trisurf-a5.svg") triSpec
  putStrLn "  trisurf → trisurf-a5.svg"

  putStrLn ""
  putStrLn "Wrote 31 SVGs/PNG/PDF to design/3d/"

-- | Double を小数 1 桁の Text に (= text3D 値ラベル用)。
tShow :: Double -> Text
tShow x = T.pack (show (fromIntegral (round (x * 10) :: Int) / 10.0 :: Double))

-- | 簡易 hash で擬似乱数 (= 決定論的、 seed なし)。
frac :: Double -> Double
frac x = let y = x - fromIntegral (floor x :: Int) in if y < 0 then y + 1 else y
