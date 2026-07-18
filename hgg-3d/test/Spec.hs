-- | Phase 3 A2 段階の test entry。
-- 中核型 (Vec3 / Mat4) の基本演算のみテスト。 projection 等は A3 以降。
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Hgg.ThreeD.Types
import Graphics.Hgg.ThreeD.Projection
import Graphics.Hgg.ThreeD.Axes
import Graphics.Hgg.ThreeD.Surface (Surface3D (..), defaultSurface3D, renderSurface3D)
import Graphics.Hgg.ThreeD.Spec (surface3DGrid, surface3D, colormap3D, viridisStops3D, layerToSurface,
                                 xRange3D, yRange3D, lyr3XRange, lyr3YRange, lyr3Grid,
                                 contourX, contourY, contourZ, lyr3Contours,
                                 ContourDir(..), text3D, lyr3Labels,
                                 alpha3D)
import Graphics.Hgg.ThreeD.Easy (padAxes3D, normPoint3D, normLayer3D, renderProjectedContour3D,
                                 saveSVG3DFacet, renderSpec3DInPanel,
                                 savePDF3D, savePNG3D)
import Graphics.Hgg.ThreeD.Spec (axisTitles3D, zAspect3D, pane3D, camera, title3D, width3DV, height3DV)
import Graphics.Hgg.ThreeD.Spec (colorBy3D, lyr3PtColors, lyr3Legend, layerToScatter,
                                 colorContinuousBy3D, sizeBy3D, sizeRange3D,
                                 lyr3PtSizes, lyr3Colorbar)
import Graphics.Hgg.ThreeD.Scatter (Scatter3D (..), defaultScatter3D)
import Graphics.Hgg.ThreeD.Spec (bar3D, bar3DPoints, barStyle3D, barWidth3D, errorBar3D,
                                 layerToBar, BarStyle3D (..), lyr3BarBaseZ, lyr3PtErrs)
import Graphics.Hgg.ThreeD.Spec (text3DPoints, annotate3D, color3D, size3D, colorRGBA3D)
import Graphics.Hgg.ThreeD.Spec (xAspect3D, yAspect3D, logScale3D)
import Graphics.Hgg.ThreeD.Spec (stem3D, stem3DPoints, stemBaseZ, layerPoints,
                                 Mark3DKind (..), lyr3Kind)
import Graphics.Hgg.ThreeD.Spec (quiver3D, vecScale3D, layerToQuiver, trisurf)
import Graphics.Hgg.ThreeD.Line (Quiver3D (..), defaultQuiver3D, renderQuiver3D)
import Graphics.Hgg.ThreeD.Delaunay (delaunay2D)
import Graphics.Hgg.ThreeD.Surface (trianglesFacesDepth)
import Graphics.Hgg.ThreeD.Bar (Bar3D (..), defaultBar3D, barFacesDepth,
                                renderBarSticks, renderStems3D, renderErrorBars3D)
import Graphics.Hgg.Palette (ggplotHue)
import Graphics.Hgg.Render.Common (continuousColor)
import Data.List (isInfixOf)
import System.Directory (getFileSize, removeFile, doesFileExist)
import Control.Monad (when)
import Graphics.Hgg.ThreeD.Bound (BoundPlot3D (..), unBound3D)
import Graphics.Hgg.ThreeD.Spec (layer3D, scatter3D, scatter3DPoints, line3DPoints,
                                 resolveLayer3D, lyr3Points, vs3Layers, VisualSpec3D (..))
import Graphics.Hgg.Frame ((|>>))
import Graphics.Hgg.Spec (ColData (..), inline, emptyResolver)
import Graphics.Hgg.Color (fromHex)
import qualified Graphics.Hgg.Validate
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Monoid (Last (..), First (..))
import Data.List (nub)
import qualified Graphics.Hgg.Render
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Phase 3 A2: Vec3 基本演算" $ do
    it "addV3: (1,2,3) + (4,5,6) = (5,7,9)" $
      addV3 (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` Vec3 5 7 9
    it "subV3: (4,5,6) - (1,2,3) = (3,3,3)" $
      subV3 (Vec3 4 5 6) (Vec3 1 2 3) `shouldBe` Vec3 3 3 3
    it "scaleV3: 2 * (1,2,3) = (2,4,6)" $
      scaleV3 2 (Vec3 1 2 3) `shouldBe` Vec3 2 4 6
    it "dotV3: (1,2,3) · (4,5,6) = 32" $
      dotV3 (Vec3 1 2 3) (Vec3 4 5 6) `shouldBe` 32

    it "crossV3: (1,0,0) × (0,1,0) = (0,0,1) (= 右手系)" $
      crossV3 (Vec3 1 0 0) (Vec3 0 1 0) `shouldBe` Vec3 0 0 1
    it "crossV3: (0,1,0) × (0,0,1) = (1,0,0)" $
      crossV3 (Vec3 0 1 0) (Vec3 0 0 1) `shouldBe` Vec3 1 0 0
    it "crossV3 反対称: a × b = -(b × a)" $
      let a = Vec3 2 3 4; b = Vec3 5 6 7
          Vec3 abx aby abz = crossV3 a b
          Vec3 bax bay baz = crossV3 b a
      in [abx, aby, abz] `shouldBe` [- bax, - bay, - baz]

    it "lengthV3: (3,4,0) = 5" $
      lengthV3 (Vec3 3 4 0) `shouldBe` 5
    it "normalizeV3: (3,0,0) → (1,0,0)" $
      normalizeV3 (Vec3 3 0 0) `shouldBe` Vec3 1 0 0
    it "normalizeV3: zero ベクトルは zero (= 例外無し)" $
      normalizeV3 (Vec3 0 0 0) `shouldBe` Vec3 0 0 0

  describe "Phase 3 A2: Camera3D / Projection3D 型" $ do
    it "Camera3D を構築できる" $
      let c = Camera3D (Point3 0 0 5) (Point3 0 0 0) (Vec3 0 1 0)
      in cameraTarget c `shouldBe` Point3 0 0 0
    it "Orthographic projection を構築できる" $
      let p = Orthographic 1 1 0.1 100
      in orthoFar p `shouldBe` 100
    it "Perspective projection を構築できる" $
      let p = Perspective (pi / 4) 1.5 0.1 100
      in perspAspect p `shouldBe` 1.5

  describe "Phase 3 z-up convention helpers" $ do
    it "zUp = Vec3 0 0 1" $
      zUp `shouldBe` Vec3 0 0 1
    it "defaultCameraZUp dist で up = zUp、 eye の z > 0" $
      let c = defaultCameraZUp 5
          Point3 _ _ ez = cameraEye c
      in do
           cameraUp c `shouldBe` zUp
           (ez > 0) `shouldBe` True
    it "defaultPerspective: fov 45°、 aspect 1.0" $
      case defaultPerspective of
        Perspective fov a _ _ -> do
          abs (fov - pi/4) `shouldSatisfy` (< 1e-9)
          a `shouldBe` 1.0
        _ -> expectationFailure "expected Perspective"

    it "yUp = Vec3 0 1 0 (= OpenGL 慣例)" $
      yUp `shouldBe` Vec3 0 1 0
    it "defaultCameraYUp dist で up = yUp、 eye の y > 0 (= 上方視点)" $
      let c = defaultCameraYUp 5
          Point3 _ ey _ = cameraEye c
      in do
           cameraUp c `shouldBe` yUp
           (ey > 0) `shouldBe` True

    it "z-up と y-up は up vector のみ異なる (= 構造同型)" $
      let cz = defaultCameraZUp 5
          cy = defaultCameraYUp 5
      in do
           cameraTarget cz `shouldBe` cameraTarget cy
           cameraUp cz `shouldNotBe` cameraUp cy

  describe "Phase 3 A3: Mat4 / Matrix 演算" $ do
    it "identityM × identityM = identityM" $
      multM identityM identityM `shouldBe` identityM
    it "identityM × P = P" $
      let p = Point3 2 3 5
      in transformPoint identityM p `shouldBe` p

  describe "Phase 3 A3: viewMatrix (= lookAt)" $ do
    it "camera が eye=(0,0,5)、 target=(0,0,0)、 up=(0,1,0) のとき world 原点は camera 前 5 (= cz = -5)" $
      let cam = Camera3D (Point3 0 0 5) (Point3 0 0 0) (Vec3 0 1 0)
          view = viewMatrix cam
          Point3 _ _ cz = transformPoint view (Point3 0 0 0)
      in abs (cz - (-5)) `shouldSatisfy` (< 1e-9)
    it "camera 前の点 (= world (0,0,0)) は camera space で y=0 / x=0" $
      let cam = Camera3D (Point3 0 0 5) (Point3 0 0 0) (Vec3 0 1 0)
          view = viewMatrix cam
          Point3 cx cy _ = transformPoint view (Point3 0 0 0)
      in (abs cx + abs cy) `shouldSatisfy` (< 1e-9)

  describe "Phase 3 A3: project3D (= 一括投影)" $ do
    let cam = Camera3D (Point3 0 0 5) (Point3 0 0 0) (Vec3 0 1 0)
        ortho = Orthographic 2 2 0.1 100
        persp = Perspective (pi/4) 1.0 0.1 100
        vp = Viewport 0 0 200 200

    it "Orthographic: 原点 (0,0,0) → screen 中央 (100, 100)" $
      let pr = project3D cam ortho vp (Point3 0 0 0)
      in (abs (projX pr - 100) + abs (projY pr - 100)) `shouldSatisfy` (< 1e-9)

    it "Orthographic: 右上 (1, 1, 0) → screen 右上 (= ortho 半幅 2 で 1/2 倍位置)" $
      let pr = project3D cam ortho vp (Point3 1 1 0)
      in do
           -- x: (1/2 + 0.5) * 200 = 150
           abs (projX pr - 150) `shouldSatisfy` (< 1e-9)
           -- y: (1 - (1/2 + 0.5)/1) * 200... 反転で 50
           abs (projY pr - 50) `shouldSatisfy` (< 1e-9)

    it "Perspective: 同じ x,y の point でも z が違うと scale 異なる (= 遠近感)" $
      let p1 = project3D cam persp vp (Point3 1 0 0)
          p2 = project3D cam persp vp (Point3 1 0 (-2))  -- 奥
      in projX p1 `shouldNotBe` projX p2

    it "depth (NDC z): 手前の点は -1 寄り、 奥は +1 寄り" $
      let near = project3D cam persp vp (Point3 0 0 4)   -- camera 近く
          far  = project3D cam persp vp (Point3 0 0 (-4)) -- 奥
      in projDepth near < projDepth far `shouldBe` True

    it "viewport y 反転: world 上 (y > 0) → screen 上 (y < 中央)" $
      let upper = project3D cam ortho vp (Point3 0 1 0)
      in projY upper < 100 `shouldBe` True

  describe "Phase 3 A4: Axes3D + niceTicks3D + renderAxes3D" $ do
    it "niceTicks3D 5 0 10 = [0, 2.5, 5, 7.5, 10]" $
      niceTicks3D 5 0 10 `shouldBe` [0, 2.5, 5, 7.5, 10]
    it "niceTicks3D 1 0 10 = [0] (= n=1 で 1 点)" $
      niceTicks3D 1 0 10 `shouldBe` [0]
    it "defaultAxes3D は単位 cube" $ do
      axesXMin defaultAxes3D `shouldBe` (-1)
      axesXMax defaultAxes3D `shouldBe` 1
    it "renderAxes3D: 12 cube edges + 3軸 tick + 3 軸名 = それなりの数の Primitive" $
      let cam = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 1 0)
          proj = Perspective (pi/4) 1 0.1 100
          vp = Viewport 0 0 400 400
          prims = renderAxes3D cam proj vp defaultAxes3D
      in length prims `shouldSatisfy` (> 15)
    it "renderAxes3D: PLine と PText 両方を含む" $
      let cam = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 1 0)
          proj = Perspective (pi/4) 1 0.1 100
          vp = Viewport 0 0 400 400
          prims = renderAxes3D cam proj vp defaultAxes3D
          nLines = length [() | Graphics.Hgg.Render.PLine{} <- prims]
          nTexts = length [() | Graphics.Hgg.Render.PText{} <- prims]
      in (nLines > 0 && nTexts > 0) `shouldBe` True

  describe "Phase 24 A2: surface z colormap + colorbar" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        -- z が 0..1 へ単調増加する 3x3 grid
        grid = [[0, 0.1, 0.2], [0.3, 0.4, 0.5], [0.6, 0.8, 1.0]]
        fills sf = [ c | Graphics.Hgg.Render.PPath _ (Graphics.Hgg.Render.FillStyle c _) _
                           <- renderSurface3D cam proj vp sf ]
    it "colormap なし (shading off) = 全面同色 (後方互換)" $
      let sf = (defaultSurface3D grid) { sf3Shaded = False }
      in nub (fills sf) `shouldBe` ["#5b9bd5"]
    it "colormap あり (shading off) = z で面色が変わる (低 z ≠ 高 z)" $
      let sf = (defaultSurface3D grid) { sf3Shaded = False
                                       , sf3Colormap = Just viridisStops3D }
          cs = fills sf
      in (length (nub cs) > 3, head cs /= last cs) `shouldBe` (True, True)
    it "layerToSurface: colormap3D が sf3Colormap に渡る" $
      sf3Colormap (layerToSurface (surface3DGrid grid <> colormap3D))
        `shouldBe` Just viridisStops3D
    it "layerToSurface: colormap 未指定は Nothing (従来単色)" $
      sf3Colormap (layerToSurface (surface3DGrid grid)) `shouldBe` Nothing

  describe "Phase 25 A4: surface 透過 (alpha3D 配線)" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        grid = [[0, 0.1], [0.2, 0.3]]
        alphas sf = nub [ a | Graphics.Hgg.Render.PPath _ (Graphics.Hgg.Render.FillStyle _ a) _
                                <- renderSurface3D cam proj vp sf ]
    it "既定 sf3Alpha = 1 (後方互換・全面不透明)" $
      alphas (defaultSurface3D grid) `shouldBe` [1.0]
    it "layerToSurface: alpha3D が sf3Alpha に渡る" $
      sf3Alpha (layerToSurface (surface3DGrid grid <> alpha3D 0.5)) `shouldBe` 0.5
    it "alpha3D 未指定は既定 1 (素通し)" $
      sf3Alpha (layerToSurface (surface3DGrid grid)) `shouldBe` 1.0
    it "renderSurface3D: 面 fill alpha が sf3Alpha を反映する" $
      alphas ((defaultSurface3D grid) { sf3Alpha = 0.4 }) `shouldBe` [0.4]
    it "colorRGBA3D \"#00887766\" == color3D (fromHex \"#008877\") <> alpha3D (0x66/255)" $
      colorRGBA3D "#00887766"
        `shouldBe` (color3D (fromHex "#008877") <> alpha3D (102/255))

  describe "Phase 25 A5: bar3d (直方体 / stick) + 誤差棒" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        tops = [Point3 0 0 0.5, Point3 0.5 0.5 1.0]
    it "bar3DPoints: layerToBar の既定 (BarCuboid・半幅 0.04・base 0)" $ do
      let b = layerToBar (bar3DPoints tops)
      (br3Style b, br3HalfW b, br3BaseZ b) `shouldBe` (BarCuboid, 0.04, 0.0)
    it "barStyle3D / barWidth3D が layerToBar に渡る" $ do
      let b = layerToBar (bar3DPoints tops <> barStyle3D BarStick <> barWidth3D 0.1)
      (br3Style b, br3HalfW b) `shouldBe` (BarStick, 0.1)
    it "barFacesDepth: cuboid は棒あたり 5 quad (PPath)" $
      let prims = barFacesDepth cam proj vp (defaultBar3D tops)
      in length prims `shouldBe` 5 * length tops
    it "barFacesDepth: stick は face を出さない ([])" $
      barFacesDepth cam proj vp ((defaultBar3D tops) { br3Style = BarStick }) `shouldBe` []
    it "renderBarSticks: stick は棒あたり 1 PLine" $
      let prims = renderBarSticks cam proj vp ((defaultBar3D tops) { br3Style = BarStick })
      in length prims `shouldBe` length tops
    it "renderBarSticks: cuboid 時は線を出さない ([])" $
      renderBarSticks cam proj vp (defaultBar3D tops) `shouldBe` []
    it "renderErrorBars3D: 点あたり 3 PLine (whisker + 上下キャップ)・err<=0 はスキップ" $
      let prims = renderErrorBars3D cam proj vp "#333" 1.0
                    [(Point3 0 0 0.5, 0.2), (Point3 0.5 0.5 1.0, 0)]
      in length prims `shouldBe` 3   -- 2 点目 (err=0) はスキップ

  describe "Phase 25 A5: bar/err の正規化パイプライン (|>> + normLayer3D)" $ do
    let ax = Axes3D { axesXMin = 0, axesXMax = 2, axesYMin = 0, axesYMax = 2
                    , axesZMin = 0, axesZMax = 10, axesNTicks = 5
                    , axesXLog = False, axesYLog = False, axesZLog = False }
        df = [ ("x", NumData (V.fromList [0, 1, 2]))
             , ("y", NumData (V.fromList [0, 1, 2]))
             , ("h", NumData (V.fromList [0, 5, 10]))
             , ("se", NumData (V.fromList [1, 1, 1])) ] :: [(T.Text, ColData)]
        spec = layer3D (bar3D "x" "y" "h" <> errorBar3D "se")
        (_, resolved) = unBound3D (df |>> spec)
        l0 = head (vs3Layers resolved)
        l  = normLayer3D ax l0
    it "bar 底面 z = data 0 の正規化値 (zMin=0 → -1)" $
      getLast (lyr3BarBaseZ l) `shouldBe` Just (-1.0)
    it "err は z 量として正規化される (data 1 / z-span 10 → 0.2)" $
      getLast (lyr3PtErrs l) `shouldBe` Just [0.2, 0.2, 0.2]
    it "renderSpec3DInPanel: bar 直方体 (PPath) + 誤差棒 (PLine) が出る" $
      let prims = renderSpec3DInPanel 0 0 400 400 (resolved <> camera (defaultCameraZUp 4))
          paths = [ () | Graphics.Hgg.Render.PPath{} <- prims ]
          lines = [ () | Graphics.Hgg.Render.PLine{} <- prims ]
      in (length paths >= 15, length lines >= 9) `shouldBe` (True, True)

  describe "Phase 26 A4: 3D stem (lollipop)" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        tops = [Point3 0 0 0.5, Point3 0.5 0.5 1.0]
    it "stem3D / stem3DPoints は M3Stem kind を立てる" $ do
      lyr3Kind (stem3DPoints tops) `shouldBe` First (Just M3Stem)
      lyr3Kind (stem3D "x" "y" "z") `shouldBe` First (Just M3Stem)
    it "renderStems3D: stem あたり 1 PLine + 1 PCircle (= 2 prim)" $
      let prims = renderStems3D cam proj vp (defaultBar3D tops) 5
      in length prims `shouldBe` 2 * length tops
    it "stemBaseZ は data 空間で正規化され lyr3BarBaseZ に落ちる (zMin=0,zMax=10,base=2 → -0.6)" $
      let ax = Axes3D { axesXMin = 0, axesXMax = 2, axesYMin = 0, axesYMax = 2
                      , axesZMin = 0, axesZMax = 10, axesNTicks = 5
                      , axesXLog = False, axesYLog = False, axesZLog = False }
          df = [ ("x", NumData (V.fromList [0, 1, 2]))
               , ("y", NumData (V.fromList [0, 1, 2]))
               , ("z", NumData (V.fromList [3, 5, 8])) ] :: [(T.Text, ColData)]
          spec = layer3D (stem3D "x" "y" "z" <> stemBaseZ 2)
          (_, resolved) = unBound3D (df |>> spec)
          l = normLayer3D ax (head (vs3Layers resolved))
      in getLast (lyr3BarBaseZ l) `shouldBe` Just (-0.6)
    it "layerPoints: stem は底面 (stemBaseZ) を axis box に含める" $
      -- tops 2 点 + bases 2 点 (z=base) = 4 点 (err 無し)
      let l = stem3DPoints tops <> stemBaseZ 0
      in length (layerPoints l) `shouldBe` 4

  describe "Phase 26 A3: 3D vector field (quiver3D)" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        items = [ (Point3 0 0 0, Vec3 1 0 0), (Point3 1 1 1, Vec3 0 0 2) ]
    it "quiver3D は M3Quiver kind + 位置/ベクトルを格納" $
      lyr3Kind (quiver3D items) `shouldBe` First (Just M3Quiver)
    it "layerToQuiver: 始点 N 個・終点 N 個 (autoscale で終点 = 始点 + scaled vec)" $
      let q = layerToQuiver (quiver3D items)
      in (length (q3Starts q), length (q3Ends q)) `shouldBe` (2, 2)
    it "layerToQuiver: 最長ベクトルの矢印長 = cube の 0.35 (autoscale)" $
      -- items の最長 vec は (0,0,2) で長さ 2。 autoS = 0.35/2。 終点 z = 1 + 0.35。
      let q = layerToQuiver (quiver3D items)
          Point3 _ _ ez = q3Ends q !! 1
      in abs (ez - (1 + 0.35)) < 1e-9 `shouldBe` True
    it "renderQuiver3D: 矢印 1 本につき 3 PLine (本線 + 矢じり 2)" $
      let q = defaultQuiver3D [Point3 0 0 0, Point3 1 0 0]
                              [Point3 0.3 0 0, Point3 1 0.3 0]
          prims = renderQuiver3D cam proj vp q
      in length prims `shouldBe` 3 * 2

  describe "Phase 26 A5: trisurf (Delaunay 三角分割)" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
    it "delaunay2D: 3 点未満は [] (三角形を作れない)" $ do
      delaunay2D [] `shouldBe` []
      delaunay2D [(0,0),(1,0)] `shouldBe` []
    it "delaunay2D: 正方形 4 隅 → 2 三角形" $
      length (delaunay2D [(0,0),(1,0),(1,1),(0,1)]) `shouldBe` 2
    it "delaunay2D: 正方形 + 中心 (5 点) → 4 三角形" $
      length (delaunay2D [(0,0),(1,0),(1,1),(0,1),(0.5,0.5)]) `shouldBe` 4
    it "delaunay2D: 全 index が元の点数未満 (super-triangle 由来は除去)" $
      let n = 5
          tris = delaunay2D [(0,0),(1,0),(1,1),(0,1),(0.5,0.5)]
      in all (\(a,b,c) -> all (< n) [a,b,c] && all (>= 0) [a,b,c]) tris
           `shouldBe` True
    it "trisurf: M3Trisurf kind を立てる" $
      lyr3Kind (trisurf [Point3 0 0 0]) `shouldBe` First (Just M3Trisurf)
    it "trianglesFacesDepth: 三角形 1 つにつき 1 PPath" $
      let tris = [ (Point3 0 0 0, Point3 1 0 0, Point3 0 1 1)
                 , (Point3 1 0 0, Point3 1 1 0, Point3 0 1 1) ]
          prims = trianglesFacesDepth cam proj vp "#5b9bd5" "#333" True Nothing 1.0 tris
      in length prims `shouldBe` 2

  describe "Phase 25 A6: 壁面 pane + gridline (G5)" $ do
    let cam  = Camera3D (Point3 3 (-3) 2) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        ax   = defaultAxes3D
    it "renderAxes3DPanes: 奥壁 3 面 (PPath) + tick 格子線 (PLine) を出す" $
      let prims = renderAxes3DPanes id defaultPaneStyle3D cam proj vp ax
          paths = [ () | Graphics.Hgg.Render.PPath{}  <- prims ]
          lines = [ () | Graphics.Hgg.Render.PLine{}  <- prims ]
      in (length paths, length lines >= 6) `shouldBe` (3, True)
    it "renderAxes3DPanes: pane 塗りは defaultPaneStyle3D の色" $
      let prims = renderAxes3DPanes id defaultPaneStyle3D cam proj vp ax
          fills = nub [ c | Graphics.Hgg.Render.PPath _ (Graphics.Hgg.Render.FillStyle c _) _ <- prims ]
      in fills `shouldBe` [paneFill defaultPaneStyle3D]
    it "renderSpec3DInPanel: 既定 (pane ON) は pane OFF より PPath/PLine が多い" $
      let base = layer3D (scatter3DPoints [Point3 0 0 0, Point3 1 1 1])
                   <> camera cam
          onP  = renderSpec3DInPanel 0 0 400 400 base
          offP = renderSpec3DInPanel 0 0 400 400 (base <> pane3D False)
          npath p = length [ () | Graphics.Hgg.Render.PPath{} <- p ]
          nline p = length [ () | Graphics.Hgg.Render.PLine{} <- p ]
      in (npath onP > npath offP, nline onP > nline offP) `shouldBe` (True, True)

  describe "Phase 25 A7 / 30 A6: 3D テキスト注釈 (text3DPoints / annotate3D)" $ do
    let cam = Camera3D (Point3 3 (-3) 2) (Point3 0 0 0) (Vec3 0 0 1)
    it "text3DPoints: 各点ごとに PText が出る (ラベル文字列を含む)" $
      let spec = layer3D (text3DPoints [(Point3 0 0 1, "peak"), (Point3 1 1 0, "corner")])
                   <> camera cam
          prims = renderSpec3DInPanel 0 0 400 400 spec
          texts = [ t | Graphics.Hgg.Render.PText _ t _ <- prims ]
      in (("peak" `elem` texts), ("corner" `elem` texts)) `shouldBe` (True, True)
    it "annotate3D: 単一ラベルは text3DPoints [(p,t)] と同値" $
      annotate3D (Point3 0 0 1) "x" `shouldBe` text3DPoints [(Point3 0 0 1, "x")]
    it "Phase 30 A6: annotate3D は <> で畳める (複数注釈が累積)" $
      let spec = layer3D (annotate3D (Point3 0 0 1) "A"
                            <> annotate3D (Point3 1 1 0) "B"
                            <> annotate3D (Point3 (-1) (-1) 0) "C") <> camera cam
          prims = renderSpec3DInPanel 0 0 400 400 spec
          texts = [ t | Graphics.Hgg.Render.PText _ t _ <- prims ]
      in (all (`elem` texts) ["A", "B", "C"]) `shouldBe` True
    it "color3D/size3D で文字色・サイズを上書きできる" $
      let spec = layer3D (annotate3D (Point3 0 0 0) "hi"
                            <> color3D (fromHex "#d62728") <> size3D 15) <> camera cam
          prims = renderSpec3DInPanel 0 0 400 400 spec
          sty   = [ (c, s) | Graphics.Hgg.Render.PText _ "hi"
                               (Graphics.Hgg.Render.TextStyle c s _ _ _ _ _) <- prims ]
      in sty `shouldBe` [("#d62728", 15)]

  describe "Phase 25 A8: log 軸 + x/y アスペクト (G7)" $ do
    let axZLog = defaultAxes3D { axesZMin = 1, axesZMax = 100, axesZLog = True }
    it "logTicks3D 1 1000 = 10 の冪 [1,10,100,1000]" $
      logTicks3D 1 1000 `shouldBe` [1, 10, 100, 1000]
    it "normPoint3D (log z): 幾何平均 z=10 (1..100) は中央 0 に写る" $
      let Point3 _ _ z = normPoint3D axZLog (Point3 0 0 10)
      in abs z `shouldSatisfy` (< 1e-9)
    it "normPoint3D (log z): 端 z=1 → -1、 z=100 → +1" $ do
      let Point3 _ _ zlo = normPoint3D axZLog (Point3 0 0 1)
          Point3 _ _ zhi = normPoint3D axZLog (Point3 0 0 100)
      abs (zlo + 1) `shouldSatisfy` (< 1e-9)
      abs (zhi - 1) `shouldSatisfy` (< 1e-9)
    it "logScale3D は vs3Log を設定する" $
      getLast (vs3Log (logScale3D False False True)) `shouldBe` Just (False, False, True)
    it "xAspect3D 2 は軸 box を x 方向に広げる (PLine の x 範囲が拡大)" $
      let base = layer3D (scatter3DPoints [Point3 (-1) (-1) (-1), Point3 1 1 1])
                   <> camera (defaultCameraZUp 4)
          xspan p = let xs = [ x | Graphics.Hgg.Render.PLine (Graphics.Hgg.Render.Point x _) _ _ <- p ]
                             ++ [ x | Graphics.Hgg.Render.PLine _ (Graphics.Hgg.Render.Point x _) _ <- p ]
                    in maximum xs - minimum xs
          p1 = renderSpec3DInPanel 0 0 400 400 base
          p2 = renderSpec3DInPanel 0 0 400 400 (base <> xAspect3D 2)
      in (xspan p2 > xspan p1) `shouldBe` True

  describe "Phase 24 A3: 正規化 pipeline (実スケールデータ → [-1,1]^3)" $ do
    let ax = Axes3D { axesXMin = 0, axesXMax = 4, axesYMin = 0, axesYMax = 6
                    , axesZMin = 50, axesZMax = 80, axesNTicks = 5
                    , axesXLog = False, axesYLog = False, axesZLog = False }
    it "normPoint3D: bbox の min 角 → (-1,-1,-1)、 max 角 → (1,1,1)" $ do
      normPoint3D ax (Point3 0 0 50) `shouldBe` Point3 (-1) (-1) (-1)
      normPoint3D ax (Point3 4 6 80) `shouldBe` Point3 1 1 1
    it "normPoint3D: 中心 → 原点" $
      normPoint3D ax (Point3 2 3 65) `shouldBe` Point3 0 0 0
    it "padAxes3D: 退化軸 (min = max) を ±0.5 に広げる" $ do
      let d = padAxes3D ax { axesZMin = 7, axesZMax = 7 }
      (axesZMin d, axesZMax d) `shouldBe` (6.5, 7.5)
      (axesXMin d, axesXMax d) `shouldBe` (0, 4)   -- 非退化軸は不変
    it "normLayer3D: surface の grid z が [-1,1]・x/y range が (-1,1) になる" $ do
      let l  = surface3DGrid [[50, 65], [65, 80]]
                 <> xRange3D (0, 4) <> yRange3D (0, 6)
          l' = normLayer3D ax l
      getLast (lyr3XRange l') `shouldBe` Just (-1, 1)
      getLast (lyr3YRange l') `shouldBe` Just (-1, 1)
      getLast (lyr3Grid l') `shouldBe` Just [[-1, 0], [0, 1]]

  describe "Phase 24 A5 / #2: 投影 contour (床 = 等値面 / 壁 = 断面)" $ do
    -- eye = (3,-3,3) → far wall は x=-1 / y=+1 / z=-1 (= 自動固定先)
    let cam  = Camera3D (Point3 3 (-3) 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
        -- 正規化済を想定した grid (z ∈ [-1,1]、 x/y range = (-1,1))
        grid = [ [ fromIntegral (i + j) / 4 - 1 | i <- [0 .. 4 :: Int] ]
               | j <- [0 .. 4 :: Int] ]
        surfC c = surface3DGrid grid <> xRange3D (-1, 1) <> yRange3D (-1, 1) <> c
        render l = renderProjectedContour3D (1, 1, 1) cam proj vp l
        nLines l = length [ () | Graphics.Hgg.Render.PLine{} <- render l ]
    it "contour 未指定 = 線なし (OFF)" $
      render (surface3DGrid grid <> xRange3D (-1,1) <> yRange3D (-1,1)) `shouldBe` []
    it "contourZ (床等値面) = PLine 群を描く" $
      (nLines (surfC (contourZ 8)) > 0) `shouldBe` True
    it "contourX / contourY (壁断面) = PLine 群を描く" $ do
      (nLines (surfC (contourX 8)) > 0) `shouldBe` True
      (nLines (surfC (contourY 8)) > 0) `shouldBe` True
    it "本数を増やすと線分が増える (単調・全 dir)" $ do
      (nLines (surfC (contourZ 4)) <= nLines (surfC (contourZ 12))) `shouldBe` True
      (nLines (surfC (contourX 2)) <= nLines (surfC (contourX 9))) `shouldBe` True
    it "contourX/Y/Z は (dir, 本数) を lyr3Contours に積む" $ do
      lyr3Contours (contourZ 8) `shouldBe` [(ContourZ, 8)]
      lyr3Contours (contourX 5) `shouldBe` [(ContourX, 5)]
    it "1 layer に contourX <> contourY <> contourZ を合成できる (3 件)" $
      lyr3Contours (contourX 8 <> contourY 6 <> contourZ 4)
        `shouldBe` [(ContourX, 8), (ContourY, 6), (ContourZ, 4)]
    it "合成した 3 dir は単一 layer から全て描かれる (各 dir 単独の和以上)" $
      let n3 = nLines (surfC (contourX 8 <> contourY 8 <> contourZ 8))
      in (n3 >= nLines (surfC (contourX 8))) `shouldBe` True

  describe "Phase 24 A7: 群別タイル配置 (facet)" $ do
    let spec = layer3D (scatter3DPoints [Point3 0 0 0, Point3 1 1 1])
        xsOf p = [ x | Graphics.Hgg.Render.PLine (Graphics.Hgg.Render.Point x1 _)
                                                 (Graphics.Hgg.Render.Point x2 _) _
                         <- renderSpec3DInPanel p 0 400 400 spec, x <- [x1, x2] ]
        ysOf p = [ y | Graphics.Hgg.Render.PLine (Graphics.Hgg.Render.Point _ y1)
                                                 (Graphics.Hgg.Render.Point _ y2) _
                         <- renderSpec3DInPanel p 0 400 400 spec, y <- [y1, y2] ]
    it "renderSpec3DInPanel: パネルを px ずらすと全 PLine x が px 平行移動 (y 不変)" $
      let dxs = zipWith (-) (xsOf 300) (xsOf 0)
          dys = zipWith (-) (ysOf 300) (ysOf 0)
      in ( all (\d -> abs (d - 300) < 1e-6) dxs
         , all (\d -> abs d < 1e-6) dys
         , not (null dxs) )
        `shouldBe` (True, True, True)
    it "renderSpec3DInPanel: 軸 box の PLine が出る (非空)" $
      (length (xsOf 0) > 0) `shouldBe` True
    it "saveSVG3DFacet: 2 群を書き出し各群ラベルが SVG に入る" $ do
      let panels = [ ("GrpA", layer3D (scatter3DPoints [Point3 0 0 0]))
                   , ("GrpB", layer3D (scatter3DPoints [Point3 1 1 1])) ]
      saveSVG3DFacet "/tmp/hgg-facet-test.svg" panels
      svg <- readFile "/tmp/hgg-facet-test.svg"
      (("GrpA" `isInfixOf` svg) && ("GrpB" `isInfixOf` svg)) `shouldBe` True
    it "saveSVG3DFacet: 空リストは no-op (例外なし)" $
      saveSVG3DFacet "/tmp/hgg-facet-empty.svg" [] `shouldReturn` ()

  describe "Phase 24 A8: 視点 preset" $ do
    it "cameraTop: eye が +z 真上・target 原点" $
      let c = cameraTop 5; Point3 ex ey ez = cameraEye c
      in (ex, ey, ez > 0) `shouldBe` (0, 0, True)
    it "cameraFront: eye が -y 方向" $
      let Point3 ex ey ez = cameraEye (cameraFront 5)
      in (ex, ey < 0, ez) `shouldBe` (0, True, 0)
    it "cameraSide: eye が +x 方向" $
      let Point3 ex ey ez = cameraEye (cameraSide 5)
      in (ex > 0, ey, ez) `shouldBe` (True, 0, 0)
    it "cameraIso = defaultCameraZUp (同一)" $
      cameraEye (cameraIso 5) `shouldBe` cameraEye (defaultCameraZUp 5)

  describe "Phase 24 A8: 軸タイトル" $ do
    let cam  = Camera3D (Point3 3 3 3) (Point3 0 0 0) (Vec3 0 0 1)
        proj = Perspective (pi/4) 1 0.1 100
        vp   = Viewport 0 0 400 400
    it "renderAxes3DWithLabels: 指定した軸名が PText に出る" $
      let prims = renderAxes3DWithLabels id ("深さ", "幅", "高さ") cam proj vp defaultAxes3D
          texts = [ s | Graphics.Hgg.Render.PText _ s _ <- prims ]
      in (("深さ" `elem` texts), ("幅" `elem` texts), ("高さ" `elem` texts))
           `shouldBe` (True, True, True)
    it "renderAxes3DWith (= ラベル無版) は \"x\"/\"y\"/\"z\" (後方互換)" $
      let prims = renderAxes3D cam proj vp defaultAxes3D
          texts = [ s | Graphics.Hgg.Render.PText _ s _ <- prims ]
      in (("x" `elem` texts), ("y" `elem` texts), ("z" `elem` texts))
           `shouldBe` (True, True, True)

  describe "Phase 24 A8: z aspect box" $ do
    let surf = layer3D (surface3DGrid [[0, 0.5], [0.5, 1]] <> xRange3D (-1,1) <> yRange3D (-1,1))
                 <> camera (defaultCameraZUp 4)
        ysOf za = [ y | Graphics.Hgg.Render.PPath segs _ _
                          <- renderSpec3DInPanel 0 0 400 400 (surf <> zAspect3D za)
                      , Graphics.Hgg.Render.MoveTo (Graphics.Hgg.Render.Point _ y) <- segs ]
    it "zAspect 1 と 2 で surface の投影 y が変わる (= z スケール反映)" $
      (ysOf 1 /= ysOf 2) `shouldBe` True
    it "zAspect3D 既定 (未指定) = aspect 1 と同一出力" $
      renderSpec3DInPanel 0 0 400 400 surf
        `shouldBe` renderSpec3DInPanel 0 0 400 400 (surf <> zAspect3D 1)

  describe "Phase 24 A8: 層間 depth 統合" $ do
    let cam   = defaultCameraZUp 4
        -- z が -1..1 に傾いた平面 (= 面の depth に幅がある)
        tilt  = [ [ fromIntegral (i + j) / 2 - 1 | i <- [0 .. 2 :: Int] ]
                | j <- [0 .. 2 :: Int] ]
        spec  = layer3D (surface3DGrid tilt <> xRange3D (-1, 1) <> yRange3D (-1, 1))
             <> layer3D (scatter3DPoints [Point3 0 0 0])   -- 中央 (= 中間 depth)
             <> camera cam
        prims = renderSpec3DInPanel 0 0 400 400 spec
        tagged = zip [0 :: Int ..] prims
        circleIdx = [ i | (i, Graphics.Hgg.Render.PCircle{}) <- tagged ]
        pathIdx   = [ i | (i, Graphics.Hgg.Render.PPath{})   <- tagged ]
    it "surface 面と scatter 点が depth で interleave する (点が全部最前面でない)" $
      let ci = head circleIdx
      in ( not (null circleIdx)
         , any (< ci) pathIdx    -- 円より奥の面が前にある
         , any (> ci) pathIdx )  -- 円より手前の面が後にある
        `shouldBe` (True, True, True)

  describe "Phase 24 A8: 3D PDF/PNG glue" $ do
    let spec = layer3D (surface3DGrid [[0,0.5],[0.5,1]] <> colormap3D
                          <> xRange3D (-1,1) <> yRange3D (-1,1))
                 <> camera (defaultCameraZUp 4) <> width3DV 300 <> height3DV 300
                 <> title3D "test"
        nonEmpty path = do exists <- doesFileExist path
                           sz <- if exists then getFileSize path else pure 0
                           when exists (removeFile path)
                           pure (sz > 0)
    it "savePDF3D: 非空 PDF を書き出す" $ do
      savePDF3D "/tmp/hgg-3d-test.pdf" spec
      nonEmpty "/tmp/hgg-3d-test.pdf" `shouldReturn` True
    it "savePNG3D: 非空 PNG を書き出す" $ do
      savePNG3D "/tmp/hgg-3d-test.png" spec
      nonEmpty "/tmp/hgg-3d-test.png" `shouldReturn` True

  describe "Phase 25 A2: 群色分け (colorBy3D) + 離散凡例" $ do
    let hue = ggplotHue 2
        df = [ ("x", NumData (V.fromList [1, 2, 3]))
             , ("y", NumData (V.fromList [4, 5, 6]))
             , ("z", NumData (V.fromList [7, 8, 9]))
             , ("group", TxtData (V.fromList ["A", "A", "B"])) ] :: [(T.Text, ColData)]
        spec = layer3D (scatter3D "x" "y" "z" <> colorBy3D "group")
        (_, resolved) = unBound3D (df |>> spec)
        l = head (vs3Layers resolved)
    it "カテゴリ列 → 点ごと色 (ggplotHue・初出順 A,A,B)" $
      getLast (lyr3PtColors l) `shouldBe` Just [hue !! 0, hue !! 0, hue !! 1]
    it "凡例 = (カテゴリ, 色) を初出順で" $
      getLast (lyr3Legend l) `shouldBe` Just [("A", hue !! 0), ("B", hue !! 1)]
    it "renderSpec3DInPanel: 凡例チップ (PRect) が出て scatter 点色が 2 種" $
      let prims = renderSpec3DInPanel 0 0 400 400 (resolved <> camera (defaultCameraZUp 4))
          rects = [ () | Graphics.Hgg.Render.PRect{} <- prims ]
          cols  = nub [ c | Graphics.Hgg.Render.PCircle _ _ (Graphics.Hgg.Render.FillStyle c _) _ _
                              <- prims ]
      in (length rects >= 2, length cols) `shouldBe` (True, 2)
    it "sc3Colors: layerToScatter が per-point 色を渡す" $
      sc3Colors (layerToScatter l) `shouldBe` Just [hue !! 0, hue !! 0, hue !! 1]

  describe "Phase 25 A3: 連続色マップ (colorContinuousBy3D) + colorbar" $ do
    let df = [ ("x", NumData (V.fromList [1, 2, 3]))
             , ("y", NumData (V.fromList [4, 5, 6]))
             , ("z", NumData (V.fromList [7, 8, 9]))
             , ("v", NumData (V.fromList [0, 5, 10])) ] :: [(T.Text, ColData)]
        spec = layer3D (scatter3D "x" "y" "z" <> colorContinuousBy3D "v")
        (_, resolved) = unBound3D (df |>> spec)
        l = head (vs3Layers resolved)
    it "数値列 → 点ごと連続色 (viridis・min/mid/max)" $
      getLast (lyr3PtColors l)
        `shouldBe` Just [ continuousColor viridisStops3D 0
                        , continuousColor viridisStops3D 0.5
                        , continuousColor viridisStops3D 1 ]
    it "colorbar 情報 = (viridis stops, min, max)" $
      getLast (lyr3Colorbar l) `shouldBe` Just (viridisStops3D, 0, 10)
    it "sc3Colors: layerToScatter が per-point 連続色を渡す" $
      sc3Colors (layerToScatter l)
        `shouldBe` Just [ continuousColor viridisStops3D 0
                        , continuousColor viridisStops3D 0.5
                        , continuousColor viridisStops3D 1 ]
    it "renderSpec3DInPanel: colorbar strip (PRect) が出る" $
      let prims = renderSpec3DInPanel 0 0 400 400 (resolved <> camera (defaultCameraZUp 4))
          rects = [ () | Graphics.Hgg.Render.PRect{} <- prims ]
      in length rects >= 10 `shouldBe` True

  describe "Phase 30 A6: surface 案C (列駆動 surface3D・long → grid pivot)" $ do
    -- 2x2 規則格子を long 形で: (x,y,z) = (0,0,10)(1,0,20)(0,1,30)(1,1,40)
    let df = [ ("x", NumData (V.fromList [0, 1, 0, 1]))
             , ("y", NumData (V.fromList [0, 0, 1, 1]))
             , ("z", NumData (V.fromList [10, 20, 30, 40])) ] :: [(T.Text, ColData)]
        spec = layer3D (surface3D "x" "y" "z")
        (_, resolved) = unBound3D (df |>> spec)
        l = head (vs3Layers resolved)
    it "long (x,y,z) を pivot して z 行列に畳む (列=x昇順・行=y昇順)" $
      getLast (lyr3Grid l) `shouldBe` Just [[10, 20], [30, 40]]
    it "xRange/yRange = データの min/max" $
      (getLast (lyr3XRange l), getLast (lyr3YRange l))
        `shouldBe` (Just (0, 1), Just (0, 1))
    it "surface3DGrid (行列直入れ・旧 API) は lyr3Grid に直入れ (後方互換)" $
      getLast (lyr3Grid (surface3DGrid [[10,20],[30,40]]))
        `shouldBe` Just [[10, 20], [30, 40]]

  describe "Phase 30 A6: 列駆動 text3D (x/y/z/label列 → lyr3Points + lyr3Labels)" $ do
    let df = [ ("x", NumData (V.fromList [0, 1]))
             , ("y", NumData (V.fromList [0, 1]))
             , ("z", NumData (V.fromList [0, 1]))
             , ("name", TxtData (V.fromList ["a", "b"])) ] :: [(T.Text, ColData)]
        spec = layer3D (text3D "x" "y" "z" "name")
        (_, resolved) = unBound3D (df |>> spec)
        l = head (vs3Layers resolved)
    it "x/y/z → lyr3Points" $
      getLast (lyr3Points l) `shouldBe` Just [Point3 0 0 0, Point3 1 1 1]
    it "label 列 → lyr3Labels" $
      getLast (lyr3Labels l) `shouldBe` Just ["a", "b"]

  describe "Phase 25 A3: size マップ (sizeBy3D / sizeRange3D)" $ do
    let df = [ ("x", NumData (V.fromList [1, 2, 3]))
             , ("y", NumData (V.fromList [4, 5, 6]))
             , ("z", NumData (V.fromList [7, 8, 9]))
             , ("m", NumData (V.fromList [0, 5, 10])) ] :: [(T.Text, ColData)]
    it "数値列 → 点ごと size (既定範囲 (4,18)・線形)" $
      let spec = layer3D (scatter3D "x" "y" "z" <> sizeBy3D "m")
          (_, resolved) = unBound3D (df |>> spec)
          l = head (vs3Layers resolved)
      in getLast (lyr3PtSizes l) `shouldBe` Just [4, 11, 18]
    it "sizeRange3D で px 範囲を明示指定" $
      let spec = layer3D (scatter3D "x" "y" "z" <> sizeBy3D "m" <> sizeRange3D (2, 22))
          (_, resolved) = unBound3D (df |>> spec)
          l = head (vs3Layers resolved)
      in getLast (lyr3PtSizes l) `shouldBe` Just [2, 12, 22]
    it "sc3Sizes: layerToScatter が per-point size を渡す" $
      let spec = layer3D (scatter3D "x" "y" "z" <> sizeBy3D "m")
          (_, resolved) = unBound3D (df |>> spec)
          l = head (vs3Layers resolved)
      in sc3Sizes (layerToScatter l) `shouldBe` Just [4, 11, 18]

  describe "Phase 24 A6: 3D 列名バインド (ColRef + |>>)" $ do
    let df = [ ("x", NumData (V.fromList [1, 2, 3]))
             , ("y", NumData (V.fromList [4, 5, 6]))
             , ("z", NumData (V.fromList [7, 8, 9])) ] :: [(T.Text, ColData)]
        spec3 = layer3D (scatter3D "x" "y" "z")
    it "df |>> spec3d → BoundPlot3D・unBound3D で列が [Point3] に解決される" $ do
      let b = df |>> spec3
          (_, resolved) = unBound3D b
      bp3Diagnostics b `shouldBe` []
      getLast (lyr3Points (head (vs3Layers resolved)))
        `shouldBe` Just [Point3 1 4 7, Point3 2 5 8, Point3 3 6 9]
    it "存在しない列は ColumnNotFound (編集距離 suggestion 付き)" $ do
      let b = df |>> layer3D (scatter3D "x" "y" "zz")
      length (bp3Diagnostics b) `shouldBe` 1
      case head (bp3Diagnostics b) of
        Graphics.Hgg.Validate.PlotError
          (Graphics.Hgg.Validate.ColumnNotFound nm sugg) _ -> do
            nm `shouldBe` "zz"
            head sugg `shouldBe` "z"   -- 最近傍が先頭 (他候補も距離 2 以内で続く)
        d -> expectationFailure ("ColumnNotFound でない: " <> show d)
    it "inline 生値は resolver 不要で解決 (2D と同じ書き味)" $ do
      let l  = scatter3D (inline [1, 2 :: Double]) (inline [3, 4 :: Double])
                         (inline [5, 6 :: Double])
          l' = resolveLayer3D emptyResolver l
      getLast (lyr3Points l') `shouldBe` Just [Point3 1 3 5, Point3 2 4 6]
    it "scatter3DPoints (旧直入れ) は素通し (後方互換)" $ do
      let l  = scatter3DPoints [Point3 9 9 9]
          l' = resolveLayer3D emptyResolver l
      getLast (lyr3Points l') `shouldBe` Just [Point3 9 9 9]
