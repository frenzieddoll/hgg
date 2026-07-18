-- |
-- Module      : Graphics.Hgg.ThreeD.Easy
-- Description : 3D 出力経路の薄い wrap (Phase 5 A6 saveSVG3D)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 'VisualSpec3D' から各出力経路への dispatch helper。 3 経路:
--
--   * 'saveSVG3D'   ─ Phase 3 CPU projection (= 静的 SVG 出力、 本 module 実装)
--   * 'saveHTML3D'  ─ Phase 4 WebGL bundle 埋込 HTML (= 'Graphics.Hgg.ThreeD.Browser')
--   * 'showBrowser' ─ tmp HTML + xdg-open (= 'Graphics.Hgg.ThreeD.Browser')
--
-- 同 spec を 3 経路に出し分けられる。 用途で使い分け:
--
--   * 印刷 / doc 埋込 → 'saveSVG3D'
--   * 配布 / 単一 HTML 配布 → 'saveHTML3D'
--   * 開発時 interactive 確認 → 'showBrowser'
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.ThreeD.Easy
  ( saveSVG3D
    -- * PDF / PNG 出力 (Phase 24 A8 glue)
  , savePDF3D
  , savePNG3D
  , spec3DToPrimitives
    -- * 群別タイル配置 (Phase 24 A7)
  , saveSVG3DFacet
  , renderSpec3DInPanel
    -- * 正規化 pipeline (Phase 24 A3・test 用に公開)
  , padAxes3D
  , normPoint3D
  , normLayer3D
    -- * 投影 contour (Phase 24 A5・test 用に公開)
  , renderProjectedContour3D
  ) where

import           Data.List                        (nub, sortOn)
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Data.Monoid                      (First (..), Last (..))
import           Data.Text                        (Text)
import qualified Data.Text                        as T

import           Graphics.Hgg.Backend.SVG         (savePrimitivesSVG)
import           Graphics.Hgg.Backend.PDF         (savePrimitivesPDF)
import           Graphics.Hgg.Backend.Rasterific  (savePrimitivesPNG, defaultPNGConfig)
import           Graphics.Hgg.Render              (FillStyle (..), Point (..),
                                                   Primitive (..), solid,
                                                   StrokeStyle (..),
                                                   TextAnchor (..),
                                                   TextStyle (..))
import           Graphics.Hgg.Render.Common       (continuousColor)
import           Graphics.Hgg.Math.Griddata       (innerLevels, marchingSegments)
import           Graphics.Hgg.Layout              (Rect (..))

import           Graphics.Hgg.ThreeD.Axes         (Axes3D (..),
                                                    defaultPaneStyle3D,
                                                    renderAxes3DPanes,
                                                    renderAxes3DWithLabels)
import           Graphics.Hgg.ThreeD.Line         (renderLine3D, renderWireframe3D,
                                                    renderQuiver3D)
import           Graphics.Hgg.ThreeD.Projection   (Projected (..), Viewport (..),
                                                    project3D)
import           Graphics.Hgg.ThreeD.Scatter      (scatterPointsDepth)
import           Graphics.Hgg.ThreeD.Spec         (Layer3D, Mark3DKind (..),
                                                    VisualSpec3D (..),
                                                    autoAxes3D, layerToLine,
                                                    layerToScatter, layerToQuiver,
                                                    layerToSurface, layerToBar,
                                                    layerToWireframe, lyr3Kind,
                                                    lyr3Color, lyr3Size,
                                                    lyr3Colormap, lyr3Colorbar,
                                                    lyr3Contours,
                                                    ContourDir (..),
                                                    lyr3Grid, lyr3Labels,
                                                    lyr3Legend,
                                                    lyr3Points, lyr3Annots,
                                                    lyr3XRange,
                                                    lyr3YRange, lyr3BarBaseZ,
                                                    lyr3Width, lyr3StemBaseZ,
                                                    lyr3Vectors, lyr3VecScale,
                                                    lyr3EdgeColor, lyr3Shaded,
                                                    lyr3Alpha,
                                                    lyr3PtErrs, viridisStops3D,
                                                    vs3Axes, vs3AxisTitles,
                                                    vs3Camera, vs3Height,
                                                    vs3Layers, vs3Log, vs3Pane,
                                                    vs3Proj, vs3Title,
                                                    vs3Width, vs3XAspect,
                                                    vs3YAspect, vs3ZAspect)
import qualified Data.Vector                      as V
import           Graphics.Hgg.ThreeD.Surface      (surfaceFacesDepth,
                                                    trianglesFacesDepth)
import           Graphics.Hgg.ThreeD.Delaunay     (delaunay2D)
import           Graphics.Hgg.ThreeD.Bar          (Bar3D (..), barFacesDepth,
                                                    renderBarSticks, renderStems3D,
                                                    renderErrorBars3D)
import           Graphics.Hgg.ThreeD.Types

-- | 'VisualSpec3D' を Phase 3 CPU projection で SVG に保存。
--
-- defaults:
--
--   * camera     → 'defaultCameraZUp' 3
--   * projection → 'defaultPerspective'
--   * axes       → 'autoAxes3D' (= layers の Point3 集合から bbox 算出)
--   * width / height → 700 / 700
--   * title      → ""
saveSVG3D :: FilePath -> VisualSpec3D -> IO ()
saveSVG3D path spec =
  let (w, h, title, prims) = spec3DToPrimitives spec
  in savePrimitivesSVG path w h title prims

-- | Phase 24 A8: 'VisualSpec3D' を (幅, 高さ, タイトル, [Primitive]) に落とす
-- 共通核。 各 backend (SVG/PDF/PNG) の出力関数が共有する。 prims は
-- 'renderSpec3DInPanel' 単一 panel (= キャンバス全域)。 title は別返し
-- (SVG は backend が <text> 化・PDF/PNG は 'savePDF3D'/'savePNG3D' が PText 化)。
spec3DToPrimitives :: VisualSpec3D -> (Int, Int, Text, [Primitive])
spec3DToPrimitives spec =
  let w     = fromMaybe 700 (getLast (vs3Width  spec))
      h     = fromMaybe 700 (getLast (vs3Height spec))
      title = fromMaybe ""  (getLast (vs3Title  spec))
      prims = renderSpec3DInPanel 0 0 (fromIntegral w) (fromIntegral h) spec
  in (w, h, title, prims)

-- | Phase 24 A8: 3D 図を PDF に保存 ('saveSVG3D' の PDF 版)。 [Primitive] を
-- 既存 PDF backend ('savePrimitivesPDF') へ配線。 title は上部中央の PText に
-- する (PDF backend は title 引数を持たないため)。 ⚠ PDF 標準フォントは
-- Latin-1 のみ (日本語ラベルは 'savePNG3D')。
savePDF3D :: FilePath -> VisualSpec3D -> IO ()
savePDF3D path spec =
  let (w, h, title, prims) = spec3DToPrimitives spec
  in savePrimitivesPDF path w h (titlePrims w title <> prims)

-- | Phase 24 A8: 3D 図を PNG に保存 ('saveSVG3D' の PNG 版・日本語ラベル可)。
-- [Primitive] を既存 Rasterific backend ('savePrimitivesPNG') へ配線。
savePNG3D :: FilePath -> VisualSpec3D -> IO ()
savePNG3D path spec =
  let (w, h, title, prims) = spec3DToPrimitives spec
  in savePrimitivesPNG defaultPNGConfig path w h (titlePrims w title <> prims)

-- | タイトルを上部中央の PText に (空なら無し)。 SVG backend の title 描画
-- (font-size 16・中央・y≈24) に概ね合わせる。
titlePrims :: Int -> Text -> [Primitive]
titlePrims w title
  | T.null title = []
  | otherwise    =
      [ PText (Point (fromIntegral w / 2) 24) title
              (TextStyle "#333333" 16 "sans-serif" AnchorMiddle 0 "normal" False) ]

-- | Phase 24 A7: 'VisualSpec3D' を**任意のパネル矩形** @(px,py,pw,ph)@ 内に
-- 描画する純粋核 ('saveSVG3D' と 'saveSVG3DFacet' が共有)。 軸 box・正規化
-- pipeline・layer・colorbar をパネル局所座標で出す。 @(0,0,w,h)@ で呼ぶと
-- 旧 'saveSVG3D' とビット同一 (margin 50・colorbar 右端)。
renderSpec3DInPanel :: Double -> Double -> Double -> Double -> VisualSpec3D -> [Primitive]
renderSpec3DInPanel px py pw ph spec =
  let layers = vs3Layers spec
      cam    = fromMaybe (defaultCameraZUp 3)   (getLast (vs3Camera spec))
      proj   = fromMaybe defaultPerspective     (getLast (vs3Proj   spec))
      -- Phase 25 A8: log scale flag を Axes3D に注入 (auto or user の bbox に上書き)。
      -- normPoint3D / 軸 renderer / surface 正規化が ax の axes*Log を読む。
      (lx, ly, lz) = fromMaybe (False, False, False) (getLast (vs3Log spec))
      ax0    = padAxes3D (fromMaybe (autoAxes3D layers) (getLast (vs3Axes spec)))
      ax     = ax0 { axesXLog = lx, axesYLog = ly, axesZLog = lz }
      -- Phase 24 A2: colormap 付き surface があれば右に colorbar を出す
      -- (z range はその layer の grid 全体)。
      -- Phase 25 A3: 連続色 scatter ('colorContinuousBy3D') の colorbar 情報
      -- ('lyr3Colorbar' = resolve 産物) も同じ場所に出す (surface 優先で先頭採用)。
      cbInfo = listToMaybe
        (  [ (stops, minimum zs, maximum zs)
           | l <- layers
           , getFirst (lyr3Kind l) == Just M3Surface
           , Just stops <- [getLast (lyr3Colormap l)]
           , Just grid  <- [getLast (lyr3Grid l)]
           , let zs = concat grid
           , not (null zs) ]
        ++ [ cb | l <- layers, Just cb <- [getLast (lyr3Colorbar l)] ])
      cbW    = case cbInfo of { Just _ -> 70; Nothing -> 0 }
      -- Phase 25 A2: 群色分けの離散凡例 (lyr3Legend・初出順で union)
      legendEntries = nub (concatMap (fromMaybe [] . getLast . lyr3Legend) layers)
      lgW    = if null legendEntries then 0 else 100
      m      = 50   -- パネル内側 margin (colorbar/凡例 分は右を空ける)
      vp     = Viewport (px + m) (py + m) (pw - 2*m - cbW - lgW) (ph - 2*m)
      -- Phase 24 A8 / 25 A8: box aspect (正規化後 x/y/z を各 a 倍) と軸名。
      -- za は従来 (z のみ)、 Phase 25 A8 で x/y も追加 (既定 1 = 恒等)。
      xa     = fromMaybe 1 (getLast (vs3XAspect spec))
      ya     = fromMaybe 1 (getLast (vs3YAspect spec))
      za     = fromMaybe 1 (getLast (vs3ZAspect spec))
      labels = fromMaybe ("x", "y", "z") (getLast (vs3AxisTitles spec))
      applyAspect (Point3 x y z) = Point3 (x * xa) (y * ya) (z * za)
      -- Phase 24 A8: 全 layer を正規化 + aspect した上で、 **surface 面と
      -- scatter 点を層横断で 1 つの depth リストに混ぜて大域ソート** する
      -- (= scatter 点が surface 膜を透ける問題の解消)。 床面 contour は床に
      -- 固定なので先 (奥)、 line/wireframe は depth 統合せず後 (前) に描く。
      scaled      = map (scaleAspectLayer xa ya za . normLayer3D ax) layers
      floorPrims  = concatMap (renderProjectedContour3D (xa, ya, za) cam proj vp) scaled
      depthItems  = concatMap (depthItemsOf cam proj vp) scaled
      otherPrims  = concatMap (otherLayerPrims cam proj vp) scaled
      errBarPrims = concatMap (errorBarPrimsOf cam proj vp) scaled   -- Phase 25 A5
      textPrims   = concatMap (textPrimsOf     cam proj vp) scaled   -- Phase 25 A7
      mergedDepth = map snd (sortOn (negate . fst) depthItems)
      -- Phase 24 A3: データ座標を [-1,1]^3 に正規化してから投影する。
      -- 既定 camera (距離 3-5) は単位スケール前提のため、 実データ
      -- (z ≈ 80 等) をそのまま投影すると視界外になる。 軸ラベルは
      -- 元のデータ値のまま ('renderAxes3DWithLabels')。 mplot3d の axes box 同型。
      -- Phase 25 A6: 壁面 pane + gridline (既定 ON)。 cube wireframe より前 =
      -- 最背面に置き、 データ・wireframe を pane の手前に出す。
      paneOn   = fromMaybe True (getLast (vs3Pane spec))
      panePrims = if paneOn
                    then renderAxes3DPanes (applyAspect . normPoint3D ax)
                           defaultPaneStyle3D cam proj vp ax
                    else []
  in panePrims
       <> renderAxes3DWithLabels (applyAspect . normPoint3D ax) labels cam proj vp ax
       <> floorPrims
       <> mergedDepth
       <> otherPrims
       <> errBarPrims
       <> textPrims
       <> maybe [] (renderColorbar3D px py pw ph) cbInfo
       <> renderLegend3D px py pw ph legendEntries

-- | Phase 25 A2: 群色分けの離散凡例 (色チップ + ラベル) をパネル右端に縦並び。
-- colorbar があればその左、 無ければ右端。 entries = (カテゴリ, 色) の初出順。
renderLegend3D :: Double -> Double -> Double -> Double -> [(Text, Text)] -> [Primitive]
renderLegend3D _  _  _  _  []      = []
renderLegend3D px py pw ph entries =
  let chip = 12 :: Double
      rowH = 20 :: Double
      n    = length entries
      x0   = px + pw - 96
      y0   = py + (ph - fromIntegral n * rowH) / 2
      tsL  = TextStyle "#333333" 11 "sans-serif" AnchorStart 0 "normal" False
      row (i, (cat, col)) =
        let y = y0 + fromIntegral i * rowH
        in [ PRect (Rect x0 y chip chip)
                   (FillStyle col 1.0) (Just (StrokeStyle "#333333" 0.5))
           , PText (Point (x0 + chip + 6) (y + chip - 2)) cat tsL ]
  in concatMap row (zip [0 :: Int ..] entries)

-- | Phase 24 A8: depth 統合対象 (surface 面・scatter 点) を @(depth, Primitive)@
-- で返す。 line/wireframe/floor は対象外 ([])。
depthItemsOf :: Camera3D -> Projection3D -> Viewport -> Layer3D -> [(Double, Primitive)]
depthItemsOf cam proj vp l = case getFirst (lyr3Kind l) of
  Just M3Surface -> surfaceFacesDepth  cam proj vp (layerToSurface l)
  Just M3Scatter -> scatterPointsDepth cam proj vp (layerToScatter l)
  Just M3Bar     -> barFacesDepth      cam proj vp (layerToBar     l)  -- Phase 25 A5 (cuboid のみ)
  -- Phase 26 A5: trisurf = 正規化済点群を (x,y) で Delaunay 三角分割し face を depth 統合
  Just M3Trisurf ->
    let pts  = fromMaybe [] (getLast (lyr3Points l))
        tris = delaunay2D [ (x, y) | Point3 x y _ <- pts ]
        vpts = V.fromList pts
        triPts = [ (vpts V.! a, vpts V.! b, vpts V.! c) | (a, b, c) <- tris ]
        col    = fromMaybe "#5b9bd5" (getLast (lyr3Color     l))
        ecol   = fromMaybe "#3a73a6" (getLast (lyr3EdgeColor l))
        shaded = fromMaybe True      (getLast (lyr3Shaded    l))
        cmap   = getLast (lyr3Colormap l)
        alpha  = fromMaybe 1         (getLast (lyr3Alpha     l))
    in trianglesFacesDepth cam proj vp col ecol shaded cmap alpha triPts
  _              -> []

-- | Phase 24 A8: depth 統合対象外の layer (line/wireframe・bar stick) を描く。
otherLayerPrims :: Camera3D -> Projection3D -> Viewport -> Layer3D -> [Primitive]
otherLayerPrims cam proj vp l = case getFirst (lyr3Kind l) of
  Just M3Line      -> renderLine3D      cam proj vp (layerToLine      l)
  Just M3Wireframe -> renderWireframe3D cam proj vp (layerToWireframe l)
  Just M3Bar       -> renderBarSticks   cam proj vp (layerToBar       l)  -- Phase 25 A5 (stick のみ)
  -- Phase 26 A4: stem は細い垂線 + 先端マーカー (前面 overlay)。 線幅既定 2・
  -- マーカー半径既定 5 (lyr3Width / lyr3Size で上書き可)。
  Just M3Stem      ->
    let bar = (layerToBar l) { br3Width = fromMaybe 2 (getLast (lyr3Width l)) }
        markerR = fromMaybe 5 (getLast (lyr3Size l))
    in renderStems3D cam proj vp bar markerR
  -- Phase 26 A3: 3D vector field (前面 overlay・投影後 2D 矢印)
  Just M3Quiver    -> renderQuiver3D cam proj vp (layerToQuiver l)
  _                -> []

-- | Phase 25 A5: 層の per-point err (正規化済) があれば誤差棒を描く (bar/scatter
-- 共通)。 頂点列は正規化済 'lyr3Points'。 最前面に描くため depth 統合外。
errorBarPrimsOf :: Camera3D -> Projection3D -> Viewport -> Layer3D -> [Primitive]
errorBarPrimsOf cam proj vp l =
  case (getLast (lyr3PtErrs l), getLast (lyr3Points l)) of
    (Just es, Just pts) | not (null es) ->
      renderErrorBars3D cam proj vp "#333333" 1.0 (zip pts es)
    _ -> []

-- | Phase 25 A7: テキスト注釈 layer (M3Text) を描く。 正規化済 'lyr3Points' を
-- 投影し、 各点に 'lyr3Labels' の文字列を PText で置く (depth 統合外の前面 overlay)。
-- 文字色 = 'lyr3Color' (既定 @#333333@)、 サイズ = 'lyr3Size' (既定 11)。 点の
-- 少し上 (-6px) に中央寄せ。
textPrimsOf :: Camera3D -> Projection3D -> Viewport -> Layer3D -> [Primitive]
textPrimsOf cam proj vp l = case getFirst (lyr3Kind l) of
  Just M3Text ->
    let pts = fromMaybe [] (getLast (lyr3Points l))
        lbs = fromMaybe [] (getLast (lyr3Labels l))
        col = fromMaybe "#333333" (getLast (lyr3Color l))
        sz  = fromMaybe 11        (getLast (lyr3Size  l))
        ts  = TextStyle col sz "sans-serif" AnchorMiddle 0 "normal" False
        toScreen p = let Projected sx sy _ = project3D cam proj vp p in Point sx sy
        place p t  = let Point lx ly = toScreen p in PText (Point lx (ly - 6)) t ts
    in zipWith place pts lbs
  _ -> []

-- | Phase 24 A8 / 25 A8: 正規化済 layer を box aspect @(xa, ya, za)@ 倍する。
-- surface は grid z (za) と x/y range (xa/ya)、 それ以外は点列の各成分。
-- @(1,1,1)@ は恒等 (= 旧出力ビット不変)。 surface の grid x/y は xRange を縮める
-- ことで反映 (Surface.hs が xRange を線形補間して頂点を置くため)。
scaleAspectLayer :: Double -> Double -> Double -> Layer3D -> Layer3D
scaleAspectLayer xa ya za l
  | xa == 1 && ya == 1 && za == 1 = l
  | otherwise = case getFirst (lyr3Kind l) of
      Just M3Surface ->
        l { lyr3Grid   = Last (fmap (map (map (* za))) (getLast (lyr3Grid l)))
          , lyr3XRange = Last (fmap (scaleRange xa) (getLast (lyr3XRange l)))
          , lyr3YRange = Last (fmap (scaleRange ya) (getLast (lyr3YRange l)))
          }
      _ ->
        -- Phase 25 A5: bar 底面 z・err (z 量) も同じ za で縮める
        -- Phase 26 A3: quiver ベクトルも各成分を aspect 倍 (始点 + vec が aspect 後点と一致)
        l { lyr3Points   = Last (fmap (map sp) (getLast (lyr3Points l)))
          , lyr3BarBaseZ = Last (fmap (* za) (getLast (lyr3BarBaseZ l)))
          , lyr3PtErrs   = Last (fmap (map (* za)) (getLast (lyr3PtErrs l)))
          , lyr3Vectors  = Last (fmap (map sv) (getLast (lyr3Vectors l)))
          }
  where sp (Point3 x y z) = Point3 (x * xa) (y * ya) (z * za)
        sv (Vec3 vx vy vz) = Vec3 (vx * xa) (vy * ya) (vz * za)
        scaleRange a (lo, hi) = (lo * a, hi * a)

-- | Phase 24 A7: 群別 3D 図のタイル配置 (= 「群別曲面の並置」)。 N 個の
-- @(群ラベル, spec)@ を near-square グリッド (@ncol = ceil(√n)@) に並べ、 各
-- パネルを 'renderSpec3DInPanel' で描き、 上部に群ラベルを置く。 colorbar は
-- panel 毎 (各 spec の colormap surface に追従)。 camera/axes は各 spec が個別に
-- 持つ前提 (analyze の群別 'surfaceOf' 出力をそのまま渡せる)。 1 パネルの
-- 既定サイズは 380×380 px (spec が 'width3DV'/'height3DV' を持てばそれを使う)。
saveSVG3DFacet :: FilePath -> [(Text, VisualSpec3D)] -> IO ()
saveSVG3DFacet _    []     = pure ()
saveSVG3DFacet path panels = do
  let n      = length panels
      ncol   = ceiling (sqrt (fromIntegral n :: Double)) :: Int
      nrow   = (n + ncol - 1) `div` ncol
      -- パネルサイズ = 先頭 spec の width/height (なければ 380)
      panelW = fromIntegral (fromMaybe 380 (getLast (vs3Width  (snd (head panels)))))
      panelH = fromIntegral (fromMaybe 380 (getLast (vs3Height (snd (head panels))))) :: Double
      wPx    = round (panelW * fromIntegral ncol) :: Int
      hPx    = round (panelH * fromIntegral nrow) :: Int
      tsLab  = TextStyle "#333333" 13 "sans-serif" AnchorMiddle 0 "bold" False
      panelPrims k (label, spec) =
        let row = k `div` ncol
            col = k `mod` ncol
            px  = fromIntegral col * panelW
            py  = fromIntegral row * panelH
            lbl = PText (Point (px + panelW / 2) (py + 22)) label tsLab
        in lbl : renderSpec3DInPanel px py panelW panelH spec
      prims = concat (zipWith panelPrims [0 ..] panels)
  savePrimitivesSVG path wPx hPx "" prims

-- | Phase 24 A3: 退化した軸 (min == max) を ±0.5 に広げる (正規化の 0 割り防止)。
padAxes3D :: Axes3D -> Axes3D
padAxes3D ax =
  let pad lo hi | hi > lo   = (lo, hi)
                | otherwise = (lo - 0.5, lo + 0.5)
      (x0, x1) = pad (axesXMin ax) (axesXMax ax)
      (y0, y1) = pad (axesYMin ax) (axesYMax ax)
      (z0, z1) = pad (axesZMin ax) (axesZMax ax)
  in ax { axesXMin = x0, axesXMax = x1
        , axesYMin = y0, axesYMax = y1
        , axesZMin = z0, axesZMax = z1 }

-- | Phase 24 A3 / 25 A8: 軸 bbox を [-1,1]^3 へ写す正規化 (各軸独立)。 log 軸
-- ('axes*Log') は log10 空間で affine、 線形軸はそのまま affine。
normPoint3D :: Axes3D -> Point3 -> Point3
normPoint3D ax (Point3 x y z) =
  Point3 (normCoord (axesXLog ax) (axesXMin ax) (axesXMax ax) x)
         (normCoord (axesYLog ax) (axesYMin ax) (axesYMax ax) y)
         (normCoord (axesZLog ax) (axesZMin ax) (axesZMax ax) z)

-- | Phase 25 A8: 1 軸の正規化 (log/線形)。 @[lo,hi]@ → @[-1,1]@。 log は値・端を
-- 1e-12 に clamp してから log10 空間で affine。
normCoord :: Bool -> Double -> Double -> Double -> Double
normCoord isLog lo hi v
  | isLog =
      let lo' = logBase 10 (max 1e-12 lo)
          hi' = logBase 10 (max 1e-12 hi)
          v'  = logBase 10 (max 1e-12 v)
      in if hi' <= lo' then 0 else -1 + 2 * (v' - lo') / (hi' - lo')
  | hi <= lo  = 0
  | otherwise = -1 + 2 * (v - lo) / (hi - lo)

-- | Phase 24 A3: layer のデータを正規化座標に写す。 surface は grid の z と
-- x/y range、 それ以外は点列。 colormap の色は z 正規化に対して不変 (affine)。
normLayer3D :: Axes3D -> Layer3D -> Layer3D
normLayer3D ax l0 = case getFirst (lyr3Kind l) of
  Just M3Surface ->
    -- Phase 25 A8: surface の z は log-z 対応 (axesZLog)。 surface の x/y log は
    -- 未対応 (grid x/y は xRange を線形補間する Surface.hs 都合)。
    let nmz = normCoord (axesZLog ax) (axesZMin ax) (axesZMax ax)
    in l { lyr3XRange = Last (Just (-1, 1))
         , lyr3YRange = Last (Just (-1, 1))
         , lyr3Grid   = Last (fmap (map (map nmz)) (getLast (lyr3Grid l)))
         }
  kind ->
    -- Phase 25 A5: 点に加え err (z 量・delta) と bar 底面 z も正規化する。
    let zspan  = axesZMax ax - axesZMin ax
        zscale = if zspan <= 0 then 0 else 2 / zspan          -- delta 用 (±err)
        nmz v  = if zspan <= 0 then 0 else -1 + 2 * (v - axesZMin ax) / zspan
        clampN = max (-1) . min 1
        l1 = l { lyr3Points = Last (fmap (map (normPoint3D ax)) (getLast (lyr3Points l)))
               , lyr3PtErrs = Last (fmap (map (* zscale)) (getLast (lyr3PtErrs l)))
               }
    in case kind of
         Just M3Bar  -> l1 { lyr3BarBaseZ = Last (Just (clampN (nmz 0))) }
         -- Phase 26 A4: stem は stemBaseZ (data 空間・既定 0) を正規化して底面に使う
         Just M3Stem -> l1 { lyr3BarBaseZ = Last (Just (clampN (nmz (fromMaybe 0 (getLast (lyr3StemBaseZ l)))))) }
         -- Phase 26 A3: quiver のベクトルは per-axis の affine 微分 (= 2/span) で正規化
         -- (正規化は affine ゆえ norm(p+v)-norm(p) = v * 2/span)。
         Just M3Quiver ->
           let xspan = axesXMax ax - axesXMin ax
               yspan = axesYMax ax - axesYMin ax
               sx = if xspan <= 0 then 0 else 2 / xspan
               sy = if yspan <= 0 then 0 else 2 / yspan
               sz = zscale
               nv (Vec3 vx vy vz) = Vec3 (vx * sx) (vy * sy) (vz * sz)
           in l1 { lyr3Vectors = Last (fmap (map nv) (getLast (lyr3Vectors l))) }
         _           -> l1
  where
    -- Phase 30 A6: inline 注釈 ('lyr3Annots') を持つ M3Text は、 正規化前に
    --   lyr3Points/lyr3Labels へ materialize する (= 以降の正規化・renderer は無改修。
    --   列駆動 text3D は resolveLayer3D で既に lyr3Points/Labels が入るので annots は空)。
    l = case (getFirst (lyr3Kind l0), lyr3Annots l0) of
          (Just M3Text, annots@(_:_)) ->
            l0 { lyr3Points = Last (Just (map fst annots))
               , lyr3Labels = Last (Just (map snd annots)) }
          _ -> l0

-- | Phase 24 A2: パネル右端の縦 colorbar (gradient strip + min/mid/max ラベル)。
--   2D の gradient bar 凡例と同じ 'continuousColor' 補間 = palette 整合。
--   Phase 24 A7: パネル矩形 @(px,py,pw,ph)@ 局所座標で配置 (facet 対応)。
renderColorbar3D :: Double -> Double -> Double -> Double -> ([Text], Double, Double) -> [Primitive]
renderColorbar3D px py pw ph (stops, zMin, zMax) =
  let barW = 14 :: Double
      barH = ph * 0.45
      x0   = px + pw - 58
      y0   = py + (ph - barH) / 2
      nStrip = 48 :: Int
      stripH = barH / fromIntegral nStrip
      strip k =
        let t = fromIntegral k / fromIntegral (nStrip - 1)
            -- t=1 (zMax) が上。 +0.5 はストライプ間の継ぎ目消し
            y = y0 + barH - stripH * fromIntegral (k + 1)
        in PRect (Rect x0 y barW (stripH + 0.5))
                 (FillStyle (continuousColor stops t) 1.0)
                 Nothing
      ts = TextStyle "#444444" 9 "sans-serif" AnchorStart 0 "normal" False
      tickAt v =
        let t = if zMax <= zMin then 0.5 else (v - zMin) / (zMax - zMin)
        in y0 + barH * (1 - t)
      tick v = PText (Point (x0 + barW + 4) (tickAt v + 3)) (fmtNum3D v) ts
  in map strip [0 .. nStrip - 1]
     <> map tick [zMin, (zMin + zMax) / 2, zMax]

-- | tick 値の短い整形 (Axes3D の formatNum と同形)。
fmtNum3D :: Double -> Text
fmtNum3D x =
  let s = T.pack (show (fromIntegral (round (x * 10) :: Int) / 10.0 :: Double))
  in T.dropWhileEnd (== '.') (T.dropWhileEnd (== '0') s)

-- | Phase 24 A5 / #2: surface の contour を壁面へ投影する (matplotlib
-- @contour(..., zdir=)@ 相当)。 'lyr3Contours' の各 @(dir, n)@ を描く:
--
--   * 'ContourZ' (床) = z 等値面 level set @{f=z_k}@ を 'marchingSegments' で抽出し
--     floor へ落とす (topographic map)。
--   * 'ContourX' (左右壁) = x 軸の @n@ 位置で曲面を切り、 断面プロファイル
--     @z = f(x_k, y)@ を yz 壁へ描く。
--   * 'ContourY' (前後壁) = y 軸の @n@ 位置で断面 @z = f(x, y_k)@ を xz 壁へ描く。
--
-- 投影壁は **カメラから遠い面に自動固定** (eye と target の各軸符号で min/max 面を選ぶ・
-- 曲面の手前に出て遮らないように)。 grid・aspect 正規化済 layer を受ける。 mpl の
-- rotate_axes 経路 (= dir⊥平面で切った断面を壁へ投影) を再現 (実測で数値突合済)。
renderProjectedContour3D
  :: (Double, Double, Double)
  -> Camera3D -> Projection3D -> Viewport -> Layer3D -> [Primitive]
renderProjectedContour3D (xa, ya, za) cam proj vp l =
  case lyr3Contours l of
    []  -> []
    cs  ->
      let grid = fromMaybe [] (getLast (lyr3Grid l))
          ny   = length grid
          nx   = case grid of { row : _ -> length row; [] -> 0 }
      in if nx < 2 || ny < 2 then [] else
        let (xMin, xMax) = fromMaybe (-1, 1) (getLast (lyr3XRange l))   -- cube [-xa, xa]
            (yMin, yMax) = fromMaybe (-1, 1) (getLast (lyr3YRange l))   -- cube [-ya, ya]
            xNodes = [ xMin + (xMax - xMin) * fromIntegral j / fromIntegral (nx - 1)
                     | j <- [0 .. nx - 1] ]
            yNodes = [ yMin + (yMax - yMin) * fromIntegral i / fromIntegral (ny - 1)
                     | i <- [0 .. ny - 1] ]
            allZ   = concat grid
            zmin   = minimum allZ
            zmax   = maximum allZ
            stops  = fromMaybe viridisStops3D (getLast (lyr3Colormap l))
            frac lo hi v = if hi <= lo then 0.5 else (v - lo) / (hi - lo)
            colAt t = continuousColor stops (max 0 (min 1 t))
            projPt p = let pr = project3D cam proj vp p in Point (projX pr) (projY pr)
            polyline col pts = [ PLine (projPt p) (projPt q) (solid col 1.2)
                               | (p, q) <- zip pts (drop 1 pts) ]
            -- 投影壁 = カメラから遠い面。 eye と target の各軸差の符号で min/max を選ぶ
            -- (eye が +側なら −側の壁を使い、 曲面の奥に投影が回る)。
            Point3 ex ey ez = cameraEye cam
            Point3 tx ty tz = cameraTarget cam
            farWall a e t = if e >= t then negate a else a
            one (ContourZ, nLev) =
              let offset = farWall za ez tz
                  levels = innerLevels nLev zmin zmax
                  drawLevel lv =
                    [ PLine (projPt (Point3 ax' ay offset)) (projPt (Point3 bx by offset))
                            (solid (colAt (frac zmin zmax lv)) 1.2)
                    | ((ax', ay), (bx, by)) <- marchingSegments xNodes yNodes grid lv ]
              in if zmax <= zmin then [] else concatMap drawLevel levels
            one (ContourX, nLev) =
              -- x 軸を等分した位置で断面 z=f(x_k, y)。 各 y 行で grid を x 方向に補間。
              let offset = farWall xa ex tx
                  section x0 =
                    polyline (colAt (frac xMin xMax x0))
                      [ Point3 offset (yNodes !! i) (interp1 xNodes (grid !! i) x0)
                      | i <- [0 .. ny - 1] ]
              in concatMap section (innerLevels nLev xMin xMax)
            one (ContourY, nLev) =
              -- y 軸を等分した位置で断面 z=f(x, y_k)。 各 x 列で grid を y 方向に補間。
              let offset = farWall ya ey ty
                  colZ j = [ (grid !! i) !! j | i <- [0 .. ny - 1] ]
                  section y0 =
                    polyline (colAt (frac yMin yMax y0))
                      [ Point3 (xNodes !! j) offset (interp1 yNodes (colZ j) y0)
                      | j <- [0 .. nx - 1] ]
              in concatMap section (innerLevels nLev yMin yMax)
        in concatMap one cs

-- | 1 次元線形補間。 @interp1 nodes vals p@ は昇順 @nodes@ 上の値 @vals@ を位置 @p@ で
-- 補間 (範囲外は端値で clamp・隣接 2 点で線形)。 断面 contour の grid 補間に使う。
interp1 :: [Double] -> [Double] -> Double -> Double
interp1 nodes vals p = go (zip nodes vals)
  where
    go [] = 0
    go [(_, v)] = v
    go ((a, va) : rest@((b, vb) : _))
      | p <= a    = va
      | p <= b    = va + (vb - va) * (p - a) / (b - a)
      | otherwise = go rest
