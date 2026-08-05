-- |
-- Module      : Graphics.Hgg.Render.Distribution
-- Description : Distribution marks: box, violin, strip, swarm, raincloud, ridge
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: Render モノリス分割 (出力中立・純粋移動)。
--   [English]: Split out from the Render monolith (an output-neutral, pure move).
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphics.Hgg.Render.Distribution where

import           Graphics.Hgg.Layout (Layout (..), Rect (..), Scale (..),
                                      ViewportSize (..), computeLayout,
                                      ggAxTextMar, ggAxTitleMar, ggHalfLine,
                                      ggTickLen, niceTicks, scaleApply,
                                      Track (..), solveTracks,
                                      needsLegend, effectiveLegendPos,
                                      coordOf, isPolar, polarCenter, polarPoint,
                                      domFrac, projectXY, projectRectData,
                                      projectBarRect, catUnitPx, AxisPlacement (..),
                                      coordXAxisPlacement, coordYAxisPlacement,
                                      coordXGridIsVertical,
                                      -- Phase 64 A3: categorical-cross 投影口
                                      CrossLoc (..), BarShape (..),
                                      projectCrossPoint, projectCrossSpan,
                                      projectCrossBar, valueAxisPx)
import           Graphics.Hgg.Layout.RangeOf (qqPoints, ecdfPoints)  -- Phase 11 A6-2/A6-4
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format     as Data.Time.Format
import           Graphics.Hgg.Spec   (Annotation (..), AxisFormat (..),
                                      ColData (..), ColRef,
                                      ColorEnc (..), ConnectSpec (..),
                                      DAGEdge (..), DAGLayoutAlgorithm (..),
                                      DAGNode (..), DAGNodeKind (..),
                                      DAGPlate (..), DAGSpec (..), Layer (..),
                                      LegendPosition (..), LegendSpec (..),
                                      Inset (..), MarginalSpec (..), MarkKind (..),
                                      MarkShape (..), ShapeMapEntry (..),
                                      LineType (..), lineTypeDash, lineTypeForIndex,
                                      ReferenceLine (..), Resolver,
                                      Position (..), Coord (..), Side (..),
                                      FacetScales (..), freeScaleX, freeScaleY,
                                      FacetSpace (..), freeSpaceX, freeSpaceY,
                                      ThemeOverride (..),
                                      VisualSpec (..), YAxisSide (..), axisFormatOf,
                                      axisRotateOf, resolveAxisAngle, axisShowTicksOf,
                                      axShowGrid,
                                      FontSpec (..),
                                      colRefName, distGroupRef, distDodgeRef,
                                      resolveCol, resolveNum)
import           Data.Maybe          (mapMaybe, isJust)
import           Data.List           (sortOn, foldl')
import qualified Data.Map.Strict     as Map
import           Data.List           (dropWhile, elemIndex, groupBy, nub,
                                      sort, takeWhile)
import qualified Graphics.Hgg.Spec
import           Data.Monoid         (First (..), Last (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Numeric             (showEFloat, showFFloat)

import           Graphics.Hgg.Primitive
import           Graphics.Hgg.Render.Common


-- | [日本語]: 各群を 'lpXCategoryLabels' 内の __大域 index__ (= 列名スロット) に置く。
--   cats が空 (単一群・非 categorical) なら局所順 @[0..]@。 既存の grouped 図は groups が cats と
--   同順・全在ゆえ大域 = 局所で __byte 不変__。 distCols は各レーンが 1 群 (自列名) = 大域 index。
--   [English]: Places each group at its __global index__ within
--   'lpXCategoryLabels' (a column-name slot). If cats is empty (a single group,
--   non-categorical), falls back to local order @[0..]@. In existing grouped
--   figures, groups follow cats in the same order and are all present, so
--   global equals local and output is __byte-identical__. For distCols, each
--   lane is its own group (its own column name), which is its global index.
laneIndices :: Layout -> [(Text, a)] -> [Int]
laneIndices layout gs =
  let xls = lpXCategoryLabels layout
  in if null xls then [0 ..]
     else [ maybe i id (elemIndex g xls) | (i, (g, _)) <- zip [0 ..] gs ]

-- | [日本語]: Box plot (= 5-number summary)。 PS / HS で API 統一: lyEncY = 値、 lyEncX = 群 (optional)。
--   群指定なしなら単一 box を plot 中央に。 群指定ありなら各群について並列描画。
--   中央線 (median) + IQR 箱 + 髭 (min/max within 1.5*IQR)。
--   [English]: A box plot (a 5-number summary). Unified API across PS/HS:
--   lyEncY is the value, lyEncX is the group (optional). With no group, draws
--   a single box centered in the plot; with a group, draws each group's box
--   side by side. Shows the median line, the IQR box, and whiskers
--   (min/max within 1.5*IQR).
-- | [日本語]: box glyph を CrossLoc (cross 位置) + 半幅指定で描く共通部。
--   座標変換は 'Graphics.Hgg.Layout.projectCrossBar' /
--   'Graphics.Hgg.Layout.projectCrossSpan' /
--   'Graphics.Hgg.Layout.projectCrossPoint' に集約し、 geom 側は coord を場合分け
--   しない。 直線座標系は旧 px 式と byte 一致 (halfPx = box 半幅 px)、 polar は
--   箱 = wedge (halfD = data 半幅)・髭 = radial 線・median/cap = 弧。
--   normal path (群 = カテゴリ位置) と dodge path (sub-slot 中心) が共有。
--   [English]: The shared routine that draws a box glyph from a 'CrossLoc'
--   (its position on the cross axis) plus a half width. All coordinate
--   conversion is concentrated in 'Graphics.Hgg.Layout.projectCrossBar' /
--   'Graphics.Hgg.Layout.projectCrossSpan' /
--   'Graphics.Hgg.Layout.projectCrossPoint', so the geom itself never
--   branches on the coordinate system. Linear systems are byte identical to
--   the previous pixel formulas (halfPx is the box half width in pixels); in
--   polar the body becomes a wedge (halfD is the half width in data units),
--   the whiskers become radial lines, and the median and caps become arcs.
--   Shared by the normal path (group at a categorical position) and the dodge
--   path (centered in a sub-slot).
boxGlyphAt :: Coord -> Layout -> CrossLoc -> Double -> Double -> Double
           -> Double -> [Double] -> Text -> Text -> [Primitive]
boxGlyphAt coord layout loc offPx halfPx halfD a sorted0 fill stroke =
  let sorted = sort sorted0
      n  = length sorted
      q p =
        let pos  = p * fromIntegral (n - 1)
            lo   = floor pos :: Int
            hi   = min (n - 1) (lo + 1)
            frac = pos - fromIntegral lo
        in case (sorted !? lo, sorted !? hi) of
             (Just a', Just b') -> a' + (b' - a') * frac
             _                  -> 0
      (!?) xs i_ = if i_ < 0 || i_ >= length xs then Nothing else Just (xs !! i_)
      q1 = q 0.25; q2 = q 0.50; q3 = q 0.75
      iqr = q3 - q1
      loW = q1 - 1.5 * iqr
      hiW = q3 + 1.5 * iqr
      loV = case dropWhile (< loW) sorted of (v:_) -> v; [] -> q1
      hiV = case reverse (takeWhile (<= hiW) sorted) of (v:_) -> v; [] -> q3
      pt v = projectCrossPoint coord layout loc offPx v
      -- cross 方向の短線 (median/cap)。 直線座標系は 2 点 = 単一 PLine (byte 不変)、
      -- polar の弧 (> 2 点) は連続 PLine 群で折線化。
      spanLine w hPx hD v =
        let pts = projectCrossSpan coord layout loc offPx hPx hD v
        in [ PLine p0 p1 (solid stroke w) | (p0, p1) <- zip pts (drop 1 pts) ]
      body = case projectCrossBar coord layout loc offPx halfPx halfD q1 q3 of
        BarRect rect  -> PRect rect (FillStyle fill a) (Just (StrokeStyle stroke 1.0))
        BarWedge segs -> PPath segs (FillStyle fill a) (Just (StrokeStyle stroke 1.0))
      outliers = filter (\v -> v < loW || v > hiW) sorted
      outR = defaultMarkerDiameter / 2
      outlierPrims =
        [ PCircle (pt v) outR (FillStyle stroke 1.0) (Just (StrokeStyle stroke 1.0)) Nothing
        | v <- outliers ]
  in [ body ]
     <> spanLine 2.0 halfPx halfD q2
     <> [ PLine (pt q1) (pt loV) (solid stroke 1.0)
        , PLine (pt q3) (pt hiV) (solid stroke 1.0) ]
     <> spanLine 1.0 (halfPx / 2) (halfD / 2) loV
     <> spanLine 1.0 (halfPx / 2) (halfD / 2) hiV
     <> outlierPrims

renderBox :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBox r layout pal ly
  | isJust (distDodgeRef ly) = renderBoxDodge r layout pal ly
renderBox r layout pal ly =
  let groups = case distGroupRef ly of
        -- group 列あり (encX または colorBy 列): PS と同じ groupedValues
        Just _ -> [(g, sort vs) | (g, vs) <- groupedValuesOrdered layout r ly, not (null vs)]
        -- group 列なし: 単一群 ("all")
        Nothing -> case V.toList (vecOr (lyEncY ly) r) of
          [] -> []
          vs -> [("", sort vs)]
      -- ★ Phase 34: ggplot geom_boxplot 既定 = 枠 grey20・塗り white・不透明・外れ値ドット。
      --   user 明示色は枠色として優先 (ggplot aes(colour=) と同型)、 塗りは white 固定。
      c  = staticColorOr ly "#333333"          -- 枠/median/whisker = grey20 既定
      -- ★ 箱の塗り: colorBy (群色マップ) があれば群ごとに彩色 (violin/bar と統一・凡例と一致)、
      --   無ければ ggplot 既定の white。 明示 color X は枠色 (c) として従来どおり優先。
      catPal     = lpCategoricalPalette layout
      hasColorBy = case getLast (lyColor ly) of
        Just (ColorByCol _) -> True
        _                   -> False
      boxFillFor i = if hasColorBy && not (null catPal)
                       then catPal !! (i `mod` length catPal)
                       else "#ffffff"
      -- ★ Phase 36 C: hollow (= ggplot geom_boxplot(fill=NA)) は塗り透明・枠/median/whisker を
      --   群色 (colorBy 時) で描く。 colorBy 無しなら従来の c (grey20/明示色) のまま。
      isHollow = getLast (lyHollow ly) == Just True
      strokeFor i = if isHollow && hasColorBy && not (null catPal)
                      then catPal !! (i `mod` length catPal)
                      else c
      a  = doubleOr (lyAlpha ly) 1.0
      area = lpPlotArea layout
      nG = length groups
      hasCats = not (null (lpXCategoryLabels layout))
      -- PS と同 box width 規約: box width = 1 スロットの 0.5。 Step4c: categorical は unit
      -- (sx 1 - sx 0) 基準 (±0.5 で rW/nG 一致, ±0.6 で軸追従)。 単一群 (非 cat) は plotArea
      -- 全幅基準のまま中央 1 本。
      step  = rW area / fromIntegral (max 1 nG)
      -- ★ Phase 36 D1: markWidth (占有率・既定 0.5) で box 幅、 nudge (slot 幅比) で slot 内 offset。
      mw      = doubleOr (lyMarkWidth ly) 0.5
      -- ★ Phase 36 D2: no-cat (単一) は slot = plotArea ゆえ nudge 基準も rW area (strip/PS と統一)。
      nudgePx = doubleOr (lyNudge ly) 0 * (if hasCats then catUnitPx (lpCoord layout) layout else rW area)
      bwFor = if hasCats then catUnitPx (lpCoord layout) layout * mw else step * mw
      -- ★ Phase 64 A3: 座標変換は boxGlyphAt (→ projectCross*) に集約。 flipOnly を
      --   撤去し polar もそのまま渡す (箱 = wedge / 髭 = radial で描かれる)。
      coord = lpCoord layout
      -- polar 用の data 半幅: cat は 1 slot = 1 data 単位の mw/2、 単一群は x domain
      -- 全幅の mw/2 (linear 座標系では halfPx 側が使われ、 この値は参照されない)。
      spanX = lsDomainHi (lpXScale layout) - lsDomainLo (lpXScale layout)
      halfD = mw / 2 * (if hasCats then 1 else spanX)
      locFor i = if hasCats then CrossAt (fromIntegral i) else CrossMid
      mkBox i (_lbl, sorted) =
        boxGlyphAt coord layout (locFor i) nudgePx (bwFor / 2) halfD
                   (if isHollow then 0.0 else a) sorted
                   (boxFillFor i) (strokeFor i)
  in concat (zipWith mkBox (laneIndices layout groups) groups)

-- | Phase 36 B2: dodge box。 位置列 (@groupBy@) × 色列 (@colorBy@) で各位置カテゴリ内に
--   色サブグループを横並び (= ggplot @position_dodge@)。 色 = colorBy 水準の categorical
--   palette、 枠 = grey20 既定 (明示 'color' があれば枠色優先)。 box 実幅 = sub-slot の 85%。
renderBoxDodge :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderBoxDodge r layout _pal ly =
  let (_positions, colorCats, cells) = dodgeCells layout r ly
      nColor = max 1 (length colorCats)
      catPal = lpCategoricalPalette layout
      colorFor cix = if null catPal then "#333333" else catPal !! (cix `mod` length catPal)
      stroke = staticColorOr ly "#333333"
      -- ★ Phase 36 C: hollow は塗り透明・枠を群色 (= colorFor)。 非 hollow は従来 (枠 grey20)。
      isHollow = getLast (lyHollow ly) == Just True
      a      = doubleOr (lyAlpha ly) 1.0
      -- ★ Phase 64 A3: 座標変換は boxGlyphAt (→ projectCross*) に集約 (flipOnly 撤去)。
      coord  = lpCoord layout
      unit   = catUnitPx (lpCoord layout) layout
      subW   = unit * 0.9 / fromIntegral nColor   -- sub-slot px 幅
      bw     = subW * 0.85                          -- box 実幅 (sub-slot の 85%)
      -- polar 用 data 半幅 = sub-slot (0.9/nColor data 単位) の 85% の半分
      halfD  = 0.9 / fromIntegral nColor * 0.85 / 2
  in concat
     [ boxGlyphAt coord layout (CrossAt (dodgeCenterD pix cix nColor)) 0 (bw / 2) halfD
                  (if isHollow then 0.0 else a) (sort vs)
                  (colorFor cix) (if isHollow then colorFor cix else stroke)
     | (pix, cix, vs) <- cells ]

-- | [日本語]: dodge violin。 位置列 × 色列で各位置内に色サブグループの violin を横並び。
--   [English]: A dodge violin. Given a position column and a color column,
--   arranges color sub-group violins side by side within each position.
renderViolinDodge :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderViolinDodge r layout _ ly =
  let (_positions, colorCats, cells) = dodgeCells layout r ly
      nColor = max 1 (length colorCats)
      catPal = lpCategoricalPalette layout
      colorFor cix = if null catPal then "#3E6A6F" else catPal !! (cix `mod` length catPal)
      a = doubleOr (lyAlpha ly) 0.5
      -- ★ Phase 64 A3: 座標変換は projectCrossPoint に集約 (flipOnly / crossScale 撤去)。
      --   violin 幅は視覚 px 量のまま (polar では接線方向の px nudge = 半径不問の等幅)。
      coord  = lpCoord layout
      unit   = catUnitPx (lpCoord layout) layout
      subW   = unit * 0.9 / fromIntegral nColor
      halfWidth = subW * 0.4
      mkViolin (pix, cix, vals) =
        let loc = CrossAt (dodgeCenterD pix cix nColor)
            mkPt off y = projectCrossPoint coord layout loc off y
            color = colorFor cix
            ds = kdeGrid 30 vals
            maxD = if null ds then 1 else max 1e-9 (maximum (map snd ds))
            wScale d = halfWidth * d / maxD
            rightPath = [ mkPt (wScale d) y | (y, d) <- ds ]
            leftPath  = [ mkPt (negate (wScale d)) y | (y, d) <- reverse ds ]
        in case rightPath ++ leftPath of
             []     -> PRect (Rect 0 0 0 0) (FillStyle color a) Nothing
             (h':t) -> PPath (MoveTo h' : map LineTo t ++ [ClosePath])
                            (FillStyle color a) (Just (StrokeStyle color 1.0))
  in map mkViolin cells

-- | [日本語]: Violin: group ごとに 縦方向 KDE shape 描画。
--   [English]: A violin: draws a vertically-oriented KDE shape for each group.
renderViolin :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderViolin r layout pal ly
  | isJust (distDodgeRef ly) = renderViolinDodge r layout pal ly
renderViolin r layout _ ly =
  let groups = distGroupsOrdered layout r ly
      a = doubleOr (lyAlpha ly) 0.5
      c = staticColorOr ly "#3E6A6F"
      pal = lpCategoricalPalette layout
      area = lpPlotArea layout
      nG = length groups
      -- ★ Phase 36 B1c: 群なし (= 単一 violin) は categorical 軸が無いので renderBox と
      --   同じく plotArea 中央に 1 本・幅も plotArea 基準にする (= 左寄り回帰の防止)。
      hasCats = not (null (lpXCategoryLabels layout))
      -- Phase 8 A2 Step4c: 1 スロット幅を unit (sx 1 - sx 0) 基準に。 ±0.5 では rW/nG と
      -- 一致 (categorical span=nG) だが、 ±0.6 で span が変わっても軸スケールに追従する。
      -- ★ Phase 36 D1: markWidth (占有率・既定 0.7) で violin 幅、 nudge で slot 内 offset、
      --   side で片側化 (半 violin)。
      mwV     = doubleOr (lyMarkWidth ly) 0.7
      -- ★ Phase 36 D2: no-cat (単一) では slot = plotArea ゆえ nudge 基準も rW area
      --   (= strip / PS と統一・旧 catUnitPx 無条件は HS 内部不整合だった)。
      nudgePx = doubleOr (lyNudge ly) 0 * (if hasCats then catUnitPx coord layout else rW area)
      sideV   = maybe SideBoth id (getLast (lySide ly))
      halfWidth = (if hasCats then catUnitPx coord layout else rW area) * mwV / 2
      -- ★ Phase 64 A3: 座標変換は projectCrossPoint に集約 (flipOnly 撤去)。 violin 幅は
      --   視覚 px 量のまま (polar では接線方向 px nudge = 半径不問の等幅、 spine は radial)。
      coord = lpCoord layout
      locFor i = if hasCats then CrossAt (fromIntegral i) else CrossMid
      mkPt i off y = projectCrossPoint coord layout (locFor i) (nudgePx + off) y
      -- 各 group の violin shape (= 縦並び KDE、 共通 kdeGrid を左右対称展開)
      mkViolin i (_label, vals) =
        let color = pal !! (i `mod` length pal)
            color' = case staticColorOr ly "" of
                       "" -> color
                       given -> given
            _ = c -- silence
            ds = kdeGrid 30 vals
            maxD = if null ds then 1 else max 1e-9 (maximum (map snd ds))
            wScale d = halfWidth * d / maxD
            rightPath  = [ mkPt i (wScale d) y | (y, d) <- ds ]
            leftPath   = [ mkPt i (negate (wScale d)) y | (y, d) <- reverse ds ]
            centerDown = [ mkPt i 0 y | (y, _) <- reverse ds ]  -- 中心線 上→下
            centerUp   = [ mkPt i 0 y | (y, _) <- ds ]          -- 中心線 下→上
            -- side: 半 violin は片側 outline + 中心線で閉じる (raincloud の「雲」)。
            allPts = case sideV of
              SideBoth  -> rightPath ++ leftPath
              SideRight -> rightPath ++ centerDown
              SideLeft  -> centerUp  ++ leftPath
        in case allPts of
             []   -> PRect (Rect 0 0 0 0) (FillStyle color' a) Nothing
             (h':t) -> PPath (MoveTo h' : map LineTo t ++ [ClosePath])
                            (FillStyle color' a) (Just (StrokeStyle color' 1.0))
  in zipWith mkViolin (laneIndices layout groups) groups

-- | [日本語]: Strip plot: group ごとに 縦に scatter、 横 jitter で散らす
--   (= ggplot geom_jitter 流)。 jitter 幅は lyJitterX 指定 > 既定 (slot の 0.4)。
--   [English]: A strip plot: scatters points vertically per group, spread
--   horizontally with jitter (in the style of ggplot's geom_jitter). Jitter
--   width follows an explicit lyJitterX, falling back to 0.4 of the slot.
-- | [日本語]: dodge strip。 位置列 × 色列で各位置内に色サブグループの jitter を横並び
--   (= ggplot @position_jitterdodge@)。 jitter は sub-slot 幅基準。
--   [English]: A dodge strip. Given a position column and a color column,
--   arranges color sub-group jitter side by side within each position
--   (ggplot's @position_jitterdodge@). Jitter is scaled to the sub-slot width.
renderStripDodge :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderStripDodge r layout pal ly =
  let (_positions, colorCats, cells) = dodgeCells layout r ly
      nColor = max 1 (length colorCats)
      catPal = lpCategoricalPalette layout
      colorFor cix = if null catPal then tpDefault pal else catPal !! (cix `mod` length catPal)
      a  = doubleOr (lyAlpha ly) 0.7
      sz = doubleOr (lySize ly) (mmPt 1.25)
      -- ★ Phase 64 A3: 座標変換は projectCrossPoint に集約 (flipOnly / crossScale 撤去)。
      --   jitter は視覚 px 量のまま (polar では接線方向 px = 半径不問の等散らし)。
      coord  = lpCoord layout
      unit   = catUnitPx (lpCoord layout) layout
      subW   = unit * 0.9 / fromIntegral nColor
      jw     = subW * 0.6
      mkPts (pix, cix, vals) =
        let loc = CrossAt (dodgeCenterD pix cix nColor)
            color = colorFor cix
        in [ PCircle (projectCrossPoint coord layout loc dx v) (sz/2)
                     (FillStyle color a) Nothing Nothing
           | (k, v) <- zip [0 :: Int ..] vals
           , let dx = (hashRand ((pix * 17 + cix) * 131 + k * 71) - 0.5) * jw ]
  in concatMap mkPts cells

renderStrip :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderStrip r layout pal ly
  | isJust (distDodgeRef ly) = renderStripDodge r layout pal ly
renderStrip r layout pal ly =
  let groups = distGroupsOrdered layout r ly
      nG = length groups
      c0 = staticColorOr ly (tpDefault pal)
      a  = doubleOr (lyAlpha ly) 0.7
      sz = doubleOr (lySize ly) (mmPt 1.25)
      area = lpPlotArea layout
      cats = lpCategoricalPalette layout
      -- ★ Phase 36 B1c: 群なし (単一 strip) は plotArea 中央・幅も plotArea 基準。
      hasCats = not (null (lpXCategoryLabels layout))
      -- jitter 幅 (px): lyJitterX 指定 (>0) があれば plotArea 比率、 なければ slot 幅の 0.4
      -- Phase 10 A4-fix: slot 幅は coord に応じた cross 軸単位 (flip では縦)。
      slotW = if hasCats then catUnitPx (lpCoord layout) layout else rW area
      jx0 = doubleOr (lyJitterX ly) 0
      -- ★ Phase 36 D1: markWidth (jitter span・占有率・既定 0.4) で散らし幅、 nudge で slot 内 offset。
      mwS     = doubleOr (lyMarkWidth ly) 0.4
      nudgePx = doubleOr (lyNudge ly) 0 * slotW
      jw = if jx0 > 0 then jx0 * rW area else slotW * mwS
      -- ★ Phase 64 A3: 座標変換は projectCrossPoint に集約 (flipOnly 撤去)。
      --   jitter は視覚 px 量のまま (polar では接線方向 px = 半径不問の等散らし)。
      coord = lpCoord layout
      locFor i = if hasCats then CrossAt (fromIntegral i) else CrossMid
      mkPt i off v = projectCrossPoint coord layout (locFor i) (nudgePx + off) v
      mkPts i (_, vals) =
        let color = if c0 == tpDefault pal then cats !! (i `mod` length cats) else c0
        in [ PCircle (mkPt i dx v) (sz/2)
                    (FillStyle color a) Nothing Nothing
           | (k, v) <- zip [0 :: Int ..] vals
           , let dx = (hashRand (i * 131 + k * 71) - 0.5) * jw ]
  in concat (zipWith mkPts (laneIndices layout groups) groups)

-- | [日本語]: Beeswarm の横 offset 計算: 値を pixel y にマップ後、 点直径ごとに
--   y ビンを切り、 各ビン内で点を中央から左右対称に並べる (= 1,-1,2,-2,... 列)。
--   N に対し安定で、 横幅は maxOff で clamp (= はみ出さない)。 戻り値は各点の dx (px)。
--   HS/PS 共通アルゴリズム。 入力 ys は pixel y 値 (sy 適用後)。
--   [English]: Computes beeswarm horizontal offsets: after mapping values to
--   pixel y, cuts y bins one point-diameter wide and, within each bin, arranges
--   points symmetrically outward from the center (the sequence 1,-1,2,-2,...).
--   Stable with respect to N, and the width is clamped by maxOff (never
--   overflows). Returns each point's dx (px). A shared HS/PS algorithm. The
--   input ys are pixel y values (after applying sy).
beeswarmOffsets :: Double -> Double -> [Double] -> [Double]
beeswarmOffsets diameter maxOff ysPix =
  let binH = diameter
      -- 各点に y ビン index を付与し、 同ビン内の出現順を数える
      go _    [] = []
      go seen (y : rest) =
        let b      = floor (y / binH) :: Int
            cnt    = maybe 0 id (lookup b seen)
            -- 中央から左右対称: 0, +1, -1, +2, -2, ...
            slot   = if even cnt then cnt `div` 2 else negate ((cnt + 1) `div` 2)
            dxRaw  = fromIntegral slot * diameter
            dx     = max (negate maxOff) (min maxOff dxRaw)
            seen'  = (b, cnt + 1) : filter ((/= b) . fst) seen
        in dx : go seen' rest
  in go [] ysPix

-- | [日本語]: Swarm plot: strip の衝突回避版 (beeswarm)。 値の近い点を
--   横方向に左右対称へ押し出して重なりを避ける。 N 大でも横幅 clamp で破綻しない。
--   [English]: A swarm plot: the collision-avoiding variant of strip (a
--   beeswarm). Points with close values are pushed apart symmetrically to
--   avoid overlap. Stays well-behaved even for large N thanks to the width clamp.
-- | [日本語]: dodge swarm。 位置列 × 色列で各位置内に色サブグループの beeswarm を横並び。
--   [English]: A dodge swarm. Given a position column and a color column,
--   arranges color sub-group beeswarms side by side within each position.
renderSwarmDodge :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderSwarmDodge r layout pal ly =
  let (_positions, colorCats, cells) = dodgeCells layout r ly
      nColor = max 1 (length colorCats)
      catPal = lpCategoricalPalette layout
      colorFor cix = if null catPal then tpDefault pal else catPal !! (cix `mod` length catPal)
      a  = doubleOr (lyAlpha ly) 0.85
      sz = doubleOr (lySize ly) (mmPt 1.25)
      -- ★ Phase 64 A3: 座標変換は projectCrossPoint / valueAxisPx に集約
      --   (flipOnly / crossScale 撤去)。 beeswarm の押し出しは重なり回避のための
      --   視覚 px 量なので px のまま (polar では接線方向 px nudge)。
      coord  = lpCoord layout
      unit   = catUnitPx (lpCoord layout) layout
      subW   = unit * 0.9 / fromIntegral nColor
      maxOff = subW * 0.45
      mkPts (pix, cix, vals) =
        let loc = CrossAt (dodgeCenterD pix cix nColor)
            color = colorFor cix
            sortedVals = sort vals
            ysPix = map (valueAxisPx coord layout) sortedVals
            offs  = beeswarmOffsets sz maxOff ysPix
        in [ PCircle (projectCrossPoint coord layout loc off v) (sz/2)
                     (FillStyle color a) Nothing Nothing
           | (v, off) <- zip sortedVals offs ]
  in concatMap mkPts cells

renderSwarm :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderSwarm r layout pal ly
  | isJust (distDodgeRef ly) = renderSwarmDodge r layout pal ly
renderSwarm r layout pal ly =
  let groups = distGroupsOrdered layout r ly
      c0 = staticColorOr ly (tpDefault pal)
      a  = doubleOr (lyAlpha ly) 0.85
      sz = doubleOr (lySize ly) (mmPt 1.25)
      area = lpPlotArea layout
      -- ★ Phase 36 B1c: 群なし (単一 swarm) は plotArea 中央・押し出し幅も plotArea 基準。
      hasCats = not (null (lpXCategoryLabels layout))
      -- Phase 10 A4-fix: 押し出し幅は coord に応じた cross 軸単位 (flip では縦) の 0.4。
      -- ★ Phase 36 D1: markWidth (span・占有率・既定 0.8) で押し出し幅、 nudge で slot 内 offset。
      slotW   = if hasCats then catUnitPx (lpCoord layout) layout else rW area
      mwSw    = doubleOr (lyMarkWidth ly) 0.8
      nudgePx = doubleOr (lyNudge ly) 0 * slotW
      maxOff = slotW * mwSw / 2
      cats = lpCategoricalPalette layout
      -- ★ Phase 64 A3: 座標変換は projectCrossPoint / valueAxisPx に集約 (flipOnly と
      --   旧非網羅 case (CoordCartesian/CoordFlip のみ) を撤去)。 beeswarm の binning は
      --   value 軸 px 上 (polar は半径 px / 外周弧長 px)、 押し出しは接線方向 px nudge。
      coord = lpCoord layout
      locFor i = if hasCats then CrossAt (fromIntegral i) else CrossMid
      mkPt i off v = projectCrossPoint coord layout (locFor i) (nudgePx + off) v
      mkPts i (_, vals) =
        let color = if c0 == tpDefault pal then cats !! (i `mod` length cats) else c0
            sortedVals = sort vals
            ysPix = map (valueAxisPx coord layout) sortedVals
            offs  = beeswarmOffsets sz maxOff ysPix
        in [ PCircle (mkPt i off v) (sz/2)
                    (FillStyle color a) Nothing Nothing
           | (v, off) <- zip sortedVals offs ]
  in concat (zipWith mkPts (laneIndices layout groups) groups)

-- | [日本語]: Raincloud plot: 群ごとに 右:half-violin + 中央:box + 左:jitter strip。
--   参照画像 (raincloud_ref.webp) 準拠。 ggplot 流に「3 つの geom を重ねる」 構成とし、
--   KDE/四分位は共通 helper ('kdeGrid' / 'boxAt') を再利用 (= violin/box とロジック重複なし)。
--   box は KDE の baseline (cx) と重ならないよう左にオフセットして配置。
--   [English]: A raincloud plot: per group, a half-violin on the right, a box
--   in the middle, and a jitter strip on the left. Follows the reference image
--   (raincloud_ref.webp). Built by layering three geoms, ggplot-style; the KDE
--   and quartiles reuse the shared helpers ('kdeGrid' / 'boxAt') so there is no
--   logic duplicated with violin/box. The box is offset to the left so it does
--   not overlap the KDE baseline (cx).
renderRaincloud :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderRaincloud r layout _ ly =
  let groups    = distGroupsOrdered layout r ly
      area      = lpPlotArea layout
      nG        = length groups
      sx        = scaleApply (lpXScale layout)
      sy        = scaleApply (lpYScale layout)
      pal       = lpCategoricalPalette layout
      -- ★ Phase 36 B1c: 群なし (単一 raincloud) は plotArea 中央・幅も plotArea 基準。
      hasCats   = not (null (lpXCategoryLabels layout))
      -- ★ Phase 64 A4: 自前 sx/sy を投影層へ。 cross 位置は 'CrossLoc'、 3 部位の
      --   横ずらしは px offset として渡す。 直線座標系は旧式と bit 一致
      --   (catUnitPx CoordCartesian == sx 1 - sx 0)、 極座標では雲/雨/箱が
      --   接線方向に並ぶ。 halfWidthD は極座標用の data 単位半幅。
      coord     = lpCoord layout
      halfWidth = if hasCats then catUnitPx coord layout * 0.35 else rW area * 0.35
      spanXD    = lsDomainHi (lpXScale layout) - lsDomainLo (lpXScale layout)
      halfWidthD = 0.35 * (if hasCats then 1 else spanXD)
      locFor i  = if hasCats then CrossAt (fromIntegral i) else CrossMid
      sz        = doubleOr (lySize ly) (mmPt 1.25)
      jAlpha    = doubleOr (lyAlpha ly) 0.6
      mkOne i (_label, vals) =
        let loc = locFor i
            pt off v = projectCrossPoint coord layout loc off v
            color = case staticColorOr ly "" of
                      ""    -> pal !! (i `mod` length pal)
                      given -> given
            -- (1) 右半身 violin (= 「雲」、 共通 kdeGrid を baseline から右へ)
            grid = kdeGrid 30 vals
            violinPrims = case grid of
              [] -> []
              _  -> let dMax     = max 1e-9 (maximum (map snd grid))
                        rightPts = [ pt ((d / dMax) * halfWidth) v | (v, d) <- grid ]
                        basePts  = reverse [ pt 0 v | (v, _) <- grid ]
                    in case rightPts ++ basePts of
                         (p0:rest) -> [ PPath (MoveTo p0 : map LineTo rest ++ [ClosePath])
                                              (FillStyle color 0.4) (Just (StrokeStyle color 1.0)) ]
                         []        -> []
            -- (2) box (= 共通 boxAtCross)。 KDE baseline と離すため左に halfWidth*0.32 寄せる
            boxOff = negate (halfWidth * 0.32)
            boxPrims = boxAtCross coord layout loc boxOff 3 (halfWidthD * 0.06) color vals
            -- (3) 左 jitter strip (= 「雨」、 box より更に左、 hashRand で deterministic)
            stripOff = negate (halfWidth * 0.7)
            stripPrims = [ PCircle (pt (stripOff + dx) v) (sz / 2)
                                   (FillStyle color jAlpha) Nothing Nothing
                         | (k, v) <- zip [0 :: Int ..] vals
                         , let dx = (hashRand (i * 97 + k * 131) - 0.5) * halfWidth * 0.5 ]
        in violinPrims ++ boxPrims ++ stripPrims
  in concat (zipWith mkOne [0..] groups)

-- | [日本語]: Ridge plot / joyplot。 群ごとに density 曲線を描き、 値方向に山を並べて少し重ねる。
--   他 distribution mark と統一し encY=値・群=distGroupRef (encX ?? colorBy)。
--   ridge は値→x・群→y の向きが要るため Layout が coord_flip を自動適用 ('Graphics.Hgg.Spec.Setters.ridgeAutoFlip')。
--   よって値→x は 'lpYScaleFlipped'、 群→y baseline は 'lpXScaleFlipped' を使う (box-flip と同機構)。
--   軸/目盛/群ラベルは標準 path が描き、 ここは glyph (群ごと 1 PPath) のみ。 重なり headroom は
--   Layout が群 (= flip 後 y) カテゴリドメインを上方向へ expand して確保。 KDE は 'kdeGridOver' を共有。
--   [English]: A ridge plot / joyplot. Draws a density curve per group and
--   lines up the peaks along the value axis with a slight overlap. Kept
--   consistent with the other distribution marks: encY is the value, and the
--   group is distGroupRef (encX, falling back to colorBy). Because ridge needs
--   the value going to x and the group going to y, Layout auto-applies
--   coord_flip ('Graphics.Hgg.Spec.Setters.ridgeAutoFlip'); accordingly value-to-x uses 'lpYScaleFlipped'
--   and the group-to-y baseline uses 'lpXScaleFlipped' (the same mechanism as
--   box-flip). The axes/ticks/group labels are drawn by the standard path;
--   this function draws only the glyphs (one 'PPath' per group). Overlap
--   headroom is secured by Layout expanding the group (post-flip y) category
--   domain upward. The KDE evaluation is shared via 'kdeGridOver'.
renderRidge :: Resolver -> Layout -> ThemePalette -> Layer -> [Primitive]
renderRidge r layout _thePal ly =
  let vals = V.toList (vecOr (lyEncY ly) r)   -- 値 (encY)
      -- 群ラベル列 = distGroupRef (encX ?? colorBy)。 無ければ単一 ("") = density 風。
      grpLabels = case distGroupRef ly of
        Just crG -> case resolveCol r crG of
          Just (TxtData v) -> map Just (V.toList v)
          Just (NumData v) -> map (Just . T.pack . show . (round :: Double -> Int)) (V.toList v)
          _                -> []
        Nothing -> []
      hasGrp = not (null grpLabels)
      pairs = if hasGrp then [ (g, v) | (Just g, v) <- zip grpLabels vals ]
                        else [ ("", v) | v <- vals ]
      groups0 = let uniq = foldr (\(l, _) acc -> if l `elem` acc then acc else l : acc) [] pairs
                in [ (l, [v | (lv, v) <- pairs, lv == l]) | l <- uniq ]
      -- 群 (= flip 後 y) カテゴリ軸順 (lpXCategoryLabels) に整列して baseline と軸ラベルを一致。
      xls = lpXCategoryLabels layout
      groups = if null xls then groups0
               else [ (g, vs) | g <- xls, Just vs <- [lookup g groups0] ]
      a = doubleOr (lyAlpha ly) 0.8
      area = lpPlotArea layout
      pal = lpCategoricalPalette layout
      vx v = scaleApply (lpYScaleFlipped layout) v        -- 値 → x (flip 済・連続)
      gyc i = scaleApply (lpXScaleFlipped layout) (fromIntegral i)  -- 群 index → y baseline
      allVals = concatMap snd groups
      (vLo, vHi) = if null allVals then (0, 1) else (minimum allVals, maximum allVals)
      -- 山高さ = 群 1 スロット (px) の 0.95。 群なしは plotArea 全高 baseline=下端 (density 風)。
      slotY  = if hasGrp then abs (gyc 1 - gyc 0) else rH area
      ridgeH = if hasGrp then slotY * 0.95 else rH area * 0.9
      baseOf i = if hasGrp then gyc (fromIntegral i) else rY area + rH area
      mkRidge i (_label, gvals) =
        let color = pal !! (i `mod` length pal)
            -- Phase 8 B23-fix: 全群共通の値域 (vLo, vHi) で評価し裾を滑らかに減衰させる。
            ds = kdeGridOver vLo vHi 60 gvals
            maxD = if null ds then 1 else max 1e-9 (maximum (map snd ds))
            yBase = baseOf i
            pts = [ Point (vx xv) (yBase - ridgeH * d / maxD) | (xv, d) <- ds ]
        in case (pts, ds) of
             (p0:rest, _) ->
               let closure = [ Point (vx (fst (last ds))) yBase
                             , Point (vx (fst (head ds))) yBase ]
               in [ PPath (MoveTo p0 : map LineTo (rest ++ closure) ++ [ClosePath])
                          (FillStyle color a) (Just (StrokeStyle color 1.2)) ]
             _ -> []
      -- 上の群 (index 大) を先に = 奥、 下 (index 0) を後 = 手前 (重なりの前後)
      ordered = reverse (zip [0..] groups)
  in concatMap (\(i, g) -> mkRidge i g) ordered
