-- | 03-decoration.md の図 (ラベル / scale / theme / facet / subplot / 座標 /
--   参照線 / 注釈 / 重畳)。
{-# LANGUAGE OverloadedStrings #-}
module DocFig.Decoration (figures) where

import           Data.Text     (Text)
import qualified Data.Vector  as V
import           DocFig.Common

figures :: [Figure]
figures =
  -- Lesson 4: 重畳 (scatter + 回帰直線 overlay)
  [ fig "lesson4-overlay.svg" $
         purePlot
      <> layer (scatter (inline oxs) (inline oys) <> alpha 0.85 <> size 6)
      <> layer (line    (inline oxs) (inline ofit) <> color (fromHex "#dc2626") <> stroke 2)
      <> title "Lesson 4: 重畳 (散布 + 回帰直線)" <> xLabel "x" <> yLabel "y"

    -- 3c タイトル・ラベル: labs を一括
  , fig "s3c-labs.svg" $
         purePlot
      <> layer (scatter xs ys <> size 6)
      <> labs (emptyLabs
           { labsTitle = Just "3c. title"
           , labsSubtitle = Just "subtitle (副題)"
           , labsCaption = Just "caption (脚注)"
           , labsX = Just "x 軸", labsY = Just "y 軸" })

    -- 3d scale: 連続色 gradient (scaleColorGradient2)
  , fig "s3d-scale.svg" $
         purePlot
      <> layer (scatter xs ys <> colorContinuousBy cz <> size 9)
      <> scaleColorGradient2 "#2166AC" "#F7F7F7" "#B2182B" 3.0
      <> legend
      <> title "3d. scaleColorGradient2 (連続色)"

    -- 3e theme: ThemeDark
  , fig "s3e-theme.svg" $
         purePlot
      <> layer (scatter xs ys <> color (fromHex "#38bdf8") <> size 6)
      <> theme ThemeDark
      <> title "3e. theme ThemeDark"

    -- 3e theme gallery: 代表テーマ全 13 種を 1 枚に
  , figW "s3e-theme-gallery.svg" 1280 1040 $
         subplots (map themePanel allThemes) <> subplotCols 4
      <> title "theme 一覧 (代表テーマ 13 種)"

    -- 3e theme × subplots: 外側に theme を足すと全 panel に効く
  , figW "s3e-theme-subplots.svg" 960 400 $
         subplots [ layer (scatter xs ys <> color (fromHex "#38bdf8") <> size 5) <> title "散布"
                  , layer (bar cats vals) <> title "棒" ]
      <> subplotCols 2 <> theme ThemeDark
      <> title "subplots <> theme ThemeDark (外側 theme が全 panel に伝播)"

    -- 3f facet: facetWrap で群ごとに小分け (facet 列は名前解決が要るため Resolver 版)
  , figR "s3f-facet.svg" rFacet $
         purePlot
      <> layer (scatter "x" "y" <> colorBy "g" <> size 6)
      <> facetWrap "g" 2
      <> title "3f. facetWrap"

    -- 3f-2 subplot: 独立図の並置 (subplots) ─ geom も軸も別でよい
  , fig "s3f2-subplot.svg" $
         subplots [ layer (scatter (inline [1,2,3,4,5]) (inline [2,4,3,5,4])) <> title "散布"
                  , layer (line    (inline [1,2,3,4,5]) (inline [1,3,2,4,3])) <> title "折れ線"
                  , layer (bar cats vals)                                     <> title "棒" ]
      <> subplotCols 3
      <> title "3f-2. subplots ─ 独立図の並置"

    -- 3f-2 selectPanels (Phase 18 A1): repeatFields で量産した panel から名前で選択
  , fig "s3f2-select-panels.svg" $
         repeatFields ["a", "b", "c", "d"] selPanelOf
      <> selectPanels ["c", "a"]
      <> subplotCols 2
      <> title "3f-2. selectPanels ─ panel の名前選択 (c, a の順)"

    -- 3d discrete limits (Phase 18 A2): forest の行を選択 + 列挙順に並べ替え
  , fig "s3d-discrete-limits.svg" $
         layer (forest (inlineCat (["b0", "b1", "b2", "b3", "sigma"] :: [Text]))
                       (inline [1.0, 2.0, 0.1, -0.45, 0.3])
                       (inline [0.2, 0.4, 0.3, 0.1, 0.05])
                <> forestNull 0)
      <> scaleYDiscreteLimits ["b1", "b0", "sigma"]
      <> title "3d. scaleYDiscreteLimits ─ forest の行選択 + 並べ替え"

    -- 3f-2 nested: 入れ子 subplots (B1) ─ 主図 + 周辺分布を入れ子グリッドで
  , fig "s3f2-nested.svg" $
         subplots [ layer (scatter nx ny) <> title "主図 (x vs y)"
                  , subplots [ layer (histogram nx) <> title "x 分布"
                             , layer (histogram ny) <> title "y 分布" ]
                      <> subplotCols 1 <> title "周辺分布 (入れ子)" ]
      <> subplotCols 2
      <> title "3f-2. nested subplots ─ 主図 + 入れ子グリッド"

    -- 3f-2 concat: 演算子 <-> / <:> での非対称合成 ─ (a <-> b <-> c) <:> d
  , fig "concat.svg" $
         ((cA <-> cB <-> cC) <:> cD)
      <> title "3f-2. (a <-> b <-> c) <:> d (横3列 + 全幅)"

    -- 3g coord: coordFlip で横棒
  , fig "s3g-coord.svg" $
         purePlot <> layer (bar cats vals)
      <> coordFlip
      <> title "3g. coordFlip (横棒)" <> xLabel "群" <> yLabel "値"

    -- 3h 補助: 参照線 + 凡例
  , fig "s3h-guides.svg" $
         purePlot
      <> layer (scatter xs ys <> colorBy gs <> size 6)
      <> refHorizontal 2.5
      <> refVertical 2.5
      <> legend
      <> title "3h. refHorizontal / refVertical + legend"

    -- 03 scale: 既製パレット okabeIto
  , fig "s3d-palette-okabe.svg" $
         purePlot <> layer (scatter xs ys <> colorBy gs <> size 7) <> palette okabeIto <> legend
      <> title "palette okabeIto (色覚多様性対応)"

    -- 03 theme: 要素単位の部分上書き
  , fig "s3e-theme-override.svg" $
         purePlot <> layer (scatter xs ys <> size 6)
      <> theme ThemeMinimal
      <> titleFont (fontSize 18 <> fontWeight "bold")
      <> tickFont  (fontSize 10 <> fontColor "#64748b")
      <> panelBorder True <> themeGrid False <> themeAxisTextAngle 45
      <> title "theme 要素の部分上書き"

    -- 03 theme × facet strip
  , figR "s3e-theme-strip.svg" rFacet $
         purePlot <> layer (scatter "x" "y" <> colorBy "g") <> facet "g"
      <> themeStrip True <> stripFill "#eef2ff"
      <> title "facet strip 背景 (stripFill)"

    -- 03 guides: 軸の細かい制御 (log + 範囲 + 回転)
  , fig "s3h-axis.svg" $
         purePlot <> layer (scatter axx axy <> size 6)
      <> xAxis (logAxis <> axisRange 1 1000 <> axisRotate 45)
      <> yAxis (axisRange 0 100)
      <> title "xAxis (logAxis <> axisRange <> axisRotate)"

    -- 03 guides: 第 2 軸 (右 y)
  , figR "s3h-second-axis.svg" rSec $
         purePlot
      <> layer (line "t" "price" <> color (fromHex "#1f77b4") <> stroke 2)
      <> layer (line "t" "volume" <> toRightY <> color (fromHex "#d62728") <> stroke 2)
      <> yAxisRight (axisRange 0 1000000)
      <> title "第 2 軸 (price=左 / volume=右)"

    -- 03 guides: 注釈
  , fig "s3h-annotate.svg" $
         purePlot <> layer (scatter xs ys <> size 6)
      <> annotText 2.0 3.6 "注釈"
      <> annotArrow 1.4 3.0 1.95 3.5
      <> annotRect 3.0 1.0 4.0 2.0 "領域"
      <> marginalX
      <> insetAt 0.72 0.1 0.25 0.25 (layer (histogram xs))
      <> title "annotText / annotArrow / annotRect / marginalX / insetAt"

    -- 高度な図: 多数の設定を一枚に
  , fig "advanced.svg" $
         purePlot
      <> layer ( scatter (inline axs) (inline ays)
                 <> colorContinuousBy (inline azs)
                 <> sizeBy (inline asz)
                 <> alpha 0.85 )
      <> layer ( line (inline axs) (inline afit)
                 <> color (fromHex "#ef4444") <> stroke 2 )
      <> scaleSize 4 16
      <> refHorizontal 1.0
      <> theme ThemeMinimal
      <> legend
      <> labs (emptyLabs
           { labsTitle    = Just "高度な図: 連続色 + サイズ + 回帰 + 参照線"
           , labsSubtitle = Just "colorContinuousBy / sizeBy / line overlay / refHorizontal / theme"
           , labsCaption  = Just "hgg ─ <> で設定を積層"
           , labsX        = Just "x"
           , labsY        = Just "y" })
  ]
  where
    xs = inline    [1,2,3,4, 1,2,3,4]
    ys = inline    [2,3,1,4, 3,1,4,2]
    gs = inlineCat (concatMap (replicate 4) (["alpha","beta"] :: [Text]))
    cats = inlineCat (["A","B","C"] :: [Text])
    vals = inline [3.0, 7.0, 5.0]
    cz = inline [1.0, 2.5, 4.0, 1.5, 3.0, 4.5, 2.0, 3.5]

    oxs = [1,2,3,4,5,6,7,8,9,10] :: [Double]
    oys = [1.2, 1.9, 3.4, 3.1, 5.2, 5.0, 6.8, 7.3, 8.1, 9.4]
    (oa, ob) = linFit oxs oys
    ofit   = [ oa * x + ob | x <- oxs ]

    themePanel (nm, th) =
      layer (scatter xs ys <> color (fromHex "#3b82f6") <> size 5) <> theme th <> title nm
    allThemes :: [(Text, ThemeName)]
    allThemes =
      [ ("Default", ThemeDefault), ("Minimal", ThemeMinimal), ("Dark", ThemeDark)
      , ("Light", ThemeLight), ("Grey", ThemeGrey), ("BW", ThemeBW)
      , ("Classic", ThemeClassic), ("Void", ThemeVoid), ("Linedraw", ThemeLinedraw)
      , ("Noir", ThemeNoir), ("Lumen", ThemeLumen)
      , ("Parchment", ThemeParchment), ("ParchmentDark", ThemeParchmentDark) ]

    selPanelOf f = layer (scatter (inline [1,2,3,4,5 :: Double])
                                  (inline [2,4,3,5,4 :: Double]))
                   <> title f

    nx = inline [ sin (0.5 * fromIntegral i) + 0.15 * fromIntegral (i `mod` 7)
                | i <- [0 .. 60 :: Int] ]
    ny = inline [ cos (0.4 * fromIntegral i) | i <- [0 .. 60 :: Int] ]

    cA = layer (scatter (inline [1,2,3,4,5]) (inline [1,4,9,16,25]))            <> title "x²"
    cB = layer (line    (inline [1,2,3,4,5]) (inline [2,4,8,16,32]))            <> title "2^x"
    cC = layer (bar (inlineCat (["a","b","c","d"] :: [Text])) (inline [3,7,5,9])) <> title "bar"
    cD = layer (scatter (inline [0,1,2,3,4]) (inline [0,3,1,4,2]))              <> title "full-width row"

    axx = inline ([1,3,10,30,100,300,1000] :: [Double])
    axy = inline ([3,7,12,20,33,52,78]     :: [Double])

    n      = 60 :: Int
    axs    = [ fromIntegral i * 0.18 | i <- [0 .. n - 1] ] :: [Double]
    noise  = take n (lcg 12345)
    ays    = [ 0.55 * x + 1.0 + (e - 0.5) * 2.2 | (x, e) <- zip axs noise ]
    azs    = ays                       -- 連続色は y 値で
    asz    = [ 0.5 + e | e <- noise ]  -- サイズは別の擬似量
    (fa, fb) = linFit axs ays
    afit     = [ fa * x + fb | x <- axs ]

    rFacet :: Resolver
    rFacet "x" = Just (NumData (V.fromList [1,2,3,4, 1,2,3,4]))
    rFacet "y" = Just (NumData (V.fromList [2,3,1,4, 3,1,4,2]))
    rFacet "g" = Just (TxtData (V.fromList (concatMap (replicate 4) ["A","B"])))
    rFacet _   = Nothing

    rSec :: Resolver
    rSec "t"      = Just (NumData (V.fromList [1,2,3,4,5,6,7,8,9,10]))
    rSec "price"  = Just (NumData (V.fromList [10,11,13,12,15,16,15,18,17,20]))
    rSec "volume" = Just (NumData (V.fromList [3e5,5e5,4e5,7e5,6e5,9e5,8e5,7e5,9e5,1e6]))
    rSec _        = Nothing
