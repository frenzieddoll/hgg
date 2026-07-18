-- |
-- Module      : Hgg.Plot.Spec
-- Description : Layer 3 ─ VisualSpec / Layer / ColRef + Monoid (Phase 26 §A-2)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 設計方針 (詳細: design/api-style-discussion-2.md + 続き):
--
--   * 2 階層 Monoid: 'Layer' (= 1 layer 内属性) と 'VisualSpec' (= 図全体)
--   * 全 helper が `<>` で paren 無し合成可能 (= plotnine 風)
--   * 'ColRef' で「文字列 col 参照」 と「Vector inline」 両対応、
--     ('OverloadedStrings' で `"weight" :: ColRef` が自動 'ColByName')
--   * core は DataFrame 型に非依存。 col 名 → Vector 解決は 'Resolver'
--     callback で render 時に行う (= core 内ではデータ source を持たない)
--   * Generic + ToJSON/FromJSON で Spec 全体が **JSON serializable**
--     (= frontend ↔ backend 間で共有し、 差分 Patch を送るユースケースを想定)
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Hgg.Plot.Spec
  ( -- * ColRef + Resolver
    ColRef(..)
  , ColData(..)
  , Resolver
  , emptyResolver
  , resolveCol
  , bakeSpec
  , resolveNum
  , resolveTxt
  , colRefName
    -- * Inline column conversion
  , Numeric(..)
  , Categorical(..)
  , inline
  , inlineCat
    -- * Layer
  , Layer(..)
  , MarkKind(..)
  , ColorEnc(..)
    -- ** 固定色 'Color' 型 (re-export from "Hgg.Plot.Color")
  , Color(..)
  , rgb
  , fromHex
  , fromHexMaybe
  , fromHexA
  , fromHexAMaybe
  , ConnectSpec(..)
  , defaultConnectSpec
    -- * 2D 点 'Point2' (= 3D 'Hgg.Plot.ThreeD.Types.Point3' と対称)
  , Point2(..)
    -- * Phase 51: custom mark (拡張可能な描画語彙)
  , RenderCtx(..)
  , CustomMark(..)
  , customMark
  , customMarkWith
  , encX
  , encY
    -- * Layer constructors (= Layer 返却)
  , scatter
  , line
  , scatterPoints       -- ★ Phase 30 A7 inline [Point2] (= 3D scatter3DPoints と対称)
  , linePoints          -- ★ Phase 30 A7 inline [Point2] (= 3D line3DPoints と対称)
  , bar
  , quiver
  , arrowScale
  , arrowColorByMagnitude
  , text
  , label
  , qq
  , ecdf
  , lineRange
  , pointRange
  , crossbar
  , statFunction
  , histogram
  , freqpoly             -- ★ Ch10 EDA (Phase 28) geom_freqpoly (= 頻度多角形)
  , countXY              -- ★ Ch10 EDA (Phase 28) geom_count (= 2 カテゴリ件数)
  , histogramWide        -- ★ P1 (Phase 6 A10) wide-form 重ね
  , autocorr             -- ★ P19 (Phase 6 A4) MCMC autocorrelation
  , autocorrMaxLag       -- ★ Phase 6 A4 max lag override
  , ess                  -- ★ P20 (Phase 6 A5) Effective Sample Size
  , chain                -- ★ Phase 6 A5 chain group 設定
  , forest               -- ★ Phase 6 A2 Forest plot (= horizontal CI bar)
  , forestNull           -- ★ Phase 6 A2 null effect 位置 (= 縦 0 線、 default 0)
  , funnel               -- ★ Phase 6 A3 Funnel plot (= effect vs SE)
  , boxplot
  , groupBy             -- ★ Phase 36: 群配置チャネル (色なし・ggplot aes(group=))
  , density
  , densityNorm
  , pie
  , waterfall
  , heatmap
  , contour
  , contourFilled
  , contourLevels
  , contourBreaks
  , bin2d
  , bin2dCount           -- ★ Ch10 EDA (Phase 28) geom_bin2d (count 版)
  , tile                 -- ★ Phase 60: 連続軸タイル塗り (geom_tile/raster・決定境界)
  , hexbin               -- ★ Phase 40: 六角ビニング (geom_hex / matplotlib hexbin)
  , hexbinBins           -- ★ Phase 40: hexbin の x 方向セル分割数 (既定 30)
  , HexCell(..)          -- ★ Phase 40: 六角セル (中心+件数+頂点)
  , hexbinCells          -- ★ Phase 40: 六角ビニング純関数 (d3-hexbin)
  , hexbinLayerCells     -- ★ Phase 40: Layer 解決版 (render/colorbar 共有)
  , subplots
  , subplotCols
  , selectPanels
  , selectedSubplots
  , scaleXDiscreteLimits
  , scaleYDiscreteLimits
  , applyDiscreteLimits
  , hconcat
  , vconcat
  , (<->)
  , (<:>)
  , repeatFields
  , pairs
  , step
  , stem
  , statLm
  , statLmLevel
  , statSmooth
  , statSmoothCI
  , statPoly
  , statResid
  , band
  , stream
  , violin
  , strip
  , swarm
  , raincloud
  , (<+>)
  , distCols
  , compositeLanes
  , ridge
  , ridgeAutoFlip
  , jitterX
  , jitterY
  , sqrtAxis
  , timeAxis
  , AxisBreak(..)
  , axisBreak
  , axisBreaksAt
  , axisTickLabels
  , axisBreaksLabeled
  , hideTicks
  , Annotation(..)
  , annotate
  , annotText
  , annotTextP
  , annotArrow
  , annotArrowP
  , annotRect
  , annotRectP
  , annotLine
  , annotLineP
  , Inset(..)
  , inset
  , insetAt
  , insetElement
  , palette
  , paletteGGplot
  , continuousPalette
  , scaleColorManual
  , scaleColorGradient2
  , scaleSize
  , YAxisSide(..)
  , yAxisRight
  , toRightY
  , toLeftY
  , statMean
  , statMedian
  , parallelCoords
  , dag
  , dagFromLists
  , dagFromListsWithPlates
  , dagNode
  , dagNodeDist
  , dagEdge
  , trace
  , traceLines
    -- * Layer-local attribute (= Layer 返却)
  , color
  , colorRGBA
  , colorRGBAMaybe
  , colorBy
  , distGroupRef
  , distDodgeRef
  , colorContinuousBy
  , alpha
  , size
  , stroke
  , edgeOn
  , edge
  , edgeWidth
  , hoverCols
  , errorX
  , errorY
  , connect
  , connectOrder
  , connectGroup
  , connectColor
  , connectWidth
    -- * Axis (= Phase 26 §C-2 #1 / #2)
  , AxisSpec(..)
  , AxisKind(..)
  , AxisFormat(..)
  , axisKindOf
  , axisFormatOf
  , axTickValsOf
  , axTickLabelsOf
  , axisRotateOf
  , resolveAxisAngle
  , axisShowTicksOf
  , axisRotate
  , linearAxis
  , logAxis
  , axisFormat
  , axisMin
  , axisMax
  , axisRange
  , binCount
  , binWidth
  , histBinning
  , histogramDensity
  , histBorder
  , densityFill
  , hollow
    -- * 分布 mark の位置決め (= Phase 36 D1)
  , Side(..)
  , nudge
  , markWidth
  , side
    -- * Bar position adjustment (= Phase 9 B)
  , Position(..)
  , position
  , Coord(..)
  , coordFlip
  , coordPolar
  , coordPolarY
  , reverseX
  , reverseY
  , coordCartesianX
  , coordCartesianY
  , coordCartesian
    -- * Reference line (= Phase 26 §C-2 #3)
  , ReferenceLine(..)
    -- * Marginal histogram (= Phase 26 §C-2 #10)
  , MarginalSpec(..)
  , MarginalKind(..)
  , defaultMarginalSpec
  , LegendSpec(..)
  , LegendPosition(..)
  , defaultLegendSpec
  , legend
  , legendOff
  , legendPos
  , guideColorNone
  , legendReverse
  , legendNcol
  , legendNrow
    -- * DAG (= Phase 26 §E-6, HBM ModelGraph)
  , DAGSpec(..)
  , DAGNode(..)
  , DAGEdge(..)
  , RoutedEdge(..)
  , EdgeShapeKind(..)
  , DAGPlate(..)
  , DAGNodeKind(..)
  , DAGLayoutAlgorithm(..)
    -- * Top-level
  , VisualSpec(..)
  , ThemeName(..)
  , themeSeriesPalette
  , okabeIto, tolBright, brewerSet2, brewerDark2
  , ThemeOverride(..)
  , themeGrid, panelFill, panelBorder, themeAxisLine, gridColor, plotBg, axisColor, textColor
  , themeTitleFont, themeAxisLabelFont, themeTickFont, themeLegendFont, themeAxisTextAngle
  , themeAxisTextAngleX, themeAxisTextAngleY, axisTextAngleXOf, axisTextAngleYOf
  , stripFill, themeStrip
  , titleHjust, titleColor, tickColor, legendKeyBg   -- ★ Phase 43 A4
    -- * Top-level setters (= VisualSpec 返却)
  , purePlot
  , layer
  , title
  , theme
  , facet
  , facetWrap
  , facetCols
  , facetGrid
  , FacetScales(..)
  , freeScaleX
  , freeScaleY
  , facetScales
  , FacetSpace(..)
  , freeSpaceX
  , freeSpaceY
  , facetSpace
  , xLabel
  , yLabel
  , legendTitle
  , subtitle
  , caption
  , tag
  , Labs(..)
  , emptyLabs
  , labs
  , width
  , height
  , widthMm
  , heightMm
  , widthUnit
  , heightUnit
  , dpi
  , aspectRatio
  , xAxis
  , yAxis
  , refLine
  , refIdentity
  , refHorizontal
  , refVertical
  , marginal
  , marginalX
  , marginalY
    -- * C-6 Shape encoding
  , MarkShape(..)
  , ShapeMapEntry(..)
  , shape
  , shapeBy
  , shapeMapEntry
  , sizeBy
  , alphaBy              -- ★ Phase 30 A8 連続 alpha encoding (= ggplot scale_alpha)
  , colorCats
  , orderedCats
    -- * Phase 11 A4-b linetype encoding
  , LineType(..)
  , linetype
  , linetypeBy
  , lineTypeDash
  , lineTypeForIndex
    -- * Font customization (= hgg-frontend-settings-spec v0.1 §1.3)
  , FontSpec(..)
  , emptyFontSpec
  , fontSize
  , fontFamily
  , fontWeight
  , fontItalic
  , fontColor
  , titleFont
  , axisLabelFont
  , tickFont
  , legendFont
  ) where

import           Hgg.Plot.Color (Color (..), rgb, fromHex, fromHexMaybe,
                                     fromHexA, fromHexAMaybe, toCss)
import           Data.Aeson      (FromJSON, ToJSON, toJSON, parseJSON, toEncoding,
                                  Value (Object))
import qualified Data.Aeson      as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Char       as Char
import qualified Data.List
import           Data.Maybe      (catMaybes)
import           Data.Monoid     (First (..), Last (..))
import           Data.String     (IsString (..))
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           GHC.Generics    (Generic, Generically (..))

import           Hgg.Plot.Unit (Length, Pos (..), lengthToPt, mm, (*~))
import           Hgg.Plot.Primitive (Primitive, Rect (..))  -- Phase 51: custom mark closure 型
-- Phase 55: Spec の module 分割 (本 module は facade として全 API を re-export)
import           Hgg.Plot.Spec.Axis
import           Hgg.Plot.Spec.Bake
import           Hgg.Plot.Spec.Column
import           Hgg.Plot.Spec.Concat
import           Hgg.Plot.Spec.Constructors
import           Hgg.Plot.Spec.Decoration
import           Hgg.Plot.Spec.CustomMark
import           Hgg.Plot.Spec.Layer
import           Hgg.Plot.Spec.Mark
import           Hgg.Plot.Spec.Setters
import           Hgg.Plot.Spec.Theme
import           Hgg.Plot.Spec.Visual

