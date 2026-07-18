-- |
-- Module      : Graphics.Hgg.Backend.SVG
-- Description : SVG backend (Phase 26 §B-1 Resolver 対応版)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Backend.SVG
  ( -- * 通常 (Resolver 不要 = inline 列のみの図)
    renderSVG
  , saveSVG
    -- * Resolver 同伴 (= 'ColByName' を含む図)
  , renderSVGWith
  , saveSVGWith
  , renderSVGInteractive
  , saveSVGInteractive
  , plot
    -- * Phase 14: BoundPlot (df バインド済) を描画する
  , renderBound
  , saveSVGBound
    -- * Phase 3 A8: Primitive 列を直接 SVG にする helper (= hgg-3d 用)
  , renderPrimitivesSVG
  , savePrimitivesSVG
  ) where

import           Graphics.Hgg.Frame    (BoundPlot (..))
import           Graphics.Hgg.Layout   (Layout (..), Rect (..),
                                        ViewportSize (..), computeLayout)
import           Graphics.Hgg.Render   (FillStyle (..), LineStyle (..),
                                        PathSegment (..), Point (..),
                                        Primitive (..), StrokeStyle (..),
                                        TextAnchor (..), TextStyle (..),
                                        renderToPrimitives, scalePrimitives)
import           Graphics.Hgg.Spec     (MarkKind (..), Resolver, VisualSpec (..),
                                        emptyResolver, lyKind, vsDpi)
import           Data.Monoid           (getFirst, getLast)
import           Graphics.Hgg.Validate (PlotDiagnostic, Severity (..),
                                        diagnosticSeverity, renderDiagnostic)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.IO             (hPutStrLn, stderr)

-- | 'Resolver' を渡して 'VisualSpec' を SVG text に。 'ColByName' を含む図用。
renderSVGWith :: Resolver -> VisualSpec -> Text
renderSVGWith r spec =
  let layout     = computeLayout r spec
      -- ★ Phase 33 B5: layout/primitive は純 pt。SVG (raster 想定) は dpi/72 を
      --   一度だけ掛けて device px へ (唯一の dpi 適用点)。既定 96dpi → k=4/3。
      k          = maybe 96 id (getLast (vsDpi spec)) / 72
      primitives = scalePrimitives k (renderToPrimitives r layout spec)
      ViewportSize wpt hpt = lpViewport layout
      w = round (fromIntegral wpt * k) :: Int
      h = round (fromIntegral hpt * k) :: Int
      header = T.concat
        [ "<svg xmlns=\"http://www.w3.org/2000/svg\" "
        , "width=\"",  num w, "\" "
        , "height=\"", num h, "\" "
        , "viewBox=\"0 0 ", num w, " ", num h, "\">"
        ]
  in T.concat (header : primsToSvg primitives : ["</svg>"])

-- | render 'VisualSpec' to SVG text。 Resolver 不要 (= 全 ColRef が inline、
-- 'ColByName' が無い図で使う、 = 通常)。 列名参照を含む図は 'renderSVGWith'。
renderSVG :: VisualSpec -> Text
renderSVG = renderSVGWith emptyResolver

-- | 'Resolver' を渡して SVG ファイルに保存。 'ColByName' を含む図用。
saveSVGWith :: FilePath -> Resolver -> VisualSpec -> IO ()
saveSVGWith path r spec = TIO.writeFile path (renderSVGWith r spec)

-- | SVG ファイルに保存。 Resolver 不要 (= inline 列のみの図、 = 通常)。
-- 列名参照を含む図は 'saveSVGWith'、 DataFrame は 'saveSVGBound' (@df |>> spec@)。
saveSVG :: FilePath -> VisualSpec -> IO ()
saveSVG path = saveSVGWith path emptyResolver

-- | 'purePlot' (= 純粋値) と対をなす副作用関数。 中身は 'saveSVG' の alias、
-- SVG backend が default。 他 backend (PDF / PNG) を使う場合はそれぞれの
-- module の `plot` を import する。
--
-- > main = plot "out.svg" $ purePlot <> layer (scatter ...) <> title "..."
plot :: FilePath -> VisualSpec -> IO ()
plot = saveSVG

-- | Phase 14: 'BoundPlot' (= @df |>> spec@ の結果) を SVG text に。
-- 'renderSVGWith' を 'bpResolver' / 'bpSpec' で呼ぶ薄いラッパ。
-- 検証診断 ('bpDiagnostics') は副作用が無いここでは無視する
-- (報告は 'saveSVGBound' / 利用者が 'bpDiagnostics' を直接見る)。
renderBound :: BoundPlot -> Text
renderBound (BoundPlot r spec _) = renderSVGWith r spec

-- | Phase 14: 'BoundPlot' を SVG ファイルに保存。
-- 'bpDiagnostics' に Error severity があれば **stderr に報告**してから書き出す
-- (描画自体は止めない = 純値 '(|>>)' の lenient 既定。 無検証で通したい場合は
-- 'unBound' → 'saveSVGWith' を直接使う)。
saveSVGBound :: FilePath -> BoundPlot -> IO ()
saveSVGBound path bp@(BoundPlot _ spec diags) = do
  reportErrors diags
  warnUnresolvedStats spec
  TIO.writeFile path (renderBound bp)

-- | Phase 16 footgun 緩和: spec に未解決 stat layer (@MStatLM@/@MStatSmooth@) が残っていたら
-- stderr に警告 (描画では skip され回帰線が出ない)。 回帰を描くには analyze-bridge の
-- @saveSVGBoundStats@ / @resolveStats@ を使う。 stat を使わない通常図では何もしない。
warnUnresolvedStats :: VisualSpec -> IO ()
warnUnresolvedStats spec
  | any isStat (vsLayers spec) =
      hPutStrLn stderr
        "[hgg] 警告: 未解決の stat layer (statLm/statSmooth) があります。 \
        \回帰線は描画されません。 analyze-bridge の saveSVGBoundStats で描画してください。"
  | otherwise = pure ()
  where
    isStat ly = case getFirst (lyKind ly) of
      Just MStatLM     -> True
      Just MStatSmooth -> True
      _                -> False

-- | Error severity の診断のみ stderr に出す。
reportErrors :: [PlotDiagnostic] -> IO ()
reportErrors diags =
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        (filter ((== SevError) . diagnosticSeverity) diags)

-- | Interactive 版: hover tooltip (= 標準 native) に加えて、
-- ドラッグで pan / wheel で zoom できる inline JS を末尾に embed。
-- ブラウザで開いた時だけ動作、 raw SVG viewer では普通に静止画。
renderSVGInteractive :: Resolver -> VisualSpec -> Text
renderSVGInteractive r spec =
  let base = renderSVGWith r spec
      -- </svg> 直前に <script> を挿入
      (pre, post) = T.breakOn "</svg>" base
  in T.concat [pre, panZoomScript, post]

saveSVGInteractive :: FilePath -> Resolver -> VisualSpec -> IO ()
saveSVGInteractive path r spec = TIO.writeFile path (renderSVGInteractive r spec)

-- | Phase 3 A8: '[Primitive]' を直接 SVG にする helper。
-- 'renderSVG' は VisualSpec 経由だが、 hgg-3d のように外部で
-- Primitive 列を生成済の場合に使う。 既存の 'primToSvg' converter をそのまま流用、
-- 出力 SVG 構造 (= header + light bg + title + body) は 'renderSVG' と同形式。
renderPrimitivesSVG :: Int -> Int -> Text -> [Primitive] -> Text
renderPrimitivesSVG w h titleTxt prims =
  let header = T.concat
        [ "<svg xmlns=\"http://www.w3.org/2000/svg\" "
        , "width=\"",  num w, "\" "
        , "height=\"", num h, "\" "
        , "viewBox=\"0 0 ", num w, " ", num h, "\">"
        ]
      -- bg + title
      bg = T.concat
        [ "<rect x=\"0\" y=\"0\" width=\"", num w
        , "\" height=\"", num h
        , "\" fill=\"#fafafa\"/>" ]
      -- ★ Phase 43 既定に合わせタイトルは左寄せ (ggplot theme_grey)。 3D backend は
      --   plotArea を持たないので左 margin は固定 20px。 2D の plot.title 左寄せと統一。
      title_ = if T.null titleTxt then "" else T.concat
        [ "<text x=\"20\" y=\"30\""
        , " text-anchor=\"start\" font-family=\"sans-serif\""
        , " font-size=\"16\" fill=\"#333\">", titleTxt, "</text>" ]
  in T.concat (header : bg : title_ : primsToSvg prims : ["</svg>"])

-- | 'renderPrimitivesSVG' をファイル書出し版。
savePrimitivesSVG :: FilePath -> Int -> Int -> Text -> [Primitive] -> IO ()
savePrimitivesSVG path w h t prims =
  TIO.writeFile path (renderPrimitivesSVG w h t prims)

-- | pan / zoom inline JS。 SVG の viewBox を操作するだけの最小実装。
panZoomScript :: Text
panZoomScript = T.concat
  [ "<script type=\"application/ecmascript\"><![CDATA[\n"
  , "(function(){\n"
  , "  var svg = document.currentScript.parentNode;\n"
  , "  var vb = svg.viewBox.baseVal;\n"
  , "  var dragging = false, sx = 0, sy = 0, vx0 = 0, vy0 = 0;\n"
  , "  svg.addEventListener('mousedown', function(e){\n"
  , "    dragging = true; sx = e.clientX; sy = e.clientY;\n"
  , "    vx0 = vb.x; vy0 = vb.y;\n"
  , "    svg.style.cursor = 'grabbing';\n"
  , "  });\n"
  , "  window.addEventListener('mouseup', function(){\n"
  , "    dragging = false; svg.style.cursor = 'default';\n"
  , "  });\n"
  , "  svg.addEventListener('mousemove', function(e){\n"
  , "    if (!dragging) return;\n"
  , "    var dx = (e.clientX - sx) * vb.width  / svg.clientWidth;\n"
  , "    var dy = (e.clientY - sy) * vb.height / svg.clientHeight;\n"
  , "    vb.x = vx0 - dx; vb.y = vy0 - dy;\n"
  , "  });\n"
  , "  svg.addEventListener('wheel', function(e){\n"
  , "    e.preventDefault();\n"
  , "    var scale = e.deltaY > 0 ? 1.1 : 1/1.1;\n"
  , "    var rect = svg.getBoundingClientRect();\n"
  , "    var mx = vb.x + (e.clientX - rect.left) * vb.width  / rect.width;\n"
  , "    var my = vb.y + (e.clientY - rect.top)  * vb.height / rect.height;\n"
  , "    vb.x = mx - (mx - vb.x) * scale;\n"
  , "    vb.y = my - (my - vb.y) * scale;\n"
  , "    vb.width  *= scale; vb.height *= scale;\n"
  , "  });\n"
  , "})();\n"
  , "]]></script>"
  ]

-- ---------------------------------------------------------------------------
-- Primitive → SVG element
-- ---------------------------------------------------------------------------

-- | Phase 11 A7-a: clip stack を解決して SVG body に変換。 'PClipPush' で
--   @\<clipPath\>@ + @\<g clip-path\>@ を開き、 'PClipPop' で @\</g\>@ を閉じる。
--   clip プリミティブが無い列では @map primToSvg@ と完全同一出力なので既存 SVG ゼロ diff。
primsToSvg :: [Primitive] -> Text
primsToSvg = T.concat . go (0 :: Int)
  where
    go _ [] = []
    go n (PClipPush (Rect x y w h) : rest) =
      let cid  = T.concat ["clip", num n]
          open = T.concat
            [ "<clipPath id=\"", cid, "\"><rect x=\"", numD x, "\" y=\"", numD y
            , "\" width=\"", numD w, "\" height=\"", numD h, "\"/></clipPath>"
            , "<g clip-path=\"url(#", cid, ")\">" ]
      in open : go (n + 1) rest
    go n (PClipPop : rest) = "</g>" : go n rest
    go n (p : rest)        = primToSvg p : go n rest

primToSvg :: Primitive -> Text
primToSvg p = case p of
  PLine (Point x1 y1) (Point x2 y2) (LineStyle c w d) ->
    T.concat
      [ "<line x1=\"", numD x1, "\" y1=\"", numD y1
      , "\" x2=\"", numD x2, "\" y2=\"", numD y2
      , "\" stroke=\"", c, "\" stroke-width=\"", numD w, "\""
      , dashAttr d, "/>"
      ]
  PRect (Rect x y w h) (FillStyle fc fo) mStroke ->
    T.concat
      [ "<rect x=\"", numD x, "\" y=\"", numD y
      , "\" width=\"", numD w, "\" height=\"", numD h
      , "\" fill=\"", fc, "\" fill-opacity=\"", numD fo, "\""
      , strokeAttr mStroke
      , "/>"
      ]
  PCircle (Point cx cy) r (FillStyle fc fo) mStroke mTitle ->
    case mTitle of
      Nothing ->
        T.concat
          [ "<circle cx=\"", numD cx, "\" cy=\"", numD cy
          , "\" r=\"", numD r
          , "\" fill=\"", fc, "\" fill-opacity=\"", numD fo, "\""
          , strokeAttr mStroke
          , "/>"
          ]
      Just t ->
        -- <circle><title>label</title></circle> でブラウザ native hover tooltip
        T.concat
          [ "<circle cx=\"", numD cx, "\" cy=\"", numD cy
          , "\" r=\"", numD r
          , "\" fill=\"", fc, "\" fill-opacity=\"", numD fo, "\""
          , strokeAttr mStroke
          , "><title>", escapeXml t, "</title></circle>"
          ]
  PText (Point x y) s (TextStyle c sz fam anchor rot weight italic) ->
    let anchorAttr = case anchor of
          AnchorStart  -> "start"
          AnchorMiddle -> "middle"
          AnchorEnd    -> "end"
        -- Phase 50 A1: 内部 'tsRotate' は **CCW 正** (canonical)。 SVG rotate() は y-down で
        --   CW 正なので、 ここで **符号反転** して device の CW へ変換する (唯一の変換点)。
        rotAttr = if rot == 0
          then ""
          else T.concat [" transform=\"rotate(", numD (negate rot)
                        , " ", numD x, " ", numD y, ")\""]
        -- TODO-10 (2026-05-29): font-weight / font-style emit (default 値は省略)
        weightAttr = if weight == "normal" || T.null weight
          then ""
          else T.concat [" font-weight=\"", weight, "\""]
        italicAttr = if italic then " font-style=\"italic\"" else ""
    in T.concat
         [ "<text x=\"", numD x, "\" y=\"", numD y
         , "\" fill=\"", c, "\" font-size=\"", numD sz
         , "\" font-family=\"", fam
         , "\" text-anchor=\"", anchorAttr, "\""
         , weightAttr, italicAttr
         , rotAttr
         , ">"
         , escapeXml s
         , "</text>"
         ]
  PPath segs (FillStyle fc fo) mStroke ->
    T.concat
      [ "<path d=\"", pathSegs segs
      , "\" fill=\"", fc, "\" fill-opacity=\"", numD fo, "\""
      , strokeAttr mStroke
      , "/>"
      ]
  PClipPush{}      -> ""
  PClipPop         -> ""
  PTransformPush{} -> ""
  PTransformPop    -> ""

pathSegs :: [PathSegment] -> Text
pathSegs = T.intercalate " " . map seg
  where
    seg (MoveTo (Point x y))  = T.concat ["M ", numD x, " ", numD y]
    seg (LineTo (Point x y))  = T.concat ["L ", numD x, " ", numD y]
    seg (CurveTo (Point cx1 cy1) (Point cx2 cy2) (Point x y)) = T.concat
      ["C ", numD cx1, " ", numD cy1, " ", numD cx2, " ", numD cy2, " ", numD x, " ", numD y]
    seg ClosePath = "Z"

strokeAttr :: Maybe StrokeStyle -> Text
strokeAttr Nothing                  = " stroke=\"none\""
strokeAttr (Just (StrokeStyle c w)) =
  T.concat [" stroke=\"", c, "\" stroke-width=\"", numD w, "\""]

-- | Phase 11 A4-b: stroke-dasharray 属性。 空配列 (= 実線) は attr を出さない
--   (= 既存 SVG ゼロ diff の要)。 非空のみ \" stroke-dasharray=\\\"a,b,..\\\"\" を付す。
dashAttr :: [Double] -> Text
dashAttr [] = ""
dashAttr ds = T.concat [" stroke-dasharray=\"", T.intercalate "," (map numD ds), "\""]

num :: Int -> Text
num = T.pack . show

numD :: Double -> Text
numD = T.pack . show

escapeXml :: Text -> Text
escapeXml = T.concatMap esc
  where
    esc '<'  = "&lt;"
    esc '>'  = "&gt;"
    esc '&'  = "&amp;"
    esc '"'  = "&quot;"
    esc '\'' = "&apos;"
    esc c    = T.singleton c
