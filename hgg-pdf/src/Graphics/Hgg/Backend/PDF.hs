-- |
-- Module      : Graphics.Hgg.Backend.PDF
-- Description : ベクタ PDF backend (Phase 17、 HPDF)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- plot-core の '[Primitive]' を HPDF (Graphics.PDF) の 'P.Draw' 命令に
-- 解釈する (SVG backend と同じ Layer 1 構図)。 座標系は per-primitive で
-- y 反転 (PDF = 左下原点 y 上向き / Primitive = SVG 系 左上原点 y 下向き。
-- global flip だと text が鏡像になるため per-primitive 変換)。
--
-- ★v1 制約: フォントは PDF 標準 14 種 (Latin) のみ — 日本語ラベルは
-- 出せない (非 Latin-1 文字は警告 + @?@ 置換)。 日本語の受け皿は
-- hgg-rasterific (PNG + TrueType)。
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Backend.PDF
  ( -- * 通常 (Resolver 不要 = inline 列のみの図)
    savePDF
    -- * Resolver 同伴 (= 'ColByName' を含む図)
  , savePDFWith
    -- * Phase 14 系: BoundPlot (df バインド済) を描画する
  , savePDFBound
    -- * 低レベル: [Primitive] を直接描画 (Phase 24 A8・3D backend glue 用)
  , savePrimitivesPDF
  ) where

import           Graphics.Hgg.Frame    (BoundPlot (..))
import           Graphics.Hgg.Layout   (Layout (..), Rect (..),
                                        ViewportSize (..), computeLayout)
import           Graphics.Hgg.Render   (FillStyle (..), LineStyle (..),
                                        PathSegment (..), Point (..),
                                        Primitive (..), StrokeStyle (..),
                                        TextAnchor (..), TextStyle (..),
                                        Transform (..), renderToPrimitives)
import           Graphics.Hgg.Spec     (Resolver, VisualSpec, emptyResolver)
import           Graphics.Hgg.Validate (Severity (..), diagnosticSeverity,
                                        renderDiagnostic)
import           Data.Char             (digitToInt, isHexDigit)
import           Data.Complex          (Complex ((:+)))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Graphics.PDF          as P
import           System.IO             (hPutStrLn, stderr)

-- ===========================================================================
-- 入口 (SVG backend の saveSVG / saveSVGWith / saveSVGBound と対称)
-- ===========================================================================

-- | PDF ファイルに保存。 Resolver 不要 (= inline 列のみの図、 = 通常)。
-- 列名参照を含む図は 'savePDFWith'、 DataFrame は 'savePDFBound' (@df |>> spec@)。
savePDF :: FilePath -> VisualSpec -> IO ()
savePDF path = savePDFWith path emptyResolver

-- | 'Resolver' を渡して PDF ファイルに保存。 'ColByName' を含む図用。
savePDFWith :: FilePath -> Resolver -> VisualSpec -> IO ()
savePDFWith path r spec = do
  -- ★ Phase 33 B5: PDF は point ネイティブ (PDFRect 単位 = pt) ゆえ k=1。layout/prims
  --   は純 pt なので scalePrimitives 不要 (恒等)・viewport pt をそのまま頁サイズに。
  --   raster backend のような dpi 乗算をするとサイズが二重変換になるので禁止。
  let layout = computeLayout r spec
      prims  = renderToPrimitives r layout spec
      ViewportSize w h = lpViewport layout
  savePrimitivesPDF path w h prims

-- | Phase 24 A8: [Primitive] 列を所与のキャンバスサイズで PDF に直接描画する
-- 低レベル経路 ('savePrimitivesSVG' の PDF 版)。 2D の 'savePDFWith' と 3D の
-- 'savePDF3D' が共有。 非 Latin-1 ラベルは警告 + @?@ 置換 (v1 制約)。
savePrimitivesPDF :: FilePath -> Int -> Int -> [Primitive] -> IO ()
savePrimitivesPDF path w h prims = do
  let (wD, hD) = (fromIntegral w, fromIntegral h)
  warnNonLatin prims
  fonts <- loadStdFonts
  P.runPdf path P.standardDocInfo (P.PDFRect 0 0 wD hD) $ do
    page <- P.addPage Nothing
    P.drawWithPage page (drawPrims fonts hD prims)

-- | v1 制約の loud 化: 非 Latin-1 文字を含むラベルがあれば stderr に 1 回警告
-- (描画では @?@ に置換される)。 日本語ラベルは PNG backend
-- (hgg-rasterific) を使う。
warnNonLatin :: [Primitive] -> IO ()
warnNonLatin prims
  | any hasNonLatin prims =
      hPutStrLn stderr
        "[hgg-pdf] 警告: 非 Latin-1 文字を含むラベルがあります。 \
        \PDF 標準フォントでは描けないため ? に置換されます (v1 制約)。 \
        \日本語ラベルは PNG backend (hgg-rasterific) を使ってください。"
  | otherwise = pure ()
  where
    hasNonLatin (PText _ s _) = T.any (> '\xFF') s
    hasNonLatin _             = False

-- | 'BoundPlot' (= @df |>> spec@ の結果) を PDF ファイルに保存。
-- Error severity の検証診断は stderr に報告してから書き出す
-- (saveSVGBound と同じ lenient 既定)。
savePDFBound :: FilePath -> BoundPlot -> IO ()
savePDFBound path (BoundPlot r spec diags) = do
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        (filter ((== SevError) . diagnosticSeverity) diags)
  savePDFWith path r spec

-- ===========================================================================
-- Primitive 解釈器
-- ===========================================================================

-- | Primitive 列を順に描く。 第 1 引数 = viewport 高さ (y 反転用)。
--
-- PClipPush/PTransformPush は対応する Pop までを**再帰グルーピング**して
-- 'P.withNewContext' に入れる (PDF の clip / CTM は graphics state 復元で
-- しか戻せないため。 SVG backend は \<g\> 開閉で済むのと対照的)。
-- 対応の取れない Pop は黙って無視 (SVG backend と同じ寛容さ)。
drawPrims :: StdFonts -> Double -> [Primitive] -> P.Draw ()
drawPrims fonts h = go
  where
    go [] = pure ()
    go (PClipPush rect : rest) =
      let (inner, after) = breakMatch isClipPush isClipPop rest
      in do P.withNewContext $ do
              clipRectOf h rect
              go inner
            go after
    go (PTransformPush tr : rest) =
      let (inner, after) = breakMatch isTrPush isTrPop rest
      in do P.withNewContext $ do
              P.applyMatrix (matrixOf h tr)
              go inner
            go after
    go (PClipPop : rest)      = go rest
    go (PTransformPop : rest) = go rest
    go (p : rest)             = drawOne fonts h p >> go rest

    isClipPush p = case p of { PClipPush _ -> True; _ -> False }
    isClipPop  p = case p of { PClipPop    -> True; _ -> False }
    isTrPush   p = case p of { PTransformPush _ -> True; _ -> False }
    isTrPop    p = case p of { PTransformPop    -> True; _ -> False }

-- | 同種 push の入れ子を数えながら、 対応する pop までの内側と残りに割る。
-- 対応 pop が無ければ全部内側 (= 末尾まで clip が効く・SVG の開きっ放しと同義)。
breakMatch :: (Primitive -> Bool) -> (Primitive -> Bool)
           -> [Primitive] -> ([Primitive], [Primitive])
breakMatch isPush isPop = walk (0 :: Int)
  where
    walk _ [] = ([], [])
    walk n (p : rest)
      | isPop p && n == 0 = ([], rest)
      | otherwise =
          let n' = if isPush p then n + 1 else if isPop p then n - 1 else n
              (inner, after) = walk n' rest
          in (p : inner, after)

-- | 単独 primitive の描画。 push/pop は 'drawPrims' が先に消費する。
drawOne :: StdFonts -> Double -> Primitive -> P.Draw ()
drawOne _ h (PLine a b ls)               = drawLine h a b ls
drawOne _ h (PRect rect fs ms)           = drawRect h rect fs ms
drawOne _ h (PCircle c rad fs ms _hover) = drawCircle h c rad fs ms
drawOne _ h (PPath segs fs ms)           = drawPath h segs fs ms
drawOne fonts h (PText p s ts)           = drawTextPrim fonts h p s ts
drawOne _ _ _                            = pure ()

-- | 矩形 clip: 矩形 path を current path にして 'P.setAsClipPath'。
clipRectOf :: Double -> Rect -> P.Draw ()
clipRectOf h (Rect x y w rh) = do
  P.addShape (P.Rectangle (x :+ (h - y - rh)) ((x + w) :+ (h - y)))
  P.setAsClipPath

-- | SVG 系 Transform → PDF Matrix。 y 反転 F (y↦h−y) は per-primitive に
-- 掛かるため、 SVG 空間の変換 M は **F∘M∘F (共役)** で PDF 空間に写す:
-- translate (dx,dy) → translate (dx,−dy) / scale (sx,sy) →
-- translate (0, h(1−sy)) ∘ scale (sx,sy)。 ※現状 core は PTransformPush を
-- 発行しない (SVG backend も未対応) — 将来の発行に備えた整合実装。
matrixOf :: Double -> Transform -> P.Matrix
matrixOf _ (TranslateT dx dy) = P.translate (dx :+ negate dy)
matrixOf h (ScaleT sx sy)     =
  P.translate (0 :+ (h * (1 - sy))) * P.scale sx sy

-- | パス: MoveTo/LineTo/CurveTo/ClosePath を HPDF path 命令に写して
-- fill / stroke / 両方 を塗り分ける。
drawPath :: Double -> [PathSegment] -> FillStyle -> Maybe StrokeStyle
         -> P.Draw ()
drawPath _ [] _ _ = pure ()
drawPath h segs fs@(FillStyle fc _) ms = P.withNewContext $ do
  applyFill fs
  applyStroke ms
  case segs of
    (MoveTo p : rest) -> P.beginPath (pdfPt h p) >> mapM_ seg rest
    _                 -> P.beginPath (0 :+ 0) >> mapM_ seg segs
  paint
  where
    seg (MoveTo p)         = P.moveto (pdfPt h p)
    seg (LineTo p)         = P.addLineToPath (pdfPt h p)
    seg (CurveTo c1 c2 p)  = P.addBezierCubic (pdfPt h c1) (pdfPt h c2) (pdfPt h p)
    seg ClosePath          = P.closePath
    paint = case (fc == "none", ms) of
      (True,  Just _)  -> P.strokePath
      (True,  Nothing) -> pure ()
      (False, Just _)  -> P.fillAndStrokePath
      (False, Nothing) -> P.fillPath

applyFill :: FillStyle -> P.Draw ()
applyFill (FillStyle col opa)
  | col == "none" = pure ()
  | otherwise     = P.fillColor (colorOf col) >> P.setFillAlpha opa

applyStroke :: Maybe StrokeStyle -> P.Draw ()
applyStroke Nothing                    = pure ()
applyStroke (Just (StrokeStyle sc sw)) =
  P.strokeColor (colorOf sc) >> P.setWidth sw

-- | SVG 系座標 (左上原点 y 下) → PDF 座標 (左下原点 y 上)。
pdfPt :: Double -> Point -> P.Point
pdfPt h (Point x y) = x :+ (h - y)

-- | 線分: 色/幅/破線を設定して stroke。
drawLine :: Double -> Point -> Point -> LineStyle -> P.Draw ()
drawLine h a b (LineStyle col w dash) = P.withNewContext $ do
  P.strokeColor (colorOf col)
  P.setWidth w
  setDashOf dash
  P.beginPath (pdfPt h a)
  P.addLineToPath (pdfPt h b)
  P.strokePath

-- | 矩形: fill (+ 任意 stroke)。 'Rect' は左上基準なので下端 = y + h 側。
drawRect :: Double -> Rect -> FillStyle -> Maybe StrokeStyle -> P.Draw ()
drawRect h (Rect x y w rh) fs ms = P.withNewContext $ do
  let shape = P.Rectangle (x :+ (h - y - rh)) ((x + w) :+ (h - y))
  fillShape shape fs ms

-- | 円: fill (+ 任意 stroke)。 hover label は PDF では捨てる。
drawCircle :: Double -> Point -> Double -> FillStyle -> Maybe StrokeStyle
           -> P.Draw ()
drawCircle h (Point cx cy) rad fs ms = P.withNewContext $ do
  let shape = P.Circle cx (h - cy) rad
  fillShape shape fs ms

-- | Shape 共通: FillStyle (色 + opacity) で塗り、 StrokeStyle があれば縁取る。
fillShape :: P.Shape a => a -> FillStyle -> Maybe StrokeStyle -> P.Draw ()
fillShape shape (FillStyle col opa) ms = do
  if col == "none"
    then pure ()
    else do
      P.fillColor (colorOf col)
      P.setFillAlpha opa
      P.fill shape
  case ms of
    Nothing -> pure ()
    Just (StrokeStyle sc sw) -> do
      P.strokeColor (colorOf sc)
      P.setWidth sw
      P.stroke shape

-- | LineStyle の dash 配列 (px) → HPDF DashPattern。 空 = 実線。
setDashOf :: [Double] -> P.Draw ()
setDashOf []   = P.setNoDash
setDashOf dash = P.setDash (P.DashPattern dash 0)

-- ===========================================================================
-- テキスト (Phase 17 A3) — PDF 標準フォント (Latin のみ・v1 制約)
-- ===========================================================================

-- | 標準フォント束 (3 family × 4 変種)。 'mkStdFont' は AFM parse を伴う IO
-- なので savePDF 入口で 1 回 load して 'P.Draw' 解釈器に渡す。
data StdFonts = StdFonts
  { sfSans  :: (P.AnyFont, P.AnyFont, P.AnyFont, P.AnyFont)
    -- ^ Helvetica (regular, bold, oblique, bold-oblique)
  , sfSerif :: (P.AnyFont, P.AnyFont, P.AnyFont, P.AnyFont)
    -- ^ Times
  , sfMono  :: (P.AnyFont, P.AnyFont, P.AnyFont, P.AnyFont)
    -- ^ Courier
  }

loadStdFonts :: IO StdFonts
loadStdFonts = do
  let load fn = do
        ef <- P.mkStdFont fn
        case ef of
          Right f  -> pure f
          Left err -> errorWithoutStackTrace
            ("hgg-pdf: 標準フォントの load に失敗: " ++ show err)
      load4 (a, b, c, d) = (,,,) <$> load a <*> load b <*> load c <*> load d
  StdFonts
    <$> load4 (P.Helvetica, P.Helvetica_Bold,
               P.Helvetica_Oblique, P.Helvetica_BoldOblique)
    <*> load4 (P.Times_Roman, P.Times_Bold,
               P.Times_Italic, P.Times_BoldItalic)
    <*> load4 (P.Courier, P.Courier_Bold,
               P.Courier_Oblique, P.Courier_BoldOblique)

-- | family ("serif" / "monospace" 系は対応 family、 他は Helvetica) +
-- weight/italic で 12 変種から選ぶ。
selectFont :: StdFonts -> Text -> Text -> Bool -> P.AnyFont
selectFont fonts fam weight italic =
  let (r, b, o, bo)
        | "serif" == fam || "Times" `T.isInfixOf` fam = sfSerif fonts
        | "mono" `T.isInfixOf` fam || "Courier" `T.isInfixOf` fam = sfMono fonts
        | otherwise = sfSans fonts
  in case (weight == "bold", italic) of
       (True,  True)  -> bo
       (True,  False) -> b
       (False, True)  -> o
       (False, False) -> r

-- | PText: anchor は 'P.textWidth' (AFM metrics) で x 補正、 tsRotate
-- (degrees CW・SVG 同義) は (x,y) 周りの回転 = PDF (y 上向き) では符号反転。
-- 非 Latin-1 文字は @?@ 置換 (警告は 'warnNonLatin' が IO 側で 1 回出す)。
drawTextPrim :: StdFonts -> Double -> Point -> Text -> TextStyle -> P.Draw ()
drawTextPrim fonts h (Point x y) txt ts = P.withNewContext $ do
  let anyf  = selectFont fonts (tsFamily ts) (tsWeight ts) (tsItalic ts)
      size  = max 1 (round (tsSize ts)) :: Int
      font  = P.PDFFont anyf size
      clean = T.map (\c -> if c > '\xFF' then '?' else c) txt
      tw    = P.textWidth font clean
      dx    = case tsAnchor ts of
                AnchorStart  -> 0
                AnchorMiddle -> negate (tw / 2)
                AnchorEnd    -> negate tw
  P.fillColor (colorOf (tsColor ts))
  if tsRotate ts == 0
    then P.drawText $ do
           P.setFont font
           P.textStart (x + dx) (h - y)
           P.displayText clean
    else do
      P.applyMatrix (P.translate (x :+ (h - y)))
      -- Phase 50 A1: 内部 tsRotate は CCW 正 (canonical)。 PDF/PostScript は y-up で CCW 正
      --   ゆえ **恒等** で渡す (旧: CW canonical を negate していた。 canonical CCW 化で解消)。
      P.applyMatrix (P.rotate (P.Degree (tsRotate ts)))
      P.drawText $ do
        P.setFont font
        P.textStart dx 0
        P.displayText clean

-- ===========================================================================
-- 色 (theme 色は全て "#rrggbb" hex。 named / 不正は黒 fallback)
-- ===========================================================================

colorOf :: Text -> P.Color
colorOf t = case T.unpack t of
  ['#', r1, r2, g1, g2, b1, b2]
    | all isHexDigit [r1, r2, g1, g2, b1, b2] ->
        P.Rgb (hex2 r1 r2) (hex2 g1 g2) (hex2 b1 b2)
  ['#', r, g, b]
    | all isHexDigit [r, g, b] -> P.Rgb (hex2 r r) (hex2 g g) (hex2 b b)
  "white" -> P.Rgb 1 1 1
  _       -> P.Rgb 0 0 0
  where
    hex2 a b = fromIntegral (digitToInt a * 16 + digitToInt b) / (255 :: Double)
