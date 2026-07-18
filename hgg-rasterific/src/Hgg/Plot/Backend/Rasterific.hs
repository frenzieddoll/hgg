-- |
-- Module      : Hgg.Plot.Backend.Rasterific
-- Description : raster PNG backend (Phase 22、 Rasterific + FontyFruity)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- plot-core の '[Primitive]' を Rasterific の 'R.Drawing' 命令に解釈して
-- JuicyPixels で PNG エンコードする (SVG/PDF backend と同じ Layer 1 構図)。
-- Rasterific は y 下向き (SVG 同系) なので **y 反転は不要** (PDF と対照的)。
--
-- フォントは FontyFruity の TrueType 読込 = **日本語ラベル対応**
-- (PDF v1 制約の受け皿)。 探索は fontconfig 非依存の固定候補リスト
-- (明示 'pngFontPath' → 既知ディレクトリ × 既知ファイル名)。
-- ★FontyFruity は .ttc (TrueType Collection) / CFF 系 OTF 非対応 —
-- 見つからない時は探索パスを列挙して loud エラー。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Backend.Rasterific
  ( -- * 通常 (Resolver 不要 = inline 列のみの図)
    savePNG
    -- * Resolver 同伴 (= 'ColByName' を含む図)
  , savePNGWith
    -- * Phase 14 系: BoundPlot (df バインド済) を描画する
  , savePNGBound
    -- * 設定付き (フォント明示 / Hi-DPI)
  , savePNGConfigured
  , PNGConfig (..)
  , defaultPNGConfig
    -- * 低レベル: [Primitive] を直接描画 (Phase 24 A8・3D backend glue 用)
  , savePrimitivesPNG
    -- * フォント解決 (診断 / テスト用)
  , PNGFonts (..)
  , loadPNGFonts
  ) where

import           Hgg.Plot.Frame        (BoundPlot (..))
import           Hgg.Plot.Layout       (Layout (..), Rect (..),
                                            ViewportSize (..), computeLayout)
import           Hgg.Plot.Render       (FillStyle (..), LineStyle (..),
                                            PathSegment (..), Point (..),
                                            Primitive (..), StrokeStyle (..),
                                            TextAnchor (..), TextStyle (..),
                                            Transform (..), renderToPrimitives,
                                            scalePrimitives)
import           Hgg.Plot.Spec         (Resolver, VisualSpec, emptyResolver,
                                            vsDpi)
import           Data.Monoid               (getLast)
import           Hgg.Plot.Validate     (Severity (..), diagnosticSeverity,
                                            renderDiagnostic)
import           Codec.Picture             (PixelRGBA8 (..), writePng)
import           Data.Char                 (digitToInt, isHexDigit, toLower)
import           Data.List                 (intercalate)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Graphics.Rasterific       as R
import qualified Graphics.Rasterific.Texture as RT
import qualified Graphics.Rasterific.Transformations as RTr
import qualified Graphics.Text.TrueType    as F
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            getHomeDirectory, listDirectory)
import           System.FilePath           (takeExtension, (</>))
import           System.IO                 (hPutStrLn, stderr)

-- ===========================================================================
-- 設定
-- ===========================================================================

-- | PNG 出力設定。 'defaultPNGConfig' から record update で部分指定する。
data PNGConfig = PNGConfig
  { pngFontPath     :: Maybe FilePath
    -- ^ regular TTF の明示指定 (最優先)。 Nothing = 候補リスト探索
  , pngFontPathBold :: Maybe FilePath
    -- ^ bold TTF。 Nothing = bold 候補探索 → 無ければ regular で代替
  , pngScale        :: Double
    -- ^ Hi-DPI 倍率 (既定 1.0 = SVG と同 pixel 寸法)。 2.0 で縦横 2 倍
  } deriving (Show, Eq)

defaultPNGConfig :: PNGConfig
defaultPNGConfig = PNGConfig
  { pngFontPath     = Nothing
  , pngFontPathBold = Nothing
  , pngScale        = 1.0
  }

-- ===========================================================================
-- 入口 (PDF backend の savePDF / savePDFWith / savePDFBound と対称)
-- ===========================================================================

-- | PNG ファイルに保存。 Resolver 不要 (= inline 列のみの図、 = 通常)。
-- 列名参照を含む図は 'savePNGWith'、 DataFrame は 'savePNGBound' (@df |>> spec@)。
savePNG :: FilePath -> VisualSpec -> IO ()
savePNG path = savePNGWith path emptyResolver

-- | 'Resolver' を渡して PNG ファイルに保存。 'ColByName' を含む図用。
savePNGWith :: FilePath -> Resolver -> VisualSpec -> IO ()
savePNGWith = savePNGConfigured defaultPNGConfig

-- | 'BoundPlot' (= @df |>> spec@ の結果) を PNG ファイルに保存。
-- Error severity の検証診断は stderr に報告してから書き出す
-- (savePDFBound と同じ lenient 既定)。
savePNGBound :: FilePath -> BoundPlot -> IO ()
savePNGBound path (BoundPlot r spec diags) = do
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        (filter ((== SevError) . diagnosticSeverity) diags)
  savePNGWith path r spec

-- | 'PNGConfig' 付き保存 (フォント明示 / Hi-DPI)。
savePNGConfigured :: PNGConfig -> FilePath -> Resolver -> VisualSpec -> IO ()
savePNGConfigured cfg path r spec = do
  -- ★ Phase 33 B5: layout/prims は純 pt。PNG は raster なので k=dpi/72 を一度だけ
  --   掛けて device px へ (HS SVG backend と同一・唯一の dpi 適用点)。font size も
  --   scalePrimitives で device px になり、drawTextPrim の px→point 変換はそのまま整合。
  --   pngScale (Hi-DPI) は k と直交の追加倍率として savePrimitivesPNG 側で温存。
  let layout = computeLayout r spec
      k      = maybe 96 id (getLast (vsDpi spec)) / 72
      prims  = scalePrimitives k (renderToPrimitives r layout spec)
      ViewportSize wpt hpt = lpViewport layout
      w = round (fromIntegral wpt * k) :: Int
      h = round (fromIntegral hpt * k) :: Int
  savePrimitivesPNG cfg path w h prims

-- | Phase 24 A8: [Primitive] 列を所与のキャンバスサイズで PNG に直接描画する
-- 低レベル経路 ('savePrimitivesSVG' の PNG 版)。 2D の 'savePNGConfigured' と
-- 3D の 'savePNG3D' が共有。 'pngScale' で Hi-DPI 拡大。
savePrimitivesPNG :: PNGConfig -> FilePath -> Int -> Int -> [Primitive] -> IO ()
savePrimitivesPNG cfg path w h prims = do
  let s  = max 1e-3 (pngScale cfg)
      wI = max 1 (ceiling (fromIntegral w * s)) :: Int
      hI = max 1 (ceiling (fromIntegral h * s)) :: Int
  fonts <- loadPNGFonts cfg
  -- 画像初期化は白。 theme 背景は [Primitive] 先頭の背景 rect が全面を塗る
  -- (Render/Layer.hs の background) ので実質 theme 色になる。
  let bg  = PixelRGBA8 255 255 255 255
      img = R.renderDrawing wI hI bg $
              R.withTransformation (RTr.scale (f s) (f s)) $
                drawPrims fonts prims
  writePng path img

-- ===========================================================================
-- フォント探索 (fontconfig 非依存・決定的)
-- ===========================================================================

-- | 解釈器に渡すフォント束。 v1 は family 非区別 (regular/bold の 2 face のみ。
-- 日本語 .ttf で serif/italic が揃う環境は稀のため。 計画 md の設計判断)。
data PNGFonts = PNGFonts
  { pfRegular :: F.Font
  , pfBold    :: F.Font
  }

-- | 候補ディレクトリ (存在するものだけ走査・/usr/share/fonts は再帰)。
fontSearchDirs :: IO [FilePath]
fontSearchDirs = do
  home <- getHomeDirectory
  pure
    [ home </> ".fonts"
    , home </> ".local/share/fonts"
    , "/usr/share/fonts"
    , "/usr/local/share/fonts"
    , "/mnt/c/Windows/Fonts"   -- WSL (※日本語は .ttc が多く対象外になりがち)
    ]

-- | 候補ファイル名 (優先順・小文字比較)。 日本語対応 .ttf を先頭に、
-- 最後に Latin のみの DejaVu (JP フォント不在環境の文字化け回避より
-- 「とりあえず描ける」 を優先。 JP が必要なら pngFontPath で明示)。
regularCandidates :: [String]
regularCandidates =
  [ "hackgen-regular.ttf"
  , "notosanscjkjp-regular.ttf"
  , "notosansjp-regular.ttf"
  , "ipagp.ttf"            -- IPA P ゴシック
  , "ipag.ttf"             -- IPA ゴシック
  , "takaopgothic.ttf"
  , "takaogothic.ttf"
  , "dejavusans.ttf"
  ]

boldCandidates :: [String]
boldCandidates =
  [ "hackgen-bold.ttf"
  , "notosanscjkjp-bold.ttf"
  , "notosansjp-bold.ttf"
  , "dejavusans-bold.ttf"
  ]

-- | フォント load。 明示 path → 候補探索 → loud エラー。
loadPNGFonts :: PNGConfig -> IO PNGFonts
loadPNGFonts cfg = do
  index <- ttfIndex
  reg <- resolveFont index "regular" (pngFontPath cfg) regularCandidates
  bold <- case pngFontPathBold cfg of
    Just p  -> loadOrDie p
    Nothing -> case lookupCandidates index boldCandidates of
      Just p  -> loadOrDie p
      Nothing -> pure reg          -- bold 不在は regular で代替 (v1 制約)
  pure (PNGFonts reg bold)
  where
    resolveFont index roleName mExplicit candidates = case mExplicit of
      Just p  -> do
        ok <- doesFileExist p
        if ok then loadOrDie p
              else die ("明示指定の " ++ roleName ++ " フォントがありません: " ++ p)
      Nothing -> case lookupCandidates index candidates of
        Just p  -> loadOrDie p
        Nothing -> do
          dirs <- fontSearchDirs
          die $ unlines
            [ "日本語対応 TTF フォントが見つかりません。"
            , "探索ディレクトリ: " ++ intercalate ", " dirs
            , "候補ファイル名: " ++ intercalate ", " candidates
            , ".ttc (TrueType Collection) は非対応です。 .ttf を"
            , "PNGConfig { pngFontPath = Just <path> } で明示指定してください。"
            ]
    loadOrDie p = do
      ef <- F.loadFontFile p
      case ef of
        Right font -> pure font
        Left err   -> die ("TTF の load に失敗 (" ++ p ++ "): " ++ err
                           ++ " (※.ttc/OTF は非対応・.ttf のみ)")
    die msg = errorWithoutStackTrace ("hgg-rasterific: " ++ msg)

-- | 探索ディレクトリ配下の .ttf を再帰列挙して (小文字ファイル名, path) の
-- 索引にする。 候補リスト順 (= 優先順) に索引を引く。
ttfIndex :: IO [(String, FilePath)]
ttfIndex = do
  dirs <- fontSearchDirs
  concat <$> mapM walk dirs
  where
    walk dir = do
      ok <- doesDirectoryExist dir
      if not ok then pure [] else do
        entries <- listDirectory dir
        fmap concat . mapM (entryOf dir) $ entries
    entryOf dir e = do
      let p = dir </> e
      isDir <- doesDirectoryExist p
      if isDir
        then walk p
        else pure [ (map toLower e, p)
                  | map toLower (takeExtension e) == ".ttf" ]

lookupCandidates :: [(String, FilePath)] -> [String] -> Maybe FilePath
lookupCandidates index = go
  where
    go []       = Nothing
    go (c : cs) = case lookup c index of
      Just p  -> Just p
      Nothing -> go cs

-- ===========================================================================
-- Primitive 解釈器
-- ===========================================================================

-- | Double → Float (Rasterific の座標は Float)。
f :: Double -> Float
f = realToFrac

v2 :: Point -> R.Point
v2 (Point x y) = R.V2 (f x) (f y)

-- | Primitive 列を順に描く。 PClipPush/PTransformPush は対応する Pop までを
-- **再帰グルーピング**して 'R.withClipping' / 'R.withTransformation' に入れる
-- (PDF backend の drawPrims と同型。 Rasterific の clip/transform も
-- scoped combinator なので同じ構図が自然に合う)。
-- 対応の取れない Pop は黙って無視 (SVG backend と同じ寛容さ)。
drawPrims :: PNGFonts -> [Primitive] -> R.Drawing PixelRGBA8 ()
drawPrims fonts = go
  where
    go [] = pure ()
    go (PClipPush rect : rest) =
      let (inner, after) = breakMatch isClipPush isClipPop rest
      in do R.withClipping (R.fill (rectShape rect)) (go inner)
            go after
    go (PTransformPush tr : rest) =
      let (inner, after) = breakMatch isTrPush isTrPop rest
      in do R.withTransformation (transformOf tr) (go inner)
            go after
    go (PClipPop : rest)      = go rest
    go (PTransformPop : rest) = go rest
    go (p : rest)             = drawOne fonts p >> go rest

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

-- | SVG 系 Transform → Rasterific Transformation (y 下向き同士なのでそのまま)。
transformOf :: Transform -> RTr.Transformation
transformOf (TranslateT dx dy) = RTr.translate (R.V2 (f dx) (f dy))
transformOf (ScaleT sx sy)     = RTr.scale (f sx) (f sy)

-- | 単独 primitive の描画。 push/pop は 'drawPrims' が先に消費する。
drawOne :: PNGFonts -> Primitive -> R.Drawing PixelRGBA8 ()
drawOne _ (PLine a b ls)               = drawLine a b ls
drawOne _ (PRect rect fs ms)           = drawRect rect fs ms
drawOne _ (PCircle c rad fs ms _hover) = drawCircle c rad fs ms
drawOne _ (PPath segs fs ms)           = drawPath segs fs ms
drawOne fonts (PText p s ts)           = drawTextPrim fonts p s ts
drawOne _ _                            = pure ()

-- | 線分: 色/幅/破線。 cap は SVG 既定 butt 相当 (CapStraight 0)。
drawLine :: Point -> Point -> LineStyle -> R.Drawing PixelRGBA8 ()
drawLine a b (LineStyle col w dash) =
  R.withTexture (RT.uniformTexture (colorOf col 1.0)) $
    strokeMaybeDashed dash (f w) (R.line (v2 a) (v2 b))

-- | dash 配列 (px) が空なら実線、 あれば dashedStroke。
strokeMaybeDashed :: [Double] -> Float
                  -> [R.Primitive] -> R.Drawing PixelRGBA8 ()
strokeMaybeDashed dash w geom = case dash of
  [] -> R.stroke w joinStyle capStyle geom
  ds -> R.dashedStroke (map f ds) w joinStyle capStyle geom
  where
    joinStyle = R.JoinMiter 0
    capStyle  = (R.CapStraight 0, R.CapStraight 0)

rectShape :: Rect -> [R.Primitive]
rectShape (Rect x y w h) = R.rectangle (R.V2 (f x) (f y)) (f w) (f h)

-- | 矩形: fill (+ 任意 stroke)。 'Rect' は左上基準 = Rasterific と同じ。
drawRect :: Rect -> FillStyle -> Maybe StrokeStyle -> R.Drawing PixelRGBA8 ()
drawRect rect fs ms = fillShape (rectShape rect) fs ms

-- | 円: fill (+ 任意 stroke)。 hover label は PNG では捨てる (PDF と同じ)。
drawCircle :: Point -> Double -> FillStyle -> Maybe StrokeStyle
           -> R.Drawing PixelRGBA8 ()
drawCircle c rad fs ms = fillShape (R.circle (v2 c) (f rad)) fs ms

-- | Shape 共通: FillStyle (色 + opacity → alpha 合成) で塗り、
-- StrokeStyle があれば縁取る。
fillShape :: [R.Primitive] -> FillStyle -> Maybe StrokeStyle
          -> R.Drawing PixelRGBA8 ()
fillShape shape (FillStyle col opa) ms = do
  if col == "none"
    then pure ()
    else R.withTexture (RT.uniformTexture (colorOf col opa)) (R.fill shape)
  case ms of
    Nothing -> pure ()
    Just (StrokeStyle sc sw) ->
      R.withTexture (RT.uniformTexture (colorOf sc 1.0)) $
        strokeMaybeDashed [] (f sw) shape

-- | パス: MoveTo 区切りで subpath ('R.Path') に分割して fill / stroke。
-- fill は全 subpath の primitive をまとめて 1 回 (= SVG の nonzero winding が
-- subpath 横断で効く)、 stroke は subpath ごと (= まとめると subpath 間に
-- 偽の接続 join が入る) に塗り分ける。
drawPath :: [PathSegment] -> FillStyle -> Maybe StrokeStyle
         -> R.Drawing PixelRGBA8 ()
drawPath [] _ _ = pure ()
drawPath segs (FillStyle fc opa) ms = do
  let subPrims = map R.pathToPrimitives (splitSubpaths segs)
  if fc == "none"
    then pure ()
    else R.withTexture (RT.uniformTexture (colorOf fc opa)) $
           R.fill (concat subPrims)
  case ms of
    Nothing -> pure ()
    Just (StrokeStyle sc sw) ->
      R.withTexture (RT.uniformTexture (colorOf sc 1.0)) $
        mapM_ (strokeMaybeDashed [] (f sw)) subPrims

-- | 'PathSegment' 列 → 'R.Path' 列。 MoveTo で新 subpath、 ClosePath は
-- 現 subpath の close フラグ (以降に命令が続く稀ケースは同じ原点で新 subpath)。
-- 先頭が MoveTo でない場合は原点 (0,0) 開始 (PDF backend と同じ寛容さ)。
splitSubpaths :: [PathSegment] -> [R.Path]
splitSubpaths = go (R.V2 0 0) []
  where
    go origin acc [] = closeOff origin acc False []
    go origin acc (MoveTo p : rest) =
      closeOff origin acc False (go (v2 p) [] rest)
    go origin acc (LineTo p : rest) =
      go origin (R.PathLineTo (v2 p) : acc) rest
    go origin acc (CurveTo c1 c2 p : rest) =
      go origin (R.PathCubicBezierCurveTo (v2 c1) (v2 c2) (v2 p) : acc) rest
    go origin acc (ClosePath : rest) =
      closeOff origin acc True (go origin [] rest)
    -- 空 subpath (連続 MoveTo 等) は捨てる
    closeOff _      []  _      rest = rest
    closeOff origin acc closed rest =
      R.Path origin closed (reverse acc) : rest

-- | テキスト: 'R.printTextAt' (基準点 = baseline 開始 = SVG の text y と同義)。
-- anchor は 'F.stringBoundingBox' の advance width (_xMax 位置に格納・
-- FontyFruity ソース実測) で x 補正。 tsRotate (degrees CW・SVG 同義) は
-- y 下向き同士なので符号そのまま (PDF と対照的)、 rotate→translate の合成で
-- (x,y) 周りに回す。 tsSize は px → 'F.pixelSizeInPointAtDpi' で 96 dpi の
-- point に変換 ('R.renderDrawing' = 96 dpi 固定)。
-- v1 は family 非区別: tsWeight == "bold" のみ分岐、 italic は regular で代替。
drawTextPrim :: PNGFonts -> Point -> Text -> TextStyle
             -> R.Drawing PixelRGBA8 ()
drawTextPrim fonts (Point x y) txt ts =
  R.withTexture (RT.uniformTexture (colorOf (tsColor ts) 1.0)) $
    if tsRotate ts == 0
      then R.printTextAt font sizePt (R.V2 (f x + dx) (f y)) str
      else R.withTransformation
             -- Phase 50 A1: 内部 tsRotate は CCW 正 (canonical)。 Rasterific は y-down/CW ゆえ
             --   ここで符号反転して device CW へ (唯一の変換点)。
             (RTr.translate (R.V2 (f x) (f y))
                <> RTr.rotate (f (negate (tsRotate ts)) * pi / 180))
             (R.printTextAt font sizePt (R.V2 dx 0) str)
  where
    font   = if tsWeight ts == "bold" then pfBold fonts else pfRegular fonts
    sizePt = F.pixelSizeInPointAtDpi (f (max 1 (tsSize ts))) 96
    str    = T.unpack txt
    advW   = F._xMax (F.stringBoundingBox font 96 sizePt str)
    dx     = case tsAnchor ts of
               AnchorStart  -> 0
               AnchorMiddle -> negate (advW / 2)
               AnchorEnd    -> negate advW

-- ===========================================================================
-- 色 (theme 色は全て "#rrggbb" hex。 named / 不正は黒 fallback)
-- ===========================================================================

-- | hex 色 + opacity → PixelRGBA8 (opacity は alpha 成分に畳む =
-- PDF v1 の opacity 問題は PNG では起きない)。
colorOf :: Text -> Double -> PixelRGBA8
colorOf t opa = case T.unpack t of
  ['#', r1, r2, g1, g2, b1, b2]
    | all isHexDigit [r1, r2, g1, g2, b1, b2] ->
        PixelRGBA8 (hex2 r1 r2) (hex2 g1 g2) (hex2 b1 b2) alpha
  ['#', r, g, b]
    | all isHexDigit [r, g, b] -> PixelRGBA8 (hex2 r r) (hex2 g g) (hex2 b b) alpha
  "white" -> PixelRGBA8 255 255 255 alpha
  _       -> PixelRGBA8 0 0 0 alpha
  where
    alpha    = fromIntegral (max 0 (min 255 (round (opa * 255) :: Int)))
    hex2 a b = fromIntegral (digitToInt a * 16 + digitToInt b)
