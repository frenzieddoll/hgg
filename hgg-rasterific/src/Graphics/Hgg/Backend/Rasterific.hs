-- |
-- Module      : Graphics.Hgg.Backend.Rasterific
-- Description : raster PNG backend (Rasterific + FontyFruity)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: plot-core の '[Primitive]' を Rasterific の 'R.Drawing' 命令に解釈して
-- JuicyPixels で PNG エンコードする (SVG/PDF backend と同じ Layer 1 構図)。
-- Rasterific は y 下向き (SVG 同系) なので __y 反転は不要__ (PDF と対照的)。
--
-- [English]: Interprets plot-core's '[Primitive]' as Rasterific 'R.Drawing'
-- instructions and encodes the result to PNG with JuicyPixels (the same
-- Layer 1 structure as the SVG/PDF backends). Since Rasterific has y pointing
-- down (the same convention as SVG), __no y flip is needed__ (unlike PDF).
--
-- フォントは FontyFruity の TrueType 読込 = __日本語ラベル対応__
-- (PDF v1 制約の受け皿)。 探索は fontconfig 非依存の固定候補リスト
-- (明示 'pngFontPath' → 既知ディレクトリ × 既知ファイル名)。
-- ★FontyFruity は .ttc (TrueType Collection) / CFF 系 OTF 非対応 —
-- 見つからない時は探索パスを列挙して loud エラー。
--
-- [English]: Fonts are loaded via FontyFruity's TrueType reader, which
-- gives __Japanese label support__ (the fallback for PDF's v1 constraint).
-- Font discovery uses a fixed, fontconfig-independent candidate list (an
-- explicit 'pngFontPath' takes priority, otherwise known directories ×
-- known file names are searched). Note: FontyFruity does not support .ttc
-- (TrueType Collection) or CFF-flavored OTF — when no font is found, it
-- lists the search paths in a loud error.
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Backend.Rasterific
  ( -- * 通常 (Resolver 不要 = inline 列のみの図)
    savePNG
    -- * Resolver 同伴 (= @ColByName@ を含む図)
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
  , loadPNGFontsFor
  , normFamily
  ) where

import           Graphics.Hgg.Frame        (BoundPlot (..))
import           Graphics.Hgg.Layout       (Layout (..), Rect (..),
                                            ViewportSize (..), computeLayout)
import           Graphics.Hgg.Render       (FillStyle (..), LineStyle (..),
                                            PathSegment (..), Point (..),
                                            Primitive (..), StrokeStyle (..),
                                            TextAnchor (..), TextStyle (..),
                                            Transform (..), renderToPrimitives,
                                            scalePrimitives, specThemePalette,
                                            tpShowBackground)
import           Graphics.Hgg.Spec         (Resolver, VisualSpec, emptyResolver,
                                            vsDpi)
import           Data.Monoid               (getLast)
import           Graphics.Hgg.Validate     (Severity (..), diagnosticSeverity,
                                            renderDiagnostic,
                                            reportFacetInlineWarnings)
import           Codec.Picture             (PixelRGBA8 (..), writePng)
import           Data.Char                 (digitToInt, isHexDigit, toLower)
import           Data.List                 (intercalate, nub)
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

-- | [日本語]: PNG 出力設定。 'defaultPNGConfig' から record update で部分指定する。
--   [English]: PNG output settings. Specify individual fields via a record
--   update on 'defaultPNGConfig'.
data PNGConfig = PNGConfig
  { pngFontPath     :: Maybe FilePath
    -- ^ [日本語]: regular TTF の明示指定 (最優先)。 Nothing = 候補リスト探索
    --   [English]: An explicit regular TTF path (highest priority).
    --   Nothing means searching the candidate list.
  , pngFontPathBold :: Maybe FilePath
    -- ^ [日本語]: bold TTF。 Nothing = bold 候補探索 → 無ければ regular で代替
    --   [English]: The bold TTF. Nothing means searching the bold
    --   candidates, falling back to regular if none is found.
  , pngScale        :: Double
    -- ^ [日本語]: Hi-DPI 倍率 (既定 1.0 = SVG と同 pixel 寸法)。 2.0 で縦横 2 倍
    --   [English]: The Hi-DPI scale factor (default 1.0, giving the same
    --   pixel dimensions as SVG). 2.0 doubles both width and height.
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

-- | [日本語]: PNG ファイルに保存。 Resolver 不要 (= inline 列のみの図、 = 通常)。
--   列名参照を含む図は 'savePNGWith'、 DataFrame は 'savePNGBound' (@df |>> spec@)。
--   [English]: Saves to a PNG file. No 'Resolver' needed (figures with
--   inline columns only — the usual case). For figures with column-name
--   references use 'savePNGWith'; for a DataFrame use 'savePNGBound'
--   (@df |>> spec@).
savePNG :: FilePath -> VisualSpec -> IO ()
savePNG path = savePNGWith path emptyResolver

-- | [日本語]: 'Resolver' を渡して PNG ファイルに保存。 @ColByName@ を含む図用。
--   [English]: Saves to a PNG file given a 'Resolver'. For figures that
--   include @ColByName@.
savePNGWith :: FilePath -> Resolver -> VisualSpec -> IO ()
savePNGWith = savePNGConfigured defaultPNGConfig

-- | [日本語]: 'BoundPlot' (= @df |>> spec@ の結果) を PNG ファイルに保存。
--   Error severity の検証診断は stderr に報告してから書き出す
--   (savePDFBound と同じ lenient 既定)。
--   [English]: Saves a 'BoundPlot' (the result of @df |>> spec@) to a PNG
--   file. Reports error-severity validation diagnostics to stderr before
--   writing the file (the same lenient default as savePDFBound).
savePNGBound :: FilePath -> BoundPlot -> IO ()
savePNGBound path (BoundPlot r spec diags) = do
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        (filter ((== SevError) . diagnosticSeverity) diags)
  savePNGWith path r spec

-- | [日本語]: 'PNGConfig' 付き保存 (フォント明示 / Hi-DPI)。
--   [English]: Saves with a 'PNGConfig' (explicit font / Hi-DPI).
savePNGConfigured :: PNGConfig -> FilePath -> Resolver -> VisualSpec -> IO ()
savePNGConfigured cfg path r spec = do
  reportFacetInlineWarnings r spec   -- ★ Phase 62 A4 (§3): 描画は継続
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
      -- ★ Phase 63 A18: 背景を塗らない theme (themePlotBg False) は init を透過に
      --   (背景 rect が無いので init 色がそのまま残るため)。 塗る theme は従来どおり
      --   白 init (= 既存図の bit 単位不変を保証。 透過 init だと全面 rect の縁 AA で
      --   端 pixel の alpha が変わり得る)。
      bgPx = if tpShowBackground (specThemePalette spec)
               then PixelRGBA8 255 255 255 255
               else PixelRGBA8 0 0 0 0
  savePrimitivesPNGBg bgPx cfg path w h prims

-- | [日本語]: [Primitive] 列を所与のキャンバスサイズで PNG に直接描画する
--   低レベル経路 (@savePrimitivesSVG@ の PNG 版)。 2D の 'savePNGConfigured' と
--   3D の @savePNG3D@ が共有。 'pngScale' で Hi-DPI 拡大。
--   [English]: A low-level path that draws a list of Primitives directly to
--   PNG at a given canvas size (the PNG counterpart of @savePrimitivesSVG@).
--   Shared by 2D's 'savePNGConfigured' and 3D's @savePNG3D@. Hi-DPI scaling
--   is applied via 'pngScale'.
savePrimitivesPNG :: PNGConfig -> FilePath -> Int -> Int -> [Primitive] -> IO ()
savePrimitivesPNG = savePrimitivesPNGBg (PixelRGBA8 255 255 255 255)

-- | [日本語]: init 色指定版 (内部)。 通常は init 白 + [Primitive] 先頭の背景 rect が全面を
--   塗る (Render/Layer.hs の background) ので実質 theme 色になる。
--   背景を塗らない theme では 'savePNGConfigured' が透過 init を渡す。
--   [English]: The internal variant that takes an explicit init color.
--   Normally the init is white, and the background rect at the head of
--   [Primitive] fills the whole canvas (Render/Layer.hs's background), so
--   the effective color is the theme's. For a theme that does not paint a
--   background, 'savePNGConfigured' passes a transparent init instead.
savePrimitivesPNGBg :: PixelRGBA8 -> PNGConfig -> FilePath -> Int -> Int
                    -> [Primitive] -> IO ()
savePrimitivesPNGBg bg cfg path w h prims = do
  let s  = max 1e-3 (pngScale cfg)
      wI = max 1 (ceiling (fromIntegral w * s)) :: Int
      hI = max 1 (ceiling (fromIntegral h * s)) :: Int
  -- ★ Phase 63 A20.5: 使用 family を先に収集して束を解決 (fontFamily の PNG 配線)
  fonts <- loadPNGFontsFor cfg [ tsFamily ts | PText _ _ ts <- prims ]
  let img = R.renderDrawing wI hI bg $
              R.withTransformation (RTr.scale (f s) (f s)) $
                drawPrims fonts prims
  writePng path img

-- ===========================================================================
-- フォント探索 (fontconfig 非依存・決定的)
-- ===========================================================================

-- | [日本語]: 解釈器に渡すフォント束。 既定 (sans-serif) は regular/bold の 2 face、
--   spec の @fontFamily@ 指定分は 'pfFamilies'
--   (正規化 family 名 → face 対) で解決する。 italic は引き続き regular 代替
--   (日本語 .ttf で italic が揃う環境は稀のため。 計画 md の設計判断)。
--   [English]: The font bundle passed to the interpreter. The default
--   (sans-serif) has 2 faces, regular/bold; a spec's @fontFamily@ is
--   resolved via 'pfFamilies' (normalized family name to face pair).
--   Italic still falls back to regular (a design decision, since
--   environments with an italic Japanese .ttf available are rare).
data PNGFonts = PNGFonts
  { pfRegular  :: F.Font
  , pfBold     :: F.Font
  , pfFamilies :: [(String, (F.Font, F.Font))]
    -- ^ [日本語]: 'normFamily' 済 family 名 → (regular, bold)。 未収載 family は既定束へ fallback
    --   [English]: A map from normalized family name (via 'normFamily') to
    --   (regular, bold). A family not listed falls back to the default
    --   bundle.
  }

-- | [日本語]: 候補ディレクトリ (存在するものだけ走査・/usr/share/fonts は再帰)。
--   [English]: The candidate directories (only existing ones are scanned;
--   /usr/share/fonts is scanned recursively).
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

-- | [日本語]: 候補ファイル名 (優先順・小文字比較)。 日本語対応 .ttf を先頭に、
--   最後に Latin のみの DejaVu (JP フォント不在環境の文字化け回避より
--   「とりあえず描ける」 を優先。 JP が必要なら pngFontPath で明示)。
--   [English]: The candidate file names, in priority order (matched
--   case-insensitively). Japanese-capable .ttf fonts come first, with the
--   Latin-only DejaVu last (prioritizing "something renders" over avoiding
--   mojibake in an environment without a Japanese font. Set pngFontPath
--   explicitly if Japanese support is required).
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

-- | [日本語]: generic family "serif" / "monospace" の候補 (regular, bold の対)。
--   [English]: Candidates for the generic families "serif" / "monospace"
--   (regular, bold pairs).
serifCandidates, serifBoldCandidates, monoCandidates, monoBoldCandidates :: [String]
serifCandidates =
  [ "notoserifcjkjp-regular.ttf", "notoserifjp-regular.ttf"
  , "ipamp.ttf", "ipam.ttf", "takaopmincho.ttf", "takaomincho.ttf"
  , "dejavuserif.ttf"
  ]
serifBoldCandidates =
  [ "notoserifcjkjp-bold.ttf", "notoserifjp-bold.ttf", "dejavuserif-bold.ttf" ]
monoCandidates =
  [ "hackgen-regular.ttf", "dejavusansmono.ttf", "hack-regular.ttf" ]
monoBoldCandidates =
  [ "hackgen-bold.ttf", "dejavusansmono-bold.ttf", "hack-bold.ttf" ]

-- | [日本語]: family 名の正規化 (小文字化 + 空白/ハイフン除去)。 索引はファイル名の小文字
--   完全一致なので "DejaVu Sans" → "dejavusans" → dejavusans.ttf のように引ける。
--   [English]: Normalizes a family name (lowercases and strips spaces and
--   hyphens). Since the index is keyed by lowercased file name for exact
--   match, "DejaVu Sans" normalizes to "dejavusans", which then resolves to
--   dejavusans.ttf.
normFamily :: Text -> String
normFamily = filter (\c -> c /= ' ' && c /= '-') . map toLower . T.unpack

-- | [日本語]: フォント load (family 追加解決なし = 従来互換)。
--   [English]: Loads fonts without resolving additional families
--   (backward-compatible behavior).
loadPNGFonts :: PNGConfig -> IO PNGFonts
loadPNGFonts cfg = loadPNGFontsFor cfg []

-- | [日本語]: 既定束 + 使用 family 束を load。
--   'pngFontPath' 明示時は従来どおり__全 text 一括で最優先__ (family 解決は行わない)。
--   解決規則 (fontconfig 非依存を維持):
--   sans-serif/"" → 既定束 / serif・monospace → 専用候補リスト /
--   その他 → 正規化名で @<名>.ttf@ → @<名>-regular.ttf@ (+ @-bold@)。
--   見つからない family は stderr 警告 + 既定束 fallback (loud エラーにはしない)。
--   [English]: Loads the default bundle plus the bundles for the families
--   actually used. When 'pngFontPath' is set explicitly, it still
--   __applies to all text at highest priority__ as before (no family
--   resolution is performed). Resolution rules (staying fontconfig-free):
--   sans-serif/"" resolves to the default bundle; serif/monospace resolve
--   via their dedicated candidate lists; anything else resolves via the
--   normalized name as @<name>.ttf@ then @<name>-regular.ttf@ (plus
--   @-bold@). A family that cannot be found gets a stderr warning and falls
--   back to the default bundle (not a loud error).
loadPNGFontsFor :: PNGConfig -> [Text] -> IO PNGFonts
loadPNGFontsFor cfg families = do
  index <- ttfIndex
  reg <- resolveFont index "regular" (pngFontPath cfg) regularCandidates
  bold <- case pngFontPathBold cfg of
    Just p  -> loadOrDie p
    Nothing -> case lookupCandidates index boldCandidates of
      Just p  -> loadOrDie p
      Nothing -> pure reg          -- bold 不在は regular で代替 (v1 制約)
  fams <- if pngFontPath cfg /= Nothing
            then pure []           -- 明示 path = 全 text 一括 (従来どおり)
            else fmap concat . mapM (resolveFamily index)
                   . nub . filter (`notElem` ["", "sansserif"])
                   . map normFamily $ families
  pure (PNGFonts reg bold fams)
  where
    -- 未解決 family の fallback は drawTextPrim 側 (map 未収載 = 既定束)
    resolveFamily index fam = do
      let (regCands, boldCands) = case fam of
            "serif"     -> (serifCandidates, serifBoldCandidates)
            "monospace" -> (monoCandidates, monoBoldCandidates)
            "mono"      -> (monoCandidates, monoBoldCandidates)
            n           -> ([n ++ ".ttf", n ++ "-regular.ttf"], [n ++ "-bold.ttf"])
      case lookupCandidates index regCands of
        Nothing -> do
          hPutStrLn stderr ("hgg-rasterific: fontFamily \"" ++ fam
                            ++ "\" が見つかりません (候補: "
                            ++ intercalate ", " regCands
                            ++ ")。 既定フォントで代替します。")
          pure []
        Just p  -> do
          r <- loadOrDie p
          b <- case lookupCandidates index boldCands of
                 Just pb -> loadOrDie pb
                 Nothing -> pure r     -- family の bold 不在は同 family regular 代替
          pure [(fam, (r, b))]
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

-- | [日本語]: 探索ディレクトリ配下の .ttf を再帰列挙して (小文字ファイル名, path) の
--   索引にする。 候補リスト順 (= 優先順) に索引を引く。
--   [English]: Recursively enumerates .ttf files under the search
--   directories into a (lowercased file name, path) index. The index is
--   looked up in candidate-list order (priority order).
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

-- | [日本語]: Double → Float (Rasterific の座標は Float)。
--   [English]: Double to Float (Rasterific coordinates are Float).
f :: Double -> Float
f = realToFrac

v2 :: Point -> R.Point
v2 (Point x y) = R.V2 (f x) (f y)

-- | [日本語]: Primitive 列を順に描く。 PClipPush/PTransformPush は対応する Pop までを
--   __再帰グルーピング__して 'R.withClipping' / 'R.withTransformation' に入れる
--   (PDF backend の drawPrims と同型。 Rasterific の clip/transform も
--   scoped combinator なので同じ構図が自然に合う)。
--   対応の取れない Pop は黙って無視 (SVG backend と同じ寛容さ)。
--   [English]: Draws the list of Primitives in order. PClipPush/PTransformPush
--   __recursively groups__ everything up to the matching Pop and wraps it in
--   'R.withClipping' / 'R.withTransformation' (the same shape as the PDF
--   backend's drawPrims — Rasterific's clip/transform are also scoped
--   combinators, so the same structure fits naturally). An unmatched Pop is
--   silently ignored (the same leniency as the SVG backend).
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

-- | [日本語]: 同種 push の入れ子を数えながら、 対応する pop までの内側と残りに割る。
--   対応 pop が無ければ全部内側 (= 末尾まで clip が効く・SVG の開きっ放しと同義)。
--   [English]: Counts nested pushes of the same kind and splits the list
--   into the inner part (up to the matching pop) and the rest. If there is
--   no matching pop, everything is treated as inner (the clip stays in
--   effect to the end — the same as SVG leaving it open).
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

-- | [日本語]: SVG 系 Transform → Rasterific Transformation (y 下向き同士なのでそのまま)。
--   [English]: Converts an SVG-style Transform to a Rasterific
--   Transformation (passed through unchanged, since both have y pointing
--   down).
transformOf :: Transform -> RTr.Transformation
transformOf (TranslateT dx dy) = RTr.translate (R.V2 (f dx) (f dy))
transformOf (ScaleT sx sy)     = RTr.scale (f sx) (f sy)

-- | [日本語]: 単独 primitive の描画。 push/pop は 'drawPrims' が先に消費する。
--   [English]: Draws a single primitive. push/pop are consumed beforehand by
--   'drawPrims'.
drawOne :: PNGFonts -> Primitive -> R.Drawing PixelRGBA8 ()
drawOne _ (PLine a b ls)               = drawLine a b ls
drawOne _ (PRect rect fs ms)           = drawRect rect fs ms
drawOne _ (PCircle c rad fs ms _hover) = drawCircle c rad fs ms
drawOne _ (PPath segs fs ms)           = drawPath segs fs ms
drawOne fonts (PText p s ts)           = drawTextPrim fonts p s ts
drawOne _ _                            = pure ()

-- | [日本語]: 線分: 色/幅/破線。 cap は SVG 既定 butt 相当 (CapStraight 0)。
--   [English]: Line segment: color, width and dash pattern. The cap
--   corresponds to SVG's default butt cap (CapStraight 0).
drawLine :: Point -> Point -> LineStyle -> R.Drawing PixelRGBA8 ()
drawLine a b (LineStyle col w dash) =
  R.withTexture (RT.uniformTexture (colorOf col 1.0)) $
    strokeMaybeDashed dash (f w) (R.line (v2 a) (v2 b))

-- | [日本語]: dash 配列 (px) が空なら実線、 あれば dashedStroke。
--   [English]: A solid stroke if the dash array (px) is empty, otherwise a
--   dashedStroke.
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

-- | [日本語]: 矩形: fill (+ 任意 stroke)。 'Rect' は左上基準 = Rasterific と同じ。
--   [English]: Rectangle: fill (+ optional stroke). 'Rect' is anchored at
--   the top-left, matching Rasterific's convention.
drawRect :: Rect -> FillStyle -> Maybe StrokeStyle -> R.Drawing PixelRGBA8 ()
drawRect rect fs ms = fillShape (rectShape rect) fs ms

-- | [日本語]: 円: fill (+ 任意 stroke)。 hover label は PNG では捨てる (PDF と同じ)。
--   [English]: Circle: fill (+ optional stroke). The hover label is dropped
--   in PNG output (as with PDF).
drawCircle :: Point -> Double -> FillStyle -> Maybe StrokeStyle
           -> R.Drawing PixelRGBA8 ()
drawCircle c rad fs ms = fillShape (R.circle (v2 c) (f rad)) fs ms

-- | [日本語]: Shape 共通: FillStyle (色 + opacity → alpha 合成) で塗り、
--   StrokeStyle があれば縁取る。
--   [English]: Common to all shapes: paints with FillStyle (color + opacity
--   composited to alpha), and outlines with StrokeStyle if present.
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

-- | [日本語]: パス: MoveTo 区切りで subpath ('R.Path') に分割して fill / stroke。
--   fill は全 subpath の primitive をまとめて 1 回 (= SVG の nonzero winding が
--   subpath 横断で効く)、 stroke は subpath ごと (= まとめると subpath 間に
--   偽の接続 join が入る) に塗り分ける。
--   [English]: Path: splits into subpaths ('R.Path') at MoveTo boundaries
--   for fill / stroke. Fill combines the primitives of all subpaths into a
--   single call (so SVG's nonzero winding rule applies across subpaths),
--   while stroke is done per subpath (combining them would introduce a
--   spurious join between subpaths).
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

-- | [日本語]: 'PathSegment' 列 → 'R.Path' 列。 MoveTo で新 subpath、 ClosePath は
--   現 subpath の close フラグ (以降に命令が続く稀ケースは同じ原点で新 subpath)。
--   先頭が MoveTo でない場合は原点 (0,0) 開始 (PDF backend と同じ寛容さ)。
--   [English]: Converts a list of 'PathSegment' to a list of 'R.Path'.
--   MoveTo starts a new subpath; ClosePath sets the close flag on the
--   current subpath (in the rare case more commands follow, a new subpath
--   starts at the same origin). If the list does not start with MoveTo, it
--   starts at the origin (0,0) — the same leniency as the PDF backend.
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

-- | [日本語]: テキスト: 'R.printTextAt' (基準点 = baseline 開始 = SVG の text y と同義)。
--   anchor は 'F.stringBoundingBox' の advance width (_xMax 位置に格納・
--   FontyFruity ソース実測) で x 補正。 tsRotate (degrees CW・SVG 同義) は
--   y 下向き同士なので符号そのまま (PDF と対照的)、 rotate→translate の合成で
--   (x,y) 周りに回す。 tsSize は px → 'F.pixelSizeInPointAtDpi' で 96 dpi の
--   point に変換 ('R.renderDrawing' = 96 dpi 固定)。
--   tsFamily を 'pfFamilies' (正規化名) で解決、 未収載は
--   既定束へ fallback。 italic は引き続き regular で代替。
--   [English]: Text: uses 'R.printTextAt' (whose reference point — the
--   baseline start — matches SVG's text y). The anchor is x-corrected using
--   the advance width from 'F.stringBoundingBox' (stored at _xMax, as
--   verified against the FontyFruity source). tsRotate (degrees CW, matching
--   SVG) keeps its sign as-is since both use y pointing down (unlike PDF),
--   and is applied by composing rotate then translate around (x,y). tsSize
--   is converted from px to a 96 dpi point via 'F.pixelSizeInPointAtDpi'
--   ('R.renderDrawing' is fixed at 96 dpi). tsFamily is resolved via
--   'pfFamilies' (normalized name); a family not listed falls back to the
--   default bundle. Italic still falls back to regular.
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
    isBold = tsWeight ts == "bold"
    font   = case lookup (normFamily (tsFamily ts)) (pfFamilies fonts) of
               Just (r, b) -> if isBold then b else r
               Nothing     -> if isBold then pfBold fonts else pfRegular fonts
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

-- | [日本語]: hex 色 + opacity → PixelRGBA8 (opacity は alpha 成分に畳む =
--   PDF v1 の opacity 問題は PNG では起きない)。
--   [English]: Converts a hex color + opacity to PixelRGBA8 (opacity is
--   folded into the alpha channel — the opacity limitation of PDF v1 does
--   not occur in PNG).
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
