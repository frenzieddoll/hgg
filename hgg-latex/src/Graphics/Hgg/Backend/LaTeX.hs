-- |
-- Module      : Graphics.Hgg.Backend.LaTeX
-- Description : LaTeX (TikZ) backend
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- plot-core の '[Primitive]' を TikZ 命令の純テキストに解釈する
-- (SVG/PDF backend と同じ Layer 1 構図)。 座標系は per-primitive で
-- y 反転 (TikZ = 左下原点 y 上向き / Primitive = SVG 系 左上原点 y 下向き。
-- PDF backend と同方針 = global flip は text が鏡像になるため不可)。
--
-- emit 規約は A1 実測で確定 (design/phase54-latex/README.md):
--   * 座標・寸法は明示 bp (= PostScript pt = core の pt。 TeX pt とは 0.37% 差)
--   * 数値は固定小数 3 桁 (TikZ は指数表記の寸法を読めない)
--   * 色は preamble に \definecolor{pcRRGGBB}{HTML}{RRGGBB} で集約
--   * text は node anchor (base west/base/base east) + rotate CCW 恒等
--   * 既定 preamble は DejaVuSans.sty (core の charWidthEm 較正元と同フォント、
--     A1 実測で見積り比 0.93-1.11 = SVG backend と同等誤差クラス)
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Backend.LaTeX
  ( -- * 通常 (Resolver 不要 = inline 列のみの図)
    saveTeX
  , renderTeX
    -- * Resolver 同伴 (= 'ColByName' を含む図)
  , saveTeXWith
  , renderTeXWith
    -- * 出力設定 (standalone / 素片 mode・preamble 差し替え・CJK family)
  , saveTeXConfigured
  , renderTeXConfigured
  , TeXConfig (..)
  , CJKMode (..)
  , defaultTeXConfig
  , luaLaTeXConfig
    -- * Phase 14 系: BoundPlot (df バインド済) を描画する
  , saveTeXBound
    -- * 低レベル: [Primitive] を直接 TikZ 化 (3D backend glue 用)
  , savePrimitivesTeX
  , renderPrimitivesTeX
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
import           Data.Char             (isHexDigit, toUpper)
import           Data.List             (nub)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Numeric               (showFFloat)
import           System.IO             (hPutStrLn, stderr)

-- ===========================================================================
-- 入口 (SVG backend の saveSVG / saveSVGWith / saveSVGBound と対称)
-- ===========================================================================

-- | LaTeX (standalone documentclass) ファイルに保存。 Resolver 不要
-- (= inline 列のみの図、 = 通常)。 列名参照を含む図は 'saveTeXWith'、
-- DataFrame は 'saveTeXBound' (@df |>> spec@)。
saveTeX :: FilePath -> VisualSpec -> IO ()
saveTeX path = saveTeXWith path emptyResolver

-- | 'Resolver' を渡して LaTeX ファイルに保存。 'ColByName' を含む図用。
saveTeXWith :: FilePath -> Resolver -> VisualSpec -> IO ()
saveTeXWith = saveTeXConfigured defaultTeXConfig

-- | 'VisualSpec' を LaTeX text に。 Resolver 不要版。
renderTeX :: VisualSpec -> Text
renderTeX = renderTeXWith emptyResolver

-- | 'Resolver' を渡して 'VisualSpec' を LaTeX text に。
renderTeXWith :: Resolver -> VisualSpec -> Text
renderTeXWith = renderTeXConfigured defaultTeXConfig

-- ===========================================================================
-- 出力設定 (rasterific の PNGConfig / savePNGConfigured と対称)
-- ===========================================================================

-- | LaTeX 出力設定。 'defaultTeXConfig' から record update で部分指定する。
data TeXConfig = TeXConfig
  { texStandalone    :: Bool
    -- ^ True (既定) = standalone documentclass の単体コンパイル可能文書。
    --   False = @tikzpicture@ 環境の素片のみ (本文へ @\\input@ する用。
    --   親文書側に tikz / DejaVuSans / (日本語時) CJKutf8 の \\usepackage が必要)
  , texExtraPreamble :: [Text]
    -- ^ preamble 追加行 (font 差し替え等)。 standalone 時のみ有効
  , texCJKMode       :: CJKMode
    -- ^ CJK ラベルの扱い (既定 'CJKWrap' = pdflatex 向け)
  , texCJKFamily     :: Text
    -- ^ 'CJKWrap' 時の CJKutf8 font family (既定 "ipxg" = IPAexゴシック。
    --   明朝 = "ipxm")。 ラベルに CJK 文字がある時だけ CJK 環境と
    --   \\usepackage{CJKutf8} を出す
  }

-- | CJK ラベルの出力方式。 生成 .tex を組版するエンジンに合わせて選ぶ。
data CJKMode
  = CJKWrap
    -- ^ CJK 文字を含むラベルを @\\begin{CJK}{UTF8}{family}@ で包み、
    --   \\usepackage{CJKutf8} を自動付与 (**pdflatex 向け**、 動作実測済)
  | CJKRaw
    -- ^ 包まず生 UTF-8 のまま出す (**lualatex / xelatex 向け**。 CJK フォント
    --   解決は preamble 側 = luatexja / fontspec 等に委ねる。 lualatex +
    --   luatexja で組版実測済 = 'luaLaTeXConfig')。
    --   ★逆組合せの footgun (2026-07-09 実測): 'CJKWrap' の .tex を lualatex
    --   で組むと **エラーにならず CJK 文字だけ黙って脱落**する。 lualatex で
    --   組むなら必ず CJKRaw を使うこと
  deriving (Show, Eq)

-- | 既定設定 (standalone + pdflatex 向け CJKWrap + IPAexゴシック)。
defaultTeXConfig :: TeXConfig
defaultTeXConfig = TeXConfig
  { texStandalone    = True
  , texExtraPreamble = []
  , texCJKMode       = CJKWrap
  , texCJKFamily     = "ipxg"
  }

-- | lualatex 向け preset (組版実測済 2026-07-09): CJK は wrap せず生 UTF-8、
-- 日本語フォントは luatexja (既定 = 原ノ味) に委ねる。
-- @saveTeXConfigured luaLaTeXConfig path r spec@ → @lualatex path@。
luaLaTeXConfig :: TeXConfig
luaLaTeXConfig = defaultTeXConfig
  { texCJKMode       = CJKRaw
  , texExtraPreamble = ["\\usepackage{luatexja}"]
  }

-- | 設定付き保存 ('savePNGConfigured' と対称)。
saveTeXConfigured :: TeXConfig -> FilePath -> Resolver -> VisualSpec -> IO ()
saveTeXConfigured cfg path r spec =
  TIO.writeFile path (renderTeXConfigured cfg r spec)

-- | 設定付き render。
-- ★ PDF backend と同じく pt 直結 (k=1、 dpi 乗算なし)。 layout/prims の
-- 純 pt を bp 単位でそのまま書く (Phase 33 B5 の唯一 dpi 適用点は raster 系のみ)。
renderTeXConfigured :: TeXConfig -> Resolver -> VisualSpec -> Text
renderTeXConfigured cfg r spec =
  let layout = computeLayout r spec
      prims  = renderToPrimitives r layout spec
      ViewportSize w h = lpViewport layout
  in renderPrimitivesTeXConfigured cfg w h prims

-- | 'BoundPlot' (= @df |>> spec@ の結果) を LaTeX ファイルに保存。
-- Error severity の検証診断は stderr に報告してから書き出す
-- (saveSVGBound と同じ lenient 既定)。
saveTeXBound :: FilePath -> BoundPlot -> IO ()
saveTeXBound path (BoundPlot r spec diags) = do
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        (filter ((== SevError) . diagnosticSeverity) diags)
  saveTeXWith path r spec

-- | [Primitive] 列を所与のキャンバスサイズで LaTeX に直接書く低レベル経路。
savePrimitivesTeX :: FilePath -> Int -> Int -> [Primitive] -> IO ()
savePrimitivesTeX path w h prims =
  TIO.writeFile path (renderPrimitivesTeX w h prims)

-- ===========================================================================
-- 文書骨格
-- ===========================================================================

-- | [Primitive] → standalone LaTeX 文書 (純関数、 golden test 可)。 既定設定。
renderPrimitivesTeX :: Int -> Int -> [Primitive] -> Text
renderPrimitivesTeX = renderPrimitivesTeXConfigured defaultTeXConfig

-- | [Primitive] → LaTeX text (設定付き・純関数)。
renderPrimitivesTeXConfigured :: TeXConfig -> Int -> Int -> [Primitive] -> Text
renderPrimitivesTeXConfigured cfg w h prims = T.unlines $
  docHead ++ body ++ docFoot
  where
    hasCJK = texCJKMode cfg == CJKWrap && any primHasCJK prims
    primHasCJK (PText _ s _) = T.any (> '\xFF') s
    primHasCJK _             = False
    docHead
      | texStandalone cfg =
          [ "% Generated by hgg-latex"
          , "\\documentclass[border=0pt]{standalone}"
          , "\\usepackage[T1]{fontenc}"
          , "\\usepackage{DejaVuSans}"
          , "\\usepackage{tikz}"
          ]
          ++ [ "\\usepackage{CJKutf8}" | hasCJK ]
          ++ texExtraPreamble cfg
          ++ colorDefs prims
          ++ [ "\\begin{document}" ]
      | otherwise =
          -- 素片 mode: 親文書に tikz / DejaVuSans / (CJK 時) CJKutf8 が必要
          [ "% Generated by hgg-latex (fragment mode)"
          , "% 要 preamble: \\usepackage{tikz}, \\usepackage{DejaVuSans}"
            <> (if hasCJK then ", \\usepackage{CJKutf8}" else "")
          ]
          ++ colorDefs prims
    body =
      [ "\\begin{tikzpicture}"
        -- viewport を bounding box として固定 (primitive がキャンバス全域に
        -- 届かなくても図サイズを SVG viewport と一致させる)
      , "\\path[use as bounding box] (0bp,0bp) rectangle ("
          <> bp (fromIntegral w) <> "," <> bp (fromIntegral h) <> ");"
      ]
      ++ drawPrims cfg (fromIntegral h) prims ++
      [ "\\end{tikzpicture}" ]
    docFoot
      | texStandalone cfg = [ "\\end{document}" ]
      | otherwise         = []

-- ===========================================================================
-- 色 (theme 色は全て "#rrggbb" hex。 preamble に \definecolor で集約)
-- ===========================================================================

-- | primitive 列が使う色を重複排除して \definecolor 行に。
colorDefs :: [Primitive] -> [Text]
colorDefs prims =
  [ "\\definecolor{pc" <> hx <> "}{HTML}{" <> hx <> "}"
  | hx <- nub (concatMap colorsOf prims) ]
  where
    colorsOf p = case p of
      PLine _ _ (LineStyle c _ _) -> [hex6 c]
      PRect _ fs ms               -> fillC fs ++ strokeC ms
      PCircle _ _ fs ms _         -> fillC fs ++ strokeC ms
      PPath _ fs ms               -> fillC fs ++ strokeC ms
      PText _ _ ts                -> [hex6 (tsColor ts)]
      _                           -> []
    fillC (FillStyle c _) = [hex6 c | c /= "none"]
    strokeC Nothing                   = []
    strokeC (Just (StrokeStyle c _))  = [hex6 c]

-- | TikZ 色名 (pcRRGGBB) 参照。
colorRef :: Text -> Text
colorRef c = "pc" <> hex6 c

-- | 色文字列 → 6 桁大文字 hex。 "#rgb" は倍長化、 named / 不正は
-- PDF backend の colorOf と同じ fallback (white 以外は黒)。
hex6 :: Text -> Text
hex6 t = case T.unpack t of
  ['#', r1, r2, g1, g2, b1, b2]
    | all isHexDigit [r1, r2, g1, g2, b1, b2] ->
        T.pack (map toUpper [r1, r2, g1, g2, b1, b2])
  ['#', r, g, b]
    | all isHexDigit [r, g, b] -> T.pack (map toUpper [r, r, g, g, b, b])
  "white" -> "FFFFFF"
  _       -> "000000"

-- ===========================================================================
-- Primitive 解釈器
-- ===========================================================================

-- | Primitive 列を順に TikZ 行へ。 第 1 引数 = viewport 高さ (y 反転用)。
--
-- PClipPush/PTransformPush は対応する Pop までを**再帰グルーピング**して
-- @\\begin{scope}@ に入れる (PDF backend の 'breakMatch' と同型。 TikZ の
-- clip / cm も scope 終端でしか戻せない)。 対応の取れない Pop は黙って無視。
drawPrims :: TeXConfig -> Double -> [Primitive] -> [Text]
drawPrims cfg h = go
  where
    go [] = []
    go (PClipPush rect : rest) =
      let (inner, after) = breakMatch isClipPush isClipPop rest
      in ("\\begin{scope}" : clipRectOf h rect : go inner)
         ++ ("\\end{scope}" : go after)
    go (PTransformPush tr : rest) =
      let (inner, after) = breakMatch isTrPush isTrPop rest
      in ("\\begin{scope}[cm={" <> cmOf h tr <> "}]" : go inner)
         ++ ("\\end{scope}" : go after)
    go (PClipPop : rest)      = go rest
    go (PTransformPop : rest) = go rest
    go (p : rest)             = drawOne cfg h p ++ go rest

    isClipPush p = case p of { PClipPush _ -> True; _ -> False }
    isClipPop  p = case p of { PClipPop    -> True; _ -> False }
    isTrPush   p = case p of { PTransformPush _ -> True; _ -> False }
    isTrPop    p = case p of { PTransformPop    -> True; _ -> False }

-- | 同種 push の入れ子を数えながら、 対応する pop までの内側と残りに割る
-- (PDF backend の breakMatch と同一。 対応 pop 無し = 末尾まで scope)。
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

-- | 矩形 clip ('Rect' は左上基準 → y 反転して両角)。
clipRectOf :: Double -> Rect -> Text
clipRectOf h (Rect x y w rh) =
  "\\clip " <> xy x (h - y - rh) <> " rectangle " <> xy (x + w) (h - y) <> ";"

-- | SVG 系 Transform → TikZ cm= 明示行列 (a,b,c,d,(tx,ty))。 y 反転 F が
-- per-primitive に掛かるため **F∘M∘F (共役)** で写す — PDF backend の
-- 'matrixOf' と同一式: translate (dx,dy) → (dx,−dy) / scale (sx,sy) →
-- 平行移動 (0, h(1−sy)) 付き scale。 ※現状 core は PTransformPush を発行しない
-- (SVG backend も未対応) — 将来の発行に備えた整合実装。
cmOf :: Double -> Transform -> Text
cmOf _ (TranslateT dx dy) =
  "1,0,0,1,(" <> bp dx <> "," <> bp (negate dy) <> ")"
cmOf h (ScaleT sx sy) =
  num sx <> ",0,0," <> num sy <> ",(0bp," <> bp (h * (1 - sy)) <> ")"

-- | 単独 primitive → TikZ 行 (0 行 = skip)。 push/pop は 'drawPrims' が先に消費。
drawOne :: TeXConfig -> Double -> Primitive -> [Text]
drawOne _   h (PLine a b ls)              = [drawLine h a b ls]
drawOne _   h (PRect rect fs ms)          = drawRect h rect fs ms
drawOne _   h (PCircle c rad fs ms _hov)  = drawCircle h c rad fs ms
drawOne _   h (PPath segs fs ms)          = drawPath h segs fs ms
drawOne cfg h (PText p s ts)              = [drawText cfg h p s ts]
drawOne _   _ _                           = []

-- | パス: MoveTo/LineTo/CurveTo/ClosePath を TikZ path 式に写す。
-- 途中の MoveTo は subpath 切替 (座標の並置 = TikZ の move-to)。
drawPath :: Double -> [PathSegment] -> FillStyle -> Maybe StrokeStyle -> [Text]
drawPath _ [] _ _ = []
drawPath h segs fs ms =
  case fillDrawOpts fs ms of
    [] -> []
    os -> [ T.concat
      [ "\\path[", T.intercalate ", " os, "]", T.concat (map seg segs), ";" ] ]
  where
    seg (MoveTo p)        = " " <> pt h p
    seg (LineTo p)        = " -- " <> pt h p
    seg (CurveTo c1 c2 p) = " .. controls " <> pt h c1 <> " and " <> pt h c2
                            <> " .. " <> pt h p
    seg ClosePath         = " -- cycle"

-- | 線分: \draw[line width, color, dash pattern]。
drawLine :: Double -> Point -> Point -> LineStyle -> Text
drawLine h a b (LineStyle col w dash) = T.concat
  [ "\\draw[", T.intercalate ", "
      ([ "line width=" <> bp w, "color=" <> colorRef col ] ++ dashOpt dash)
  , "] ", pt h a, " -- ", pt h b, ";" ]

-- | LineStyle の dash 配列 (pt) → TikZ dash pattern。 空 = 実線 (option 無し)。
-- 奇数長は SVG stroke-dasharray と同じく 2 周期に複製して on/off 対にする。
dashOpt :: [Double] -> [Text]
dashOpt []   = []
dashOpt ds   =
  let ds' = if even (length ds) then ds else ds ++ ds
      onOff (x : y : rest) = "on " <> bp x <> " off " <> bp y : onOff rest
      onOff _              = []
  in [ "dash pattern=" <> T.unwords (onOff ds') ]

-- | 矩形: \path[fill/draw] (x0,y0) rectangle (x1,y1)。 'Rect' は左上基準。
drawRect :: Double -> Rect -> FillStyle -> Maybe StrokeStyle -> [Text]
drawRect h (Rect x y w rh) fs ms =
  case fillDrawOpts fs ms of
    [] -> []
    os -> [ T.concat
      [ "\\path[", T.intercalate ", " os, "] "
      , xy x (h - y - rh), " rectangle ", xy (x + w) (h - y), ";" ] ]

-- | 円: \path[fill/draw] circle。 hover label は LaTeX では捨てる (PDF と同じ)。
drawCircle :: Double -> Point -> Double -> FillStyle -> Maybe StrokeStyle
           -> [Text]
drawCircle h (Point cx cy) rad fs ms =
  case fillDrawOpts fs ms of
    [] -> []
    os -> [ T.concat
      [ "\\path[", T.intercalate ", " os, "] "
      , xy cx (h - cy), " circle [radius=", bp rad, "];" ] ]

-- | fill (色 + opacity) / draw (色 + 線幅) の option 列。 両方無しは [] = skip。
fillDrawOpts :: FillStyle -> Maybe StrokeStyle -> [Text]
fillDrawOpts (FillStyle fc opa) ms =
  (if fc == "none" then []
   else [ "fill=" <> colorRef fc ]
     ++ [ "fill opacity=" <> num opa | opa /= 1 ])
  ++ case ms of
       Nothing                  -> []
       Just (StrokeStyle sc sw) ->
         [ "draw=" <> colorRef sc, "line width=" <> bp sw ]

-- | PText: TikZ node。 anchor = base west/base/base east (SVG の
-- text-anchor start/middle/end + alphabetic baseline に対応、 A1 probe 検証済)。
-- tsRotate は core canonical CCW = TikZ CCW で恒等。 font は \sffamily
-- (DejaVuSans.sty が sans を DejaVu 化) + \fontsize{bp}{1.2bp}。
--
-- ラベルの解釈 (A4):
--   * 全体が @$...$@ の文字列 = **数式 passthrough** (escape せず生で出す。
--     LaTeX backend の固有価値。 ★他 backend では $ 込みで文字列描画される)
--   * CJK 文字を含む = 'texCJKFamily' の CJK 環境で包む (pdflatex + CJKutf8)
drawText :: TeXConfig -> Double -> Point -> Text -> TextStyle -> Text
drawText cfg h (Point x y) txt ts = T.concat
  [ "\\node[", T.intercalate ", " opts, "] at ", xy x (h - y)
  , " {", styleCmds, content, "};" ]
  where
    content
      | isMathLabel txt = txt
      | T.any (> '\xFF') txt && texCJKMode cfg == CJKWrap = T.concat
          [ "\\begin{CJK}{UTF8}{", texCJKFamily cfg, "}"
          , escapeTeX txt, "\\end{CJK}" ]
      | otherwise       = escapeTeX txt
    opts = [ "anchor=" <> anchorOf (tsAnchor ts)
           , "inner sep=0"
           , "text=" <> colorRef (tsColor ts) ]
           ++ [ "rotate=" <> num (tsRotate ts) | tsRotate ts /= 0 ]
    styleCmds = T.concat
      [ familyCmd (tsFamily ts)
      , "\\fontsize{", bp (tsSize ts), "}{", bp (1.2 * tsSize ts)
      , "}\\selectfont "
      , if tsWeight ts == "bold" then "\\bfseries " else ""
      , if tsItalic ts then "\\itshape " else "" ]
    anchorOf AnchorStart  = "base west"
    anchorOf AnchorMiddle = "base"
    anchorOf AnchorEnd    = "base east"
    familyCmd fam
      | "serif" == fam || "Times" `T.isInfixOf` fam     = "\\rmfamily"
      | "mono" `T.isInfixOf` fam
        || "Courier" `T.isInfixOf` fam                  = "\\ttfamily"
      | otherwise                                       = "\\sffamily"

-- ===========================================================================
-- 書式 helper
-- ===========================================================================

-- | 全体が @$...$@ で包まれた数式ラベルか (passthrough 判定)。
-- 内部に追加の $ を含む場合 (= "$a$ and $b$" のような混在) は数式扱いしない。
isMathLabel :: Text -> Bool
isMathLabel t =
     T.length t > 2
  && "$" `T.isPrefixOf` t
  && "$" `T.isSuffixOf` t
  && not ("$" `T.isInfixOf` T.drop 1 (T.dropEnd 1 t))

-- | LaTeX 特殊文字の escape (text mode)。 数式 passthrough は 'isMathLabel'。
escapeTeX :: Text -> Text
escapeTeX = T.concatMap esc
  where
    esc '\\' = "\\textbackslash{}"
    esc '#'  = "\\#"
    esc '$'  = "\\$"
    esc '%'  = "\\%"
    esc '&'  = "\\&"
    esc '_'  = "\\_"
    esc '{'  = "\\{"
    esc '}'  = "\\}"
    esc '~'  = "\\textasciitilde{}"
    esc '^'  = "\\textasciicircum{}"
    esc c    = T.singleton c

-- | 座標 (x, y は既に TikZ 系 = y 反転済で渡る)。
xy :: Double -> Double -> Text
xy x y = "(" <> bp x <> "," <> bp y <> ")"

-- | SVG 系座標 → TikZ 座標 (y 反転して括弧書き)。
pt :: Double -> Point -> Text
pt h (Point x y) = xy x (h - y)

-- | 寸法 (bp 単位明示)。 固定小数 3 桁 = TikZ が読めない指数表記を回避しつつ
-- 0.001bp (≈ 0.35µm) 精度で決定論的。
bp :: Double -> Text
bp v = num v <> "bp"

-- | 無次元数 (opacity / rotate)。 固定小数 3 桁。
num :: Double -> Text
num v = T.pack (showFFloat (Just 3) v "")
