-- |
-- Module      : Graphics.Hgg.Color
-- Description : Type-safe named colors, with the full RGB space in one constructor
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
--   [日本語]: RGB 全色 (256³ = 16,777,216) を単一構成子 @Color@ で連続的に内包し、
--   固定色のタイポをコンパイルエラーに落とす。 名前付き 657 色は
--   @Graphics.Hgg.Color.Named@ にトップレベル束縛として隔離する。
--
--   ★ワイヤ形式は従来通り Text: @ColorEnc@ の @ColorStatic !Text@ は据置で、
--     固定色 combinator が入口で 'toCss' 変換して格納する。 → Render / PS
--     canvas / JSON は無改修 (PS は Color 型を知らず解決済み Text のみ見る)。
--   [English]: The full RGB space (256³ = 16,777,216 colors) is covered
--   continuously by the single constructor @Color@, turning typos in named
--   colors into compile errors. The 657 named colors are isolated as
--   top-level bindings in @Graphics.Hgg.Color.Named@.
--
--   ★The wire format is Text as before: @ColorEnc@'s @ColorStatic !Text@ is
--   kept unchanged, and named-color combinators convert via 'toCss' at the
--   entry point and store the result. Render / the PureScript canvas / JSON
--   therefore need no changes (PureScript does not know the Color type and
--   only ever sees the already-resolved Text).
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.Hgg.Color
  ( Color(..)
  , rgb
  , fromHex
  , fromHexMaybe
  , fromHexA
  , fromHexAMaybe
  , toCss
  ) where

import           Data.Char    (digitToInt, isHexDigit, toLower)
import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Data.Word    (Word8)
import           GHC.Generics (Generic)
import           Numeric      (showHex)

-- ===========================================================================
-- 型
-- ===========================================================================

-- | [日本語]: 固定色。 単一構成子で RGB 全色を張る (各成分 0–255・Word8 で範囲保証)。
--   [English]: A named/fixed color. A single constructor spans the full RGB
--   space (each component ranges 0–255, guaranteed by Word8).
data Color = Color !Word8 !Word8 !Word8
  deriving (Show, Eq, Ord, Generic)

-- ===========================================================================
-- 構築 / 変換
-- ===========================================================================

-- | [日本語]: RGB 成分から構築 ('Color' と同義の読みやすい別名)。
--   [English]: Constructs a color from its RGB components (a readable alias
--   synonymous with 'Color').
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb = Color

-- | [日本語]: @"#rrggbb"@ / @"#rgb"@ (先頭 @#@ は省略可) を解釈。 不正は
--   'Nothing'。 3 桁省略形は各桁を 2 倍展開 (CSS 同様 @#f80@ → @#ff8800@)。
--   total 版。
--   [English]: Parses @"#rrggbb"@ / @"#rgb"@ (the leading @#@ is optional).
--   Invalid input yields 'Nothing'. The 3-digit shorthand expands each digit
--   twice, as in CSS (@#f80@ becomes @#ff8800@). A total function.
fromHexMaybe :: Text -> Maybe Color
fromHexMaybe raw =
  case map toLower (T.unpack (T.dropWhile (== '#') (T.strip raw))) of
    [r, g, b]
      | all isHexDigit [r, g, b]            -> Just (Color (dup r) (dup g) (dup b))
    [r1, r2, g1, g2, b1, b2]
      | all isHexDigit [r1, r2, g1, g2, b1, b2]
                                            -> Just (Color (byte r1 r2) (byte g1 g2) (byte b1 b2))
    _                                       -> Nothing
  where
    byte hi lo = fromIntegral (digitToInt hi * 16 + digitToInt lo)
    dup c      = byte c c

-- | [日本語]: 'fromHexMaybe' の partial 版。 不正入力で 'error' (リテラル用途で簡潔)。
--   [English]: The partial version of 'fromHexMaybe'. Invalid input causes
--   an 'error' (kept concise for literal use).
fromHex :: Text -> Color
fromHex t = fromMaybe err (fromHexMaybe t)
  where err = error ("Graphics.Hgg.Color.fromHex: invalid hex color " ++ show t)

-- | [日本語]: @"#rrggbbaa"@ / @"#rgba"@ (RGBA・先頭 @#@ 省略可) を (色, 不透明度
--   0–1) に分解。 不透明度は @aa/255@ (CSS 8 桁 hex / 4 桁省略形)。 alpha を
--   持たない 6/3 桁は alpha = 1.0 で素通し ('fromHexMaybe' に委譲)。 不正は
--   'Nothing'。 total 版。
--   ★'Color' は RGB のみゆえ alpha を分離して返す (@colorRGBA@ が @color c <>
--   alpha a@ に展開)。
--   [English]: Splits @"#rrggbbaa"@ / @"#rgba"@ (RGBA, leading @#@ optional)
--   into (color, opacity 0–1). Opacity is @aa/255@ (CSS 8-digit hex or the
--   4-digit shorthand). The 6/3-digit forms without alpha pass through with
--   alpha = 1.0 (delegated to 'fromHexMaybe'). Invalid input yields
--   'Nothing'. A total function.
--   ★Since 'Color' is RGB-only, alpha is returned separately (@colorRGBA@
--   expands it to @color c <> alpha a@).
fromHexAMaybe :: Text -> Maybe (Color, Double)
fromHexAMaybe raw =
  case map toLower (T.unpack (T.dropWhile (== '#') (T.strip raw))) of
    [r, g, b, a]
      | all isHexDigit [r, g, b, a]
          -> Just (Color (dup r) (dup g) (dup b), alphaOf (dup a))
    [r1, r2, g1, g2, b1, b2, a1, a2]
      | all isHexDigit [r1, r2, g1, g2, b1, b2, a1, a2]
          -> Just (Color (byte r1 r2) (byte g1 g2) (byte b1 b2), alphaOf (byte a1 a2))
    _   -> (\c -> (c, 1.0)) <$> fromHexMaybe raw   -- 6/3 桁 (alpha 無し) は不透明
  where
    byte hi lo = fromIntegral (digitToInt hi * 16 + digitToInt lo) :: Word8
    dup c      = byte c c
    alphaOf w  = fromIntegral w / 255

-- | [日本語]: 'fromHexAMaybe' の partial 版。 不正入力で 'error' (リテラル用途で簡潔)。
--   [English]: The partial version of 'fromHexAMaybe'. Invalid input causes
--   an 'error' (kept concise for literal use).
fromHexA :: Text -> (Color, Double)
fromHexA t = fromMaybe err (fromHexAMaybe t)
  where err = error ("Graphics.Hgg.Color.fromHexA: invalid hex color " ++ show t)

-- | [日本語]: CSS 文字列 @"#rrggbb"@ に整形 (各成分 2 桁・小文字 hex)。
--   [English]: Formats as the CSS string @"#rrggbb"@ (each component as 2
--   lowercase hex digits).
toCss :: Color -> Text
toCss (Color r g b) = T.pack ('#' : pad r ++ pad g ++ pad b)
  where
    pad n = let s = showHex n "" in if length s == 1 then '0' : s else s
