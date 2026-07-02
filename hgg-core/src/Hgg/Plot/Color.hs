{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hgg.Plot.Color — 型安全な固定色 (Phase 30)。
--
--   RGB 全色 (256³ = 16,777,216) を単一構成子 @Color@ で連続的に内包し、
--   固定色のタイポをコンパイルエラーに落とす。 名前付き 657 色は
--   @Hgg.Plot.Color.Named@ にトップレベル束縛として隔離する。
--
--   ★ワイヤ形式は従来通り Text: @ColorEnc@ の @ColorStatic !Text@ は据置で、
--     固定色 combinator が入口で 'toCss' 変換して格納する。 → Render / PS
--     canvas / JSON は無改修 (PS は Color 型を知らず解決済み Text のみ見る)。
module Hgg.Plot.Color
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

-- | 固定色。 単一構成子で RGB 全色を張る (各成分 0–255・Word8 で範囲保証)。
data Color = Color !Word8 !Word8 !Word8
  deriving (Show, Eq, Ord, Generic)

-- ===========================================================================
-- 構築 / 変換
-- ===========================================================================

-- | RGB 成分から構築 ('Color' と同義の読みやすい別名)。
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb = Color

-- | @"#rrggbb"@ / @"#rgb"@ (先頭 @#@ は省略可) を解釈。 不正は 'Nothing'。
--   3 桁省略形は各桁を 2 倍展開 (CSS 同様 @#f80@ → @#ff8800@)。 total 版。
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

-- | 'fromHexMaybe' の partial 版。 不正入力で 'error' (リテラル用途で簡潔)。
fromHex :: Text -> Color
fromHex t = fromMaybe err (fromHexMaybe t)
  where err = error ("Hgg.Plot.Color.fromHex: invalid hex color " ++ show t)

-- | @"#rrggbbaa"@ / @"#rgba"@ (RGBA・先頭 @#@ 省略可) を (色, 不透明度 0–1) に分解。
--   不透明度は @aa/255@ (CSS 8 桁 hex / 4 桁省略形)。 alpha を持たない 6/3 桁は
--   alpha = 1.0 で素通し ('fromHexMaybe' に委譲)。 不正は 'Nothing'。 total 版。
--   ★'Color' は RGB のみゆえ alpha を分離して返す ('colorRGBA' が @color c <> alpha a@ に展開)。
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

-- | 'fromHexAMaybe' の partial 版。 不正入力で 'error' (リテラル用途で簡潔)。
fromHexA :: Text -> (Color, Double)
fromHexA t = fromMaybe err (fromHexAMaybe t)
  where err = error ("Hgg.Plot.Color.fromHexA: invalid hex color " ++ show t)

-- | CSS 文字列 @"#rrggbb"@ に整形 (各成分 2 桁・小文字 hex)。
toCss :: Color -> Text
toCss (Color r g b) = T.pack ('#' : pad r ++ pad g ++ pad b)
  where
    pad n = let s = showHex n "" in if length s == 1 then '0' : s else s
