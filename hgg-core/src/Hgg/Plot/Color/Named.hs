{-# LANGUAGE OverloadedStrings #-}

-- | Hgg.Plot.Color.Named — R @colors()@ の 657 名前付き色 (Phase 30)。
--
--   ★このファイルは機械生成物 (scripts/gen-named-colors.py)。 手で編集しない。
--   一次ソース = R src/library/grDevices/src/colors.c の ColorDataBase[]
--   (= R colors() の実体・657 色)。 値は捏造せず colors.c の hex から導出。
--
--   タイポは文字列ルックアップでなくトップレベル束縛ゆえコンパイルエラーで防げる。
--   grey/gray の両綴り・連番 (grey0..grey100 等) も R に倣って保持。
module Hgg.Plot.Color.Named where

import Hgg.Plot.Color (Color (..))


white :: Color
white = Color 255 255 255   -- #FFFFFF

aliceblue :: Color
aliceblue = Color 240 248 255   -- #F0F8FF

antiquewhite :: Color
antiquewhite = Color 250 235 215   -- #FAEBD7

antiquewhite1 :: Color
antiquewhite1 = Color 255 239 219   -- #FFEFDB

antiquewhite2 :: Color
antiquewhite2 = Color 238 223 204   -- #EEDFCC

antiquewhite3 :: Color
antiquewhite3 = Color 205 192 176   -- #CDC0B0

antiquewhite4 :: Color
antiquewhite4 = Color 139 131 120   -- #8B8378

aquamarine :: Color
aquamarine = Color 127 255 212   -- #7FFFD4

aquamarine1 :: Color
aquamarine1 = Color 127 255 212   -- #7FFFD4

aquamarine2 :: Color
aquamarine2 = Color 118 238 198   -- #76EEC6

aquamarine3 :: Color
aquamarine3 = Color 102 205 170   -- #66CDAA

aquamarine4 :: Color
aquamarine4 = Color 69 139 116   -- #458B74

azure :: Color
azure = Color 240 255 255   -- #F0FFFF

azure1 :: Color
azure1 = Color 240 255 255   -- #F0FFFF

azure2 :: Color
azure2 = Color 224 238 238   -- #E0EEEE

azure3 :: Color
azure3 = Color 193 205 205   -- #C1CDCD

azure4 :: Color
azure4 = Color 131 139 139   -- #838B8B

beige :: Color
beige = Color 245 245 220   -- #F5F5DC

bisque :: Color
bisque = Color 255 228 196   -- #FFE4C4

bisque1 :: Color
bisque1 = Color 255 228 196   -- #FFE4C4

bisque2 :: Color
bisque2 = Color 238 213 183   -- #EED5B7

bisque3 :: Color
bisque3 = Color 205 183 158   -- #CDB79E

bisque4 :: Color
bisque4 = Color 139 125 107   -- #8B7D6B

black :: Color
black = Color 0 0 0   -- #000000

blanchedalmond :: Color
blanchedalmond = Color 255 235 205   -- #FFEBCD

blue :: Color
blue = Color 0 0 255   -- #0000FF

blue1 :: Color
blue1 = Color 0 0 255   -- #0000FF

blue2 :: Color
blue2 = Color 0 0 238   -- #0000EE

blue3 :: Color
blue3 = Color 0 0 205   -- #0000CD

blue4 :: Color
blue4 = Color 0 0 139   -- #00008B

blueviolet :: Color
blueviolet = Color 138 43 226   -- #8A2BE2

brown :: Color
brown = Color 165 42 42   -- #A52A2A

brown1 :: Color
brown1 = Color 255 64 64   -- #FF4040

brown2 :: Color
brown2 = Color 238 59 59   -- #EE3B3B

brown3 :: Color
brown3 = Color 205 51 51   -- #CD3333

brown4 :: Color
brown4 = Color 139 35 35   -- #8B2323

burlywood :: Color
burlywood = Color 222 184 135   -- #DEB887

burlywood1 :: Color
burlywood1 = Color 255 211 155   -- #FFD39B

burlywood2 :: Color
burlywood2 = Color 238 197 145   -- #EEC591

burlywood3 :: Color
burlywood3 = Color 205 170 125   -- #CDAA7D

burlywood4 :: Color
burlywood4 = Color 139 115 85   -- #8B7355

cadetblue :: Color
cadetblue = Color 95 158 160   -- #5F9EA0

cadetblue1 :: Color
cadetblue1 = Color 152 245 255   -- #98F5FF

cadetblue2 :: Color
cadetblue2 = Color 142 229 238   -- #8EE5EE

cadetblue3 :: Color
cadetblue3 = Color 122 197 205   -- #7AC5CD

cadetblue4 :: Color
cadetblue4 = Color 83 134 139   -- #53868B

chartreuse :: Color
chartreuse = Color 127 255 0   -- #7FFF00

chartreuse1 :: Color
chartreuse1 = Color 127 255 0   -- #7FFF00

chartreuse2 :: Color
chartreuse2 = Color 118 238 0   -- #76EE00

chartreuse3 :: Color
chartreuse3 = Color 102 205 0   -- #66CD00

chartreuse4 :: Color
chartreuse4 = Color 69 139 0   -- #458B00

chocolate :: Color
chocolate = Color 210 105 30   -- #D2691E

chocolate1 :: Color
chocolate1 = Color 255 127 36   -- #FF7F24

chocolate2 :: Color
chocolate2 = Color 238 118 33   -- #EE7621

chocolate3 :: Color
chocolate3 = Color 205 102 29   -- #CD661D

chocolate4 :: Color
chocolate4 = Color 139 69 19   -- #8B4513

coral :: Color
coral = Color 255 127 80   -- #FF7F50

coral1 :: Color
coral1 = Color 255 114 86   -- #FF7256

coral2 :: Color
coral2 = Color 238 106 80   -- #EE6A50

coral3 :: Color
coral3 = Color 205 91 69   -- #CD5B45

coral4 :: Color
coral4 = Color 139 62 47   -- #8B3E2F

cornflowerblue :: Color
cornflowerblue = Color 100 149 237   -- #6495ED

cornsilk :: Color
cornsilk = Color 255 248 220   -- #FFF8DC

cornsilk1 :: Color
cornsilk1 = Color 255 248 220   -- #FFF8DC

cornsilk2 :: Color
cornsilk2 = Color 238 232 205   -- #EEE8CD

cornsilk3 :: Color
cornsilk3 = Color 205 200 177   -- #CDC8B1

cornsilk4 :: Color
cornsilk4 = Color 139 136 120   -- #8B8878

cyan :: Color
cyan = Color 0 255 255   -- #00FFFF

cyan1 :: Color
cyan1 = Color 0 255 255   -- #00FFFF

cyan2 :: Color
cyan2 = Color 0 238 238   -- #00EEEE

cyan3 :: Color
cyan3 = Color 0 205 205   -- #00CDCD

cyan4 :: Color
cyan4 = Color 0 139 139   -- #008B8B

darkblue :: Color
darkblue = Color 0 0 139   -- #00008B

darkcyan :: Color
darkcyan = Color 0 139 139   -- #008B8B

darkgoldenrod :: Color
darkgoldenrod = Color 184 134 11   -- #B8860B

darkgoldenrod1 :: Color
darkgoldenrod1 = Color 255 185 15   -- #FFB90F

darkgoldenrod2 :: Color
darkgoldenrod2 = Color 238 173 14   -- #EEAD0E

darkgoldenrod3 :: Color
darkgoldenrod3 = Color 205 149 12   -- #CD950C

darkgoldenrod4 :: Color
darkgoldenrod4 = Color 139 101 8   -- #8B6508

darkgray :: Color
darkgray = Color 169 169 169   -- #A9A9A9

darkgreen :: Color
darkgreen = Color 0 100 0   -- #006400

darkgrey :: Color
darkgrey = Color 169 169 169   -- #A9A9A9

darkkhaki :: Color
darkkhaki = Color 189 183 107   -- #BDB76B

darkmagenta :: Color
darkmagenta = Color 139 0 139   -- #8B008B

darkolivegreen :: Color
darkolivegreen = Color 85 107 47   -- #556B2F

darkolivegreen1 :: Color
darkolivegreen1 = Color 202 255 112   -- #CAFF70

darkolivegreen2 :: Color
darkolivegreen2 = Color 188 238 104   -- #BCEE68

darkolivegreen3 :: Color
darkolivegreen3 = Color 162 205 90   -- #A2CD5A

darkolivegreen4 :: Color
darkolivegreen4 = Color 110 139 61   -- #6E8B3D

darkorange :: Color
darkorange = Color 255 140 0   -- #FF8C00

darkorange1 :: Color
darkorange1 = Color 255 127 0   -- #FF7F00

darkorange2 :: Color
darkorange2 = Color 238 118 0   -- #EE7600

darkorange3 :: Color
darkorange3 = Color 205 102 0   -- #CD6600

darkorange4 :: Color
darkorange4 = Color 139 69 0   -- #8B4500

darkorchid :: Color
darkorchid = Color 153 50 204   -- #9932CC

darkorchid1 :: Color
darkorchid1 = Color 191 62 255   -- #BF3EFF

darkorchid2 :: Color
darkorchid2 = Color 178 58 238   -- #B23AEE

darkorchid3 :: Color
darkorchid3 = Color 154 50 205   -- #9A32CD

darkorchid4 :: Color
darkorchid4 = Color 104 34 139   -- #68228B

darkred :: Color
darkred = Color 139 0 0   -- #8B0000

darksalmon :: Color
darksalmon = Color 233 150 122   -- #E9967A

darkseagreen :: Color
darkseagreen = Color 143 188 143   -- #8FBC8F

darkseagreen1 :: Color
darkseagreen1 = Color 193 255 193   -- #C1FFC1

darkseagreen2 :: Color
darkseagreen2 = Color 180 238 180   -- #B4EEB4

darkseagreen3 :: Color
darkseagreen3 = Color 155 205 155   -- #9BCD9B

darkseagreen4 :: Color
darkseagreen4 = Color 105 139 105   -- #698B69

darkslateblue :: Color
darkslateblue = Color 72 61 139   -- #483D8B

darkslategray :: Color
darkslategray = Color 47 79 79   -- #2F4F4F

darkslategray1 :: Color
darkslategray1 = Color 151 255 255   -- #97FFFF

darkslategray2 :: Color
darkslategray2 = Color 141 238 238   -- #8DEEEE

darkslategray3 :: Color
darkslategray3 = Color 121 205 205   -- #79CDCD

darkslategray4 :: Color
darkslategray4 = Color 82 139 139   -- #528B8B

darkslategrey :: Color
darkslategrey = Color 47 79 79   -- #2F4F4F

darkturquoise :: Color
darkturquoise = Color 0 206 209   -- #00CED1

darkviolet :: Color
darkviolet = Color 148 0 211   -- #9400D3

deeppink :: Color
deeppink = Color 255 20 147   -- #FF1493

deeppink1 :: Color
deeppink1 = Color 255 20 147   -- #FF1493

deeppink2 :: Color
deeppink2 = Color 238 18 137   -- #EE1289

deeppink3 :: Color
deeppink3 = Color 205 16 118   -- #CD1076

deeppink4 :: Color
deeppink4 = Color 139 10 80   -- #8B0A50

deepskyblue :: Color
deepskyblue = Color 0 191 255   -- #00BFFF

deepskyblue1 :: Color
deepskyblue1 = Color 0 191 255   -- #00BFFF

deepskyblue2 :: Color
deepskyblue2 = Color 0 178 238   -- #00B2EE

deepskyblue3 :: Color
deepskyblue3 = Color 0 154 205   -- #009ACD

deepskyblue4 :: Color
deepskyblue4 = Color 0 104 139   -- #00688B

dimgray :: Color
dimgray = Color 105 105 105   -- #696969

dimgrey :: Color
dimgrey = Color 105 105 105   -- #696969

dodgerblue :: Color
dodgerblue = Color 30 144 255   -- #1E90FF

dodgerblue1 :: Color
dodgerblue1 = Color 30 144 255   -- #1E90FF

dodgerblue2 :: Color
dodgerblue2 = Color 28 134 238   -- #1C86EE

dodgerblue3 :: Color
dodgerblue3 = Color 24 116 205   -- #1874CD

dodgerblue4 :: Color
dodgerblue4 = Color 16 78 139   -- #104E8B

firebrick :: Color
firebrick = Color 178 34 34   -- #B22222

firebrick1 :: Color
firebrick1 = Color 255 48 48   -- #FF3030

firebrick2 :: Color
firebrick2 = Color 238 44 44   -- #EE2C2C

firebrick3 :: Color
firebrick3 = Color 205 38 38   -- #CD2626

firebrick4 :: Color
firebrick4 = Color 139 26 26   -- #8B1A1A

floralwhite :: Color
floralwhite = Color 255 250 240   -- #FFFAF0

forestgreen :: Color
forestgreen = Color 34 139 34   -- #228B22

gainsboro :: Color
gainsboro = Color 220 220 220   -- #DCDCDC

ghostwhite :: Color
ghostwhite = Color 248 248 255   -- #F8F8FF

gold :: Color
gold = Color 255 215 0   -- #FFD700

gold1 :: Color
gold1 = Color 255 215 0   -- #FFD700

gold2 :: Color
gold2 = Color 238 201 0   -- #EEC900

gold3 :: Color
gold3 = Color 205 173 0   -- #CDAD00

gold4 :: Color
gold4 = Color 139 117 0   -- #8B7500

goldenrod :: Color
goldenrod = Color 218 165 32   -- #DAA520

goldenrod1 :: Color
goldenrod1 = Color 255 193 37   -- #FFC125

goldenrod2 :: Color
goldenrod2 = Color 238 180 34   -- #EEB422

goldenrod3 :: Color
goldenrod3 = Color 205 155 29   -- #CD9B1D

goldenrod4 :: Color
goldenrod4 = Color 139 105 20   -- #8B6914

gray :: Color
gray = Color 190 190 190   -- #BEBEBE

gray0 :: Color
gray0 = Color 0 0 0   -- #000000

gray1 :: Color
gray1 = Color 3 3 3   -- #030303

gray2 :: Color
gray2 = Color 5 5 5   -- #050505

gray3 :: Color
gray3 = Color 8 8 8   -- #080808

gray4 :: Color
gray4 = Color 10 10 10   -- #0A0A0A

gray5 :: Color
gray5 = Color 13 13 13   -- #0D0D0D

gray6 :: Color
gray6 = Color 15 15 15   -- #0F0F0F

gray7 :: Color
gray7 = Color 18 18 18   -- #121212

gray8 :: Color
gray8 = Color 20 20 20   -- #141414

gray9 :: Color
gray9 = Color 23 23 23   -- #171717

gray10 :: Color
gray10 = Color 26 26 26   -- #1A1A1A

gray11 :: Color
gray11 = Color 28 28 28   -- #1C1C1C

gray12 :: Color
gray12 = Color 31 31 31   -- #1F1F1F

gray13 :: Color
gray13 = Color 33 33 33   -- #212121

gray14 :: Color
gray14 = Color 36 36 36   -- #242424

gray15 :: Color
gray15 = Color 38 38 38   -- #262626

gray16 :: Color
gray16 = Color 41 41 41   -- #292929

gray17 :: Color
gray17 = Color 43 43 43   -- #2B2B2B

gray18 :: Color
gray18 = Color 46 46 46   -- #2E2E2E

gray19 :: Color
gray19 = Color 48 48 48   -- #303030

gray20 :: Color
gray20 = Color 51 51 51   -- #333333

gray21 :: Color
gray21 = Color 54 54 54   -- #363636

gray22 :: Color
gray22 = Color 56 56 56   -- #383838

gray23 :: Color
gray23 = Color 59 59 59   -- #3B3B3B

gray24 :: Color
gray24 = Color 61 61 61   -- #3D3D3D

gray25 :: Color
gray25 = Color 64 64 64   -- #404040

gray26 :: Color
gray26 = Color 66 66 66   -- #424242

gray27 :: Color
gray27 = Color 69 69 69   -- #454545

gray28 :: Color
gray28 = Color 71 71 71   -- #474747

gray29 :: Color
gray29 = Color 74 74 74   -- #4A4A4A

gray30 :: Color
gray30 = Color 77 77 77   -- #4D4D4D

gray31 :: Color
gray31 = Color 79 79 79   -- #4F4F4F

gray32 :: Color
gray32 = Color 82 82 82   -- #525252

gray33 :: Color
gray33 = Color 84 84 84   -- #545454

gray34 :: Color
gray34 = Color 87 87 87   -- #575757

gray35 :: Color
gray35 = Color 89 89 89   -- #595959

gray36 :: Color
gray36 = Color 92 92 92   -- #5C5C5C

gray37 :: Color
gray37 = Color 94 94 94   -- #5E5E5E

gray38 :: Color
gray38 = Color 97 97 97   -- #616161

gray39 :: Color
gray39 = Color 99 99 99   -- #636363

gray40 :: Color
gray40 = Color 102 102 102   -- #666666

gray41 :: Color
gray41 = Color 105 105 105   -- #696969

gray42 :: Color
gray42 = Color 107 107 107   -- #6B6B6B

gray43 :: Color
gray43 = Color 110 110 110   -- #6E6E6E

gray44 :: Color
gray44 = Color 112 112 112   -- #707070

gray45 :: Color
gray45 = Color 115 115 115   -- #737373

gray46 :: Color
gray46 = Color 117 117 117   -- #757575

gray47 :: Color
gray47 = Color 120 120 120   -- #787878

gray48 :: Color
gray48 = Color 122 122 122   -- #7A7A7A

gray49 :: Color
gray49 = Color 125 125 125   -- #7D7D7D

gray50 :: Color
gray50 = Color 127 127 127   -- #7F7F7F

gray51 :: Color
gray51 = Color 130 130 130   -- #828282

gray52 :: Color
gray52 = Color 133 133 133   -- #858585

gray53 :: Color
gray53 = Color 135 135 135   -- #878787

gray54 :: Color
gray54 = Color 138 138 138   -- #8A8A8A

gray55 :: Color
gray55 = Color 140 140 140   -- #8C8C8C

gray56 :: Color
gray56 = Color 143 143 143   -- #8F8F8F

gray57 :: Color
gray57 = Color 145 145 145   -- #919191

gray58 :: Color
gray58 = Color 148 148 148   -- #949494

gray59 :: Color
gray59 = Color 150 150 150   -- #969696

gray60 :: Color
gray60 = Color 153 153 153   -- #999999

gray61 :: Color
gray61 = Color 156 156 156   -- #9C9C9C

gray62 :: Color
gray62 = Color 158 158 158   -- #9E9E9E

gray63 :: Color
gray63 = Color 161 161 161   -- #A1A1A1

gray64 :: Color
gray64 = Color 163 163 163   -- #A3A3A3

gray65 :: Color
gray65 = Color 166 166 166   -- #A6A6A6

gray66 :: Color
gray66 = Color 168 168 168   -- #A8A8A8

gray67 :: Color
gray67 = Color 171 171 171   -- #ABABAB

gray68 :: Color
gray68 = Color 173 173 173   -- #ADADAD

gray69 :: Color
gray69 = Color 176 176 176   -- #B0B0B0

gray70 :: Color
gray70 = Color 179 179 179   -- #B3B3B3

gray71 :: Color
gray71 = Color 181 181 181   -- #B5B5B5

gray72 :: Color
gray72 = Color 184 184 184   -- #B8B8B8

gray73 :: Color
gray73 = Color 186 186 186   -- #BABABA

gray74 :: Color
gray74 = Color 189 189 189   -- #BDBDBD

gray75 :: Color
gray75 = Color 191 191 191   -- #BFBFBF

gray76 :: Color
gray76 = Color 194 194 194   -- #C2C2C2

gray77 :: Color
gray77 = Color 196 196 196   -- #C4C4C4

gray78 :: Color
gray78 = Color 199 199 199   -- #C7C7C7

gray79 :: Color
gray79 = Color 201 201 201   -- #C9C9C9

gray80 :: Color
gray80 = Color 204 204 204   -- #CCCCCC

gray81 :: Color
gray81 = Color 207 207 207   -- #CFCFCF

gray82 :: Color
gray82 = Color 209 209 209   -- #D1D1D1

gray83 :: Color
gray83 = Color 212 212 212   -- #D4D4D4

gray84 :: Color
gray84 = Color 214 214 214   -- #D6D6D6

gray85 :: Color
gray85 = Color 217 217 217   -- #D9D9D9

gray86 :: Color
gray86 = Color 219 219 219   -- #DBDBDB

gray87 :: Color
gray87 = Color 222 222 222   -- #DEDEDE

gray88 :: Color
gray88 = Color 224 224 224   -- #E0E0E0

gray89 :: Color
gray89 = Color 227 227 227   -- #E3E3E3

gray90 :: Color
gray90 = Color 229 229 229   -- #E5E5E5

gray91 :: Color
gray91 = Color 232 232 232   -- #E8E8E8

gray92 :: Color
gray92 = Color 235 235 235   -- #EBEBEB

gray93 :: Color
gray93 = Color 237 237 237   -- #EDEDED

gray94 :: Color
gray94 = Color 240 240 240   -- #F0F0F0

gray95 :: Color
gray95 = Color 242 242 242   -- #F2F2F2

gray96 :: Color
gray96 = Color 245 245 245   -- #F5F5F5

gray97 :: Color
gray97 = Color 247 247 247   -- #F7F7F7

gray98 :: Color
gray98 = Color 250 250 250   -- #FAFAFA

gray99 :: Color
gray99 = Color 252 252 252   -- #FCFCFC

gray100 :: Color
gray100 = Color 255 255 255   -- #FFFFFF

green :: Color
green = Color 0 255 0   -- #00FF00

green1 :: Color
green1 = Color 0 255 0   -- #00FF00

green2 :: Color
green2 = Color 0 238 0   -- #00EE00

green3 :: Color
green3 = Color 0 205 0   -- #00CD00

green4 :: Color
green4 = Color 0 139 0   -- #008B00

greenyellow :: Color
greenyellow = Color 173 255 47   -- #ADFF2F

grey :: Color
grey = Color 190 190 190   -- #BEBEBE

grey0 :: Color
grey0 = Color 0 0 0   -- #000000

grey1 :: Color
grey1 = Color 3 3 3   -- #030303

grey2 :: Color
grey2 = Color 5 5 5   -- #050505

grey3 :: Color
grey3 = Color 8 8 8   -- #080808

grey4 :: Color
grey4 = Color 10 10 10   -- #0A0A0A

grey5 :: Color
grey5 = Color 13 13 13   -- #0D0D0D

grey6 :: Color
grey6 = Color 15 15 15   -- #0F0F0F

grey7 :: Color
grey7 = Color 18 18 18   -- #121212

grey8 :: Color
grey8 = Color 20 20 20   -- #141414

grey9 :: Color
grey9 = Color 23 23 23   -- #171717

grey10 :: Color
grey10 = Color 26 26 26   -- #1A1A1A

grey11 :: Color
grey11 = Color 28 28 28   -- #1C1C1C

grey12 :: Color
grey12 = Color 31 31 31   -- #1F1F1F

grey13 :: Color
grey13 = Color 33 33 33   -- #212121

grey14 :: Color
grey14 = Color 36 36 36   -- #242424

grey15 :: Color
grey15 = Color 38 38 38   -- #262626

grey16 :: Color
grey16 = Color 41 41 41   -- #292929

grey17 :: Color
grey17 = Color 43 43 43   -- #2B2B2B

grey18 :: Color
grey18 = Color 46 46 46   -- #2E2E2E

grey19 :: Color
grey19 = Color 48 48 48   -- #303030

grey20 :: Color
grey20 = Color 51 51 51   -- #333333

grey21 :: Color
grey21 = Color 54 54 54   -- #363636

grey22 :: Color
grey22 = Color 56 56 56   -- #383838

grey23 :: Color
grey23 = Color 59 59 59   -- #3B3B3B

grey24 :: Color
grey24 = Color 61 61 61   -- #3D3D3D

grey25 :: Color
grey25 = Color 64 64 64   -- #404040

grey26 :: Color
grey26 = Color 66 66 66   -- #424242

grey27 :: Color
grey27 = Color 69 69 69   -- #454545

grey28 :: Color
grey28 = Color 71 71 71   -- #474747

grey29 :: Color
grey29 = Color 74 74 74   -- #4A4A4A

grey30 :: Color
grey30 = Color 77 77 77   -- #4D4D4D

grey31 :: Color
grey31 = Color 79 79 79   -- #4F4F4F

grey32 :: Color
grey32 = Color 82 82 82   -- #525252

grey33 :: Color
grey33 = Color 84 84 84   -- #545454

grey34 :: Color
grey34 = Color 87 87 87   -- #575757

grey35 :: Color
grey35 = Color 89 89 89   -- #595959

grey36 :: Color
grey36 = Color 92 92 92   -- #5C5C5C

grey37 :: Color
grey37 = Color 94 94 94   -- #5E5E5E

grey38 :: Color
grey38 = Color 97 97 97   -- #616161

grey39 :: Color
grey39 = Color 99 99 99   -- #636363

grey40 :: Color
grey40 = Color 102 102 102   -- #666666

grey41 :: Color
grey41 = Color 105 105 105   -- #696969

grey42 :: Color
grey42 = Color 107 107 107   -- #6B6B6B

grey43 :: Color
grey43 = Color 110 110 110   -- #6E6E6E

grey44 :: Color
grey44 = Color 112 112 112   -- #707070

grey45 :: Color
grey45 = Color 115 115 115   -- #737373

grey46 :: Color
grey46 = Color 117 117 117   -- #757575

grey47 :: Color
grey47 = Color 120 120 120   -- #787878

grey48 :: Color
grey48 = Color 122 122 122   -- #7A7A7A

grey49 :: Color
grey49 = Color 125 125 125   -- #7D7D7D

grey50 :: Color
grey50 = Color 127 127 127   -- #7F7F7F

grey51 :: Color
grey51 = Color 130 130 130   -- #828282

grey52 :: Color
grey52 = Color 133 133 133   -- #858585

grey53 :: Color
grey53 = Color 135 135 135   -- #878787

grey54 :: Color
grey54 = Color 138 138 138   -- #8A8A8A

grey55 :: Color
grey55 = Color 140 140 140   -- #8C8C8C

grey56 :: Color
grey56 = Color 143 143 143   -- #8F8F8F

grey57 :: Color
grey57 = Color 145 145 145   -- #919191

grey58 :: Color
grey58 = Color 148 148 148   -- #949494

grey59 :: Color
grey59 = Color 150 150 150   -- #969696

grey60 :: Color
grey60 = Color 153 153 153   -- #999999

grey61 :: Color
grey61 = Color 156 156 156   -- #9C9C9C

grey62 :: Color
grey62 = Color 158 158 158   -- #9E9E9E

grey63 :: Color
grey63 = Color 161 161 161   -- #A1A1A1

grey64 :: Color
grey64 = Color 163 163 163   -- #A3A3A3

grey65 :: Color
grey65 = Color 166 166 166   -- #A6A6A6

grey66 :: Color
grey66 = Color 168 168 168   -- #A8A8A8

grey67 :: Color
grey67 = Color 171 171 171   -- #ABABAB

grey68 :: Color
grey68 = Color 173 173 173   -- #ADADAD

grey69 :: Color
grey69 = Color 176 176 176   -- #B0B0B0

grey70 :: Color
grey70 = Color 179 179 179   -- #B3B3B3

grey71 :: Color
grey71 = Color 181 181 181   -- #B5B5B5

grey72 :: Color
grey72 = Color 184 184 184   -- #B8B8B8

grey73 :: Color
grey73 = Color 186 186 186   -- #BABABA

grey74 :: Color
grey74 = Color 189 189 189   -- #BDBDBD

grey75 :: Color
grey75 = Color 191 191 191   -- #BFBFBF

grey76 :: Color
grey76 = Color 194 194 194   -- #C2C2C2

grey77 :: Color
grey77 = Color 196 196 196   -- #C4C4C4

grey78 :: Color
grey78 = Color 199 199 199   -- #C7C7C7

grey79 :: Color
grey79 = Color 201 201 201   -- #C9C9C9

grey80 :: Color
grey80 = Color 204 204 204   -- #CCCCCC

grey81 :: Color
grey81 = Color 207 207 207   -- #CFCFCF

grey82 :: Color
grey82 = Color 209 209 209   -- #D1D1D1

grey83 :: Color
grey83 = Color 212 212 212   -- #D4D4D4

grey84 :: Color
grey84 = Color 214 214 214   -- #D6D6D6

grey85 :: Color
grey85 = Color 217 217 217   -- #D9D9D9

grey86 :: Color
grey86 = Color 219 219 219   -- #DBDBDB

grey87 :: Color
grey87 = Color 222 222 222   -- #DEDEDE

grey88 :: Color
grey88 = Color 224 224 224   -- #E0E0E0

grey89 :: Color
grey89 = Color 227 227 227   -- #E3E3E3

grey90 :: Color
grey90 = Color 229 229 229   -- #E5E5E5

grey91 :: Color
grey91 = Color 232 232 232   -- #E8E8E8

grey92 :: Color
grey92 = Color 235 235 235   -- #EBEBEB

grey93 :: Color
grey93 = Color 237 237 237   -- #EDEDED

grey94 :: Color
grey94 = Color 240 240 240   -- #F0F0F0

grey95 :: Color
grey95 = Color 242 242 242   -- #F2F2F2

grey96 :: Color
grey96 = Color 245 245 245   -- #F5F5F5

grey97 :: Color
grey97 = Color 247 247 247   -- #F7F7F7

grey98 :: Color
grey98 = Color 250 250 250   -- #FAFAFA

grey99 :: Color
grey99 = Color 252 252 252   -- #FCFCFC

grey100 :: Color
grey100 = Color 255 255 255   -- #FFFFFF

honeydew :: Color
honeydew = Color 240 255 240   -- #F0FFF0

honeydew1 :: Color
honeydew1 = Color 240 255 240   -- #F0FFF0

honeydew2 :: Color
honeydew2 = Color 224 238 224   -- #E0EEE0

honeydew3 :: Color
honeydew3 = Color 193 205 193   -- #C1CDC1

honeydew4 :: Color
honeydew4 = Color 131 139 131   -- #838B83

hotpink :: Color
hotpink = Color 255 105 180   -- #FF69B4

hotpink1 :: Color
hotpink1 = Color 255 110 180   -- #FF6EB4

hotpink2 :: Color
hotpink2 = Color 238 106 167   -- #EE6AA7

hotpink3 :: Color
hotpink3 = Color 205 96 144   -- #CD6090

hotpink4 :: Color
hotpink4 = Color 139 58 98   -- #8B3A62

indianred :: Color
indianred = Color 205 92 92   -- #CD5C5C

indianred1 :: Color
indianred1 = Color 255 106 106   -- #FF6A6A

indianred2 :: Color
indianred2 = Color 238 99 99   -- #EE6363

indianred3 :: Color
indianred3 = Color 205 85 85   -- #CD5555

indianred4 :: Color
indianred4 = Color 139 58 58   -- #8B3A3A

ivory :: Color
ivory = Color 255 255 240   -- #FFFFF0

ivory1 :: Color
ivory1 = Color 255 255 240   -- #FFFFF0

ivory2 :: Color
ivory2 = Color 238 238 224   -- #EEEEE0

ivory3 :: Color
ivory3 = Color 205 205 193   -- #CDCDC1

ivory4 :: Color
ivory4 = Color 139 139 131   -- #8B8B83

khaki :: Color
khaki = Color 240 230 140   -- #F0E68C

khaki1 :: Color
khaki1 = Color 255 246 143   -- #FFF68F

khaki2 :: Color
khaki2 = Color 238 230 133   -- #EEE685

khaki3 :: Color
khaki3 = Color 205 198 115   -- #CDC673

khaki4 :: Color
khaki4 = Color 139 134 78   -- #8B864E

lavender :: Color
lavender = Color 230 230 250   -- #E6E6FA

lavenderblush :: Color
lavenderblush = Color 255 240 245   -- #FFF0F5

lavenderblush1 :: Color
lavenderblush1 = Color 255 240 245   -- #FFF0F5

lavenderblush2 :: Color
lavenderblush2 = Color 238 224 229   -- #EEE0E5

lavenderblush3 :: Color
lavenderblush3 = Color 205 193 197   -- #CDC1C5

lavenderblush4 :: Color
lavenderblush4 = Color 139 131 134   -- #8B8386

lawngreen :: Color
lawngreen = Color 124 252 0   -- #7CFC00

lemonchiffon :: Color
lemonchiffon = Color 255 250 205   -- #FFFACD

lemonchiffon1 :: Color
lemonchiffon1 = Color 255 250 205   -- #FFFACD

lemonchiffon2 :: Color
lemonchiffon2 = Color 238 233 191   -- #EEE9BF

lemonchiffon3 :: Color
lemonchiffon3 = Color 205 201 165   -- #CDC9A5

lemonchiffon4 :: Color
lemonchiffon4 = Color 139 137 112   -- #8B8970

lightblue :: Color
lightblue = Color 173 216 230   -- #ADD8E6

lightblue1 :: Color
lightblue1 = Color 191 239 255   -- #BFEFFF

lightblue2 :: Color
lightblue2 = Color 178 223 238   -- #B2DFEE

lightblue3 :: Color
lightblue3 = Color 154 192 205   -- #9AC0CD

lightblue4 :: Color
lightblue4 = Color 104 131 139   -- #68838B

lightcoral :: Color
lightcoral = Color 240 128 128   -- #F08080

lightcyan :: Color
lightcyan = Color 224 255 255   -- #E0FFFF

lightcyan1 :: Color
lightcyan1 = Color 224 255 255   -- #E0FFFF

lightcyan2 :: Color
lightcyan2 = Color 209 238 238   -- #D1EEEE

lightcyan3 :: Color
lightcyan3 = Color 180 205 205   -- #B4CDCD

lightcyan4 :: Color
lightcyan4 = Color 122 139 139   -- #7A8B8B

lightgoldenrod :: Color
lightgoldenrod = Color 238 221 130   -- #EEDD82

lightgoldenrod1 :: Color
lightgoldenrod1 = Color 255 236 139   -- #FFEC8B

lightgoldenrod2 :: Color
lightgoldenrod2 = Color 238 220 130   -- #EEDC82

lightgoldenrod3 :: Color
lightgoldenrod3 = Color 205 190 112   -- #CDBE70

lightgoldenrod4 :: Color
lightgoldenrod4 = Color 139 129 76   -- #8B814C

lightgoldenrodyellow :: Color
lightgoldenrodyellow = Color 250 250 210   -- #FAFAD2

lightgray :: Color
lightgray = Color 211 211 211   -- #D3D3D3

lightgreen :: Color
lightgreen = Color 144 238 144   -- #90EE90

lightgrey :: Color
lightgrey = Color 211 211 211   -- #D3D3D3

lightpink :: Color
lightpink = Color 255 182 193   -- #FFB6C1

lightpink1 :: Color
lightpink1 = Color 255 174 185   -- #FFAEB9

lightpink2 :: Color
lightpink2 = Color 238 162 173   -- #EEA2AD

lightpink3 :: Color
lightpink3 = Color 205 140 149   -- #CD8C95

lightpink4 :: Color
lightpink4 = Color 139 95 101   -- #8B5F65

lightsalmon :: Color
lightsalmon = Color 255 160 122   -- #FFA07A

lightsalmon1 :: Color
lightsalmon1 = Color 255 160 122   -- #FFA07A

lightsalmon2 :: Color
lightsalmon2 = Color 238 149 114   -- #EE9572

lightsalmon3 :: Color
lightsalmon3 = Color 205 129 98   -- #CD8162

lightsalmon4 :: Color
lightsalmon4 = Color 139 87 66   -- #8B5742

lightseagreen :: Color
lightseagreen = Color 32 178 170   -- #20B2AA

lightskyblue :: Color
lightskyblue = Color 135 206 250   -- #87CEFA

lightskyblue1 :: Color
lightskyblue1 = Color 176 226 255   -- #B0E2FF

lightskyblue2 :: Color
lightskyblue2 = Color 164 211 238   -- #A4D3EE

lightskyblue3 :: Color
lightskyblue3 = Color 141 182 205   -- #8DB6CD

lightskyblue4 :: Color
lightskyblue4 = Color 96 123 139   -- #607B8B

lightslateblue :: Color
lightslateblue = Color 132 112 255   -- #8470FF

lightslategray :: Color
lightslategray = Color 119 136 153   -- #778899

lightslategrey :: Color
lightslategrey = Color 119 136 153   -- #778899

lightsteelblue :: Color
lightsteelblue = Color 176 196 222   -- #B0C4DE

lightsteelblue1 :: Color
lightsteelblue1 = Color 202 225 255   -- #CAE1FF

lightsteelblue2 :: Color
lightsteelblue2 = Color 188 210 238   -- #BCD2EE

lightsteelblue3 :: Color
lightsteelblue3 = Color 162 181 205   -- #A2B5CD

lightsteelblue4 :: Color
lightsteelblue4 = Color 110 123 139   -- #6E7B8B

lightyellow :: Color
lightyellow = Color 255 255 224   -- #FFFFE0

lightyellow1 :: Color
lightyellow1 = Color 255 255 224   -- #FFFFE0

lightyellow2 :: Color
lightyellow2 = Color 238 238 209   -- #EEEED1

lightyellow3 :: Color
lightyellow3 = Color 205 205 180   -- #CDCDB4

lightyellow4 :: Color
lightyellow4 = Color 139 139 122   -- #8B8B7A

limegreen :: Color
limegreen = Color 50 205 50   -- #32CD32

linen :: Color
linen = Color 250 240 230   -- #FAF0E6

magenta :: Color
magenta = Color 255 0 255   -- #FF00FF

magenta1 :: Color
magenta1 = Color 255 0 255   -- #FF00FF

magenta2 :: Color
magenta2 = Color 238 0 238   -- #EE00EE

magenta3 :: Color
magenta3 = Color 205 0 205   -- #CD00CD

magenta4 :: Color
magenta4 = Color 139 0 139   -- #8B008B

maroon :: Color
maroon = Color 176 48 96   -- #B03060

maroon1 :: Color
maroon1 = Color 255 52 179   -- #FF34B3

maroon2 :: Color
maroon2 = Color 238 48 167   -- #EE30A7

maroon3 :: Color
maroon3 = Color 205 41 144   -- #CD2990

maroon4 :: Color
maroon4 = Color 139 28 98   -- #8B1C62

mediumaquamarine :: Color
mediumaquamarine = Color 102 205 170   -- #66CDAA

mediumblue :: Color
mediumblue = Color 0 0 205   -- #0000CD

mediumorchid :: Color
mediumorchid = Color 186 85 211   -- #BA55D3

mediumorchid1 :: Color
mediumorchid1 = Color 224 102 255   -- #E066FF

mediumorchid2 :: Color
mediumorchid2 = Color 209 95 238   -- #D15FEE

mediumorchid3 :: Color
mediumorchid3 = Color 180 82 205   -- #B452CD

mediumorchid4 :: Color
mediumorchid4 = Color 122 55 139   -- #7A378B

mediumpurple :: Color
mediumpurple = Color 147 112 219   -- #9370DB

mediumpurple1 :: Color
mediumpurple1 = Color 171 130 255   -- #AB82FF

mediumpurple2 :: Color
mediumpurple2 = Color 159 121 238   -- #9F79EE

mediumpurple3 :: Color
mediumpurple3 = Color 137 104 205   -- #8968CD

mediumpurple4 :: Color
mediumpurple4 = Color 93 71 139   -- #5D478B

mediumseagreen :: Color
mediumseagreen = Color 60 179 113   -- #3CB371

mediumslateblue :: Color
mediumslateblue = Color 123 104 238   -- #7B68EE

mediumspringgreen :: Color
mediumspringgreen = Color 0 250 154   -- #00FA9A

mediumturquoise :: Color
mediumturquoise = Color 72 209 204   -- #48D1CC

mediumvioletred :: Color
mediumvioletred = Color 199 21 133   -- #C71585

midnightblue :: Color
midnightblue = Color 25 25 112   -- #191970

mintcream :: Color
mintcream = Color 245 255 250   -- #F5FFFA

mistyrose :: Color
mistyrose = Color 255 228 225   -- #FFE4E1

mistyrose1 :: Color
mistyrose1 = Color 255 228 225   -- #FFE4E1

mistyrose2 :: Color
mistyrose2 = Color 238 213 210   -- #EED5D2

mistyrose3 :: Color
mistyrose3 = Color 205 183 181   -- #CDB7B5

mistyrose4 :: Color
mistyrose4 = Color 139 125 123   -- #8B7D7B

moccasin :: Color
moccasin = Color 255 228 181   -- #FFE4B5

navajowhite :: Color
navajowhite = Color 255 222 173   -- #FFDEAD

navajowhite1 :: Color
navajowhite1 = Color 255 222 173   -- #FFDEAD

navajowhite2 :: Color
navajowhite2 = Color 238 207 161   -- #EECFA1

navajowhite3 :: Color
navajowhite3 = Color 205 179 139   -- #CDB38B

navajowhite4 :: Color
navajowhite4 = Color 139 121 94   -- #8B795E

navy :: Color
navy = Color 0 0 128   -- #000080

navyblue :: Color
navyblue = Color 0 0 128   -- #000080

oldlace :: Color
oldlace = Color 253 245 230   -- #FDF5E6

olivedrab :: Color
olivedrab = Color 107 142 35   -- #6B8E23

olivedrab1 :: Color
olivedrab1 = Color 192 255 62   -- #C0FF3E

olivedrab2 :: Color
olivedrab2 = Color 179 238 58   -- #B3EE3A

olivedrab3 :: Color
olivedrab3 = Color 154 205 50   -- #9ACD32

olivedrab4 :: Color
olivedrab4 = Color 105 139 34   -- #698B22

orange :: Color
orange = Color 255 165 0   -- #FFA500

orange1 :: Color
orange1 = Color 255 165 0   -- #FFA500

orange2 :: Color
orange2 = Color 238 154 0   -- #EE9A00

orange3 :: Color
orange3 = Color 205 133 0   -- #CD8500

orange4 :: Color
orange4 = Color 139 90 0   -- #8B5A00

orangered :: Color
orangered = Color 255 69 0   -- #FF4500

orangered1 :: Color
orangered1 = Color 255 69 0   -- #FF4500

orangered2 :: Color
orangered2 = Color 238 64 0   -- #EE4000

orangered3 :: Color
orangered3 = Color 205 55 0   -- #CD3700

orangered4 :: Color
orangered4 = Color 139 37 0   -- #8B2500

orchid :: Color
orchid = Color 218 112 214   -- #DA70D6

orchid1 :: Color
orchid1 = Color 255 131 250   -- #FF83FA

orchid2 :: Color
orchid2 = Color 238 122 233   -- #EE7AE9

orchid3 :: Color
orchid3 = Color 205 105 201   -- #CD69C9

orchid4 :: Color
orchid4 = Color 139 71 137   -- #8B4789

palegoldenrod :: Color
palegoldenrod = Color 238 232 170   -- #EEE8AA

palegreen :: Color
palegreen = Color 152 251 152   -- #98FB98

palegreen1 :: Color
palegreen1 = Color 154 255 154   -- #9AFF9A

palegreen2 :: Color
palegreen2 = Color 144 238 144   -- #90EE90

palegreen3 :: Color
palegreen3 = Color 124 205 124   -- #7CCD7C

palegreen4 :: Color
palegreen4 = Color 84 139 84   -- #548B54

paleturquoise :: Color
paleturquoise = Color 175 238 238   -- #AFEEEE

paleturquoise1 :: Color
paleturquoise1 = Color 187 255 255   -- #BBFFFF

paleturquoise2 :: Color
paleturquoise2 = Color 174 238 238   -- #AEEEEE

paleturquoise3 :: Color
paleturquoise3 = Color 150 205 205   -- #96CDCD

paleturquoise4 :: Color
paleturquoise4 = Color 102 139 139   -- #668B8B

palevioletred :: Color
palevioletred = Color 219 112 147   -- #DB7093

palevioletred1 :: Color
palevioletred1 = Color 255 130 171   -- #FF82AB

palevioletred2 :: Color
palevioletred2 = Color 238 121 159   -- #EE799F

palevioletred3 :: Color
palevioletred3 = Color 205 104 137   -- #CD6889

palevioletred4 :: Color
palevioletred4 = Color 139 71 93   -- #8B475D

papayawhip :: Color
papayawhip = Color 255 239 213   -- #FFEFD5

peachpuff :: Color
peachpuff = Color 255 218 185   -- #FFDAB9

peachpuff1 :: Color
peachpuff1 = Color 255 218 185   -- #FFDAB9

peachpuff2 :: Color
peachpuff2 = Color 238 203 173   -- #EECBAD

peachpuff3 :: Color
peachpuff3 = Color 205 175 149   -- #CDAF95

peachpuff4 :: Color
peachpuff4 = Color 139 119 101   -- #8B7765

peru :: Color
peru = Color 205 133 63   -- #CD853F

pink :: Color
pink = Color 255 192 203   -- #FFC0CB

pink1 :: Color
pink1 = Color 255 181 197   -- #FFB5C5

pink2 :: Color
pink2 = Color 238 169 184   -- #EEA9B8

pink3 :: Color
pink3 = Color 205 145 158   -- #CD919E

pink4 :: Color
pink4 = Color 139 99 108   -- #8B636C

plum :: Color
plum = Color 221 160 221   -- #DDA0DD

plum1 :: Color
plum1 = Color 255 187 255   -- #FFBBFF

plum2 :: Color
plum2 = Color 238 174 238   -- #EEAEEE

plum3 :: Color
plum3 = Color 205 150 205   -- #CD96CD

plum4 :: Color
plum4 = Color 139 102 139   -- #8B668B

powderblue :: Color
powderblue = Color 176 224 230   -- #B0E0E6

purple :: Color
purple = Color 160 32 240   -- #A020F0

purple1 :: Color
purple1 = Color 155 48 255   -- #9B30FF

purple2 :: Color
purple2 = Color 145 44 238   -- #912CEE

purple3 :: Color
purple3 = Color 125 38 205   -- #7D26CD

purple4 :: Color
purple4 = Color 85 26 139   -- #551A8B

red :: Color
red = Color 255 0 0   -- #FF0000

red1 :: Color
red1 = Color 255 0 0   -- #FF0000

red2 :: Color
red2 = Color 238 0 0   -- #EE0000

red3 :: Color
red3 = Color 205 0 0   -- #CD0000

red4 :: Color
red4 = Color 139 0 0   -- #8B0000

rosybrown :: Color
rosybrown = Color 188 143 143   -- #BC8F8F

rosybrown1 :: Color
rosybrown1 = Color 255 193 193   -- #FFC1C1

rosybrown2 :: Color
rosybrown2 = Color 238 180 180   -- #EEB4B4

rosybrown3 :: Color
rosybrown3 = Color 205 155 155   -- #CD9B9B

rosybrown4 :: Color
rosybrown4 = Color 139 105 105   -- #8B6969

royalblue :: Color
royalblue = Color 65 105 225   -- #4169E1

royalblue1 :: Color
royalblue1 = Color 72 118 255   -- #4876FF

royalblue2 :: Color
royalblue2 = Color 67 110 238   -- #436EEE

royalblue3 :: Color
royalblue3 = Color 58 95 205   -- #3A5FCD

royalblue4 :: Color
royalblue4 = Color 39 64 139   -- #27408B

saddlebrown :: Color
saddlebrown = Color 139 69 19   -- #8B4513

salmon :: Color
salmon = Color 250 128 114   -- #FA8072

salmon1 :: Color
salmon1 = Color 255 140 105   -- #FF8C69

salmon2 :: Color
salmon2 = Color 238 130 98   -- #EE8262

salmon3 :: Color
salmon3 = Color 205 112 84   -- #CD7054

salmon4 :: Color
salmon4 = Color 139 76 57   -- #8B4C39

sandybrown :: Color
sandybrown = Color 244 164 96   -- #F4A460

seagreen :: Color
seagreen = Color 46 139 87   -- #2E8B57

seagreen1 :: Color
seagreen1 = Color 84 255 159   -- #54FF9F

seagreen2 :: Color
seagreen2 = Color 78 238 148   -- #4EEE94

seagreen3 :: Color
seagreen3 = Color 67 205 128   -- #43CD80

seagreen4 :: Color
seagreen4 = Color 46 139 87   -- #2E8B57

seashell :: Color
seashell = Color 255 245 238   -- #FFF5EE

seashell1 :: Color
seashell1 = Color 255 245 238   -- #FFF5EE

seashell2 :: Color
seashell2 = Color 238 229 222   -- #EEE5DE

seashell3 :: Color
seashell3 = Color 205 197 191   -- #CDC5BF

seashell4 :: Color
seashell4 = Color 139 134 130   -- #8B8682

sienna :: Color
sienna = Color 160 82 45   -- #A0522D

sienna1 :: Color
sienna1 = Color 255 130 71   -- #FF8247

sienna2 :: Color
sienna2 = Color 238 121 66   -- #EE7942

sienna3 :: Color
sienna3 = Color 205 104 57   -- #CD6839

sienna4 :: Color
sienna4 = Color 139 71 38   -- #8B4726

skyblue :: Color
skyblue = Color 135 206 235   -- #87CEEB

skyblue1 :: Color
skyblue1 = Color 135 206 255   -- #87CEFF

skyblue2 :: Color
skyblue2 = Color 126 192 238   -- #7EC0EE

skyblue3 :: Color
skyblue3 = Color 108 166 205   -- #6CA6CD

skyblue4 :: Color
skyblue4 = Color 74 112 139   -- #4A708B

slateblue :: Color
slateblue = Color 106 90 205   -- #6A5ACD

slateblue1 :: Color
slateblue1 = Color 131 111 255   -- #836FFF

slateblue2 :: Color
slateblue2 = Color 122 103 238   -- #7A67EE

slateblue3 :: Color
slateblue3 = Color 105 89 205   -- #6959CD

slateblue4 :: Color
slateblue4 = Color 71 60 139   -- #473C8B

slategray :: Color
slategray = Color 112 128 144   -- #708090

slategray1 :: Color
slategray1 = Color 198 226 255   -- #C6E2FF

slategray2 :: Color
slategray2 = Color 185 211 238   -- #B9D3EE

slategray3 :: Color
slategray3 = Color 159 182 205   -- #9FB6CD

slategray4 :: Color
slategray4 = Color 108 123 139   -- #6C7B8B

slategrey :: Color
slategrey = Color 112 128 144   -- #708090

snow :: Color
snow = Color 255 250 250   -- #FFFAFA

snow1 :: Color
snow1 = Color 255 250 250   -- #FFFAFA

snow2 :: Color
snow2 = Color 238 233 233   -- #EEE9E9

snow3 :: Color
snow3 = Color 205 201 201   -- #CDC9C9

snow4 :: Color
snow4 = Color 139 137 137   -- #8B8989

springgreen :: Color
springgreen = Color 0 255 127   -- #00FF7F

springgreen1 :: Color
springgreen1 = Color 0 255 127   -- #00FF7F

springgreen2 :: Color
springgreen2 = Color 0 238 118   -- #00EE76

springgreen3 :: Color
springgreen3 = Color 0 205 102   -- #00CD66

springgreen4 :: Color
springgreen4 = Color 0 139 69   -- #008B45

steelblue :: Color
steelblue = Color 70 130 180   -- #4682B4

steelblue1 :: Color
steelblue1 = Color 99 184 255   -- #63B8FF

steelblue2 :: Color
steelblue2 = Color 92 172 238   -- #5CACEE

steelblue3 :: Color
steelblue3 = Color 79 148 205   -- #4F94CD

steelblue4 :: Color
steelblue4 = Color 54 100 139   -- #36648B

tan :: Color
tan = Color 210 180 140   -- #D2B48C

tan1 :: Color
tan1 = Color 255 165 79   -- #FFA54F

tan2 :: Color
tan2 = Color 238 154 73   -- #EE9A49

tan3 :: Color
tan3 = Color 205 133 63   -- #CD853F

tan4 :: Color
tan4 = Color 139 90 43   -- #8B5A2B

thistle :: Color
thistle = Color 216 191 216   -- #D8BFD8

thistle1 :: Color
thistle1 = Color 255 225 255   -- #FFE1FF

thistle2 :: Color
thistle2 = Color 238 210 238   -- #EED2EE

thistle3 :: Color
thistle3 = Color 205 181 205   -- #CDB5CD

thistle4 :: Color
thistle4 = Color 139 123 139   -- #8B7B8B

tomato :: Color
tomato = Color 255 99 71   -- #FF6347

tomato1 :: Color
tomato1 = Color 255 99 71   -- #FF6347

tomato2 :: Color
tomato2 = Color 238 92 66   -- #EE5C42

tomato3 :: Color
tomato3 = Color 205 79 57   -- #CD4F39

tomato4 :: Color
tomato4 = Color 139 54 38   -- #8B3626

turquoise :: Color
turquoise = Color 64 224 208   -- #40E0D0

turquoise1 :: Color
turquoise1 = Color 0 245 255   -- #00F5FF

turquoise2 :: Color
turquoise2 = Color 0 229 238   -- #00E5EE

turquoise3 :: Color
turquoise3 = Color 0 197 205   -- #00C5CD

turquoise4 :: Color
turquoise4 = Color 0 134 139   -- #00868B

violet :: Color
violet = Color 238 130 238   -- #EE82EE

violetred :: Color
violetred = Color 208 32 144   -- #D02090

violetred1 :: Color
violetred1 = Color 255 62 150   -- #FF3E96

violetred2 :: Color
violetred2 = Color 238 58 140   -- #EE3A8C

violetred3 :: Color
violetred3 = Color 205 50 120   -- #CD3278

violetred4 :: Color
violetred4 = Color 139 34 82   -- #8B2252

wheat :: Color
wheat = Color 245 222 179   -- #F5DEB3

wheat1 :: Color
wheat1 = Color 255 231 186   -- #FFE7BA

wheat2 :: Color
wheat2 = Color 238 216 174   -- #EED8AE

wheat3 :: Color
wheat3 = Color 205 186 150   -- #CDBA96

wheat4 :: Color
wheat4 = Color 139 126 102   -- #8B7E66

whitesmoke :: Color
whitesmoke = Color 245 245 245   -- #F5F5F5

yellow :: Color
yellow = Color 255 255 0   -- #FFFF00

yellow1 :: Color
yellow1 = Color 255 255 0   -- #FFFF00

yellow2 :: Color
yellow2 = Color 238 238 0   -- #EEEE00

yellow3 :: Color
yellow3 = Color 205 205 0   -- #CDCD00

yellow4 :: Color
yellow4 = Color 139 139 0   -- #8B8B00

yellowgreen :: Color
yellowgreen = Color 154 205 50   -- #9ACD32
