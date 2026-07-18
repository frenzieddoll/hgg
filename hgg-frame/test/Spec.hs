-- | hgg-frame テスト。 A1 = class PlotData のゼロ依存 instance 検証。
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- 注: renderBound (df |>> spec → SVG) の smoke は hgg-svg の test に置く。
-- frame:test が svg に依存すると cabal が frame↔svg をパッケージ循環と見なすため。
import           Graphics.Hgg.Frame
import           Graphics.Hgg.Spec     (ColData (..), layer, scatter)
import           Graphics.Hgg.Validate (PlotDiagnostic (..), PlotErrorKind (..),
                                        Severity (..), diagnosticSeverity)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Vector           as V
import           Test.Hspec

-- 長さ違いの列を含むサンプル (g が最長 = 4 行)
assocDF :: [(Text, ColData)]
assocDF =
  [ ("x", NumData (V.fromList [1, 2, 3]))
  , ("g", TxtData (V.fromList ["a", "b", "c", "d"]))
  ]

mapDF :: Map Text ColData
mapDF = M.fromList assocDF

-- scatter 用の x/y を持つ df (bind/render テスト用)。 列順違いで同一データ
xyAssoc :: [(Text, ColData)]
xyAssoc =
  [ ("x", NumData (V.fromList [1, 2, 3]))
  , ("y", NumData (V.fromList [4, 5, 6]))
  ]

xyMap :: Map Text ColData
xyMap = M.fromList xyAssoc

main :: IO ()
main = hspec $ do
  describe "PlotData [(Text, ColData)]" $ do
    it "columnNames は宣言順" $
      columnNames assocDF `shouldBe` ["x", "g"]
    it "nrows は最長列の長さ" $
      nrows assocDF `shouldBe` 4
    it "toResolver は列を引ける" $
      toResolver assocDF "x" `shouldBe` Just (NumData (V.fromList [1, 2, 3]))
    it "toResolver は不在列で Nothing" $
      toResolver assocDF "zzz" `shouldBe` Nothing

  describe "PlotData (Map Text ColData)" $ do
    it "columnNames は Map のキー (昇順)" $
      columnNames mapDF `shouldBe` ["g", "x"]
    it "nrows は最長列の長さ" $
      nrows mapDF `shouldBe` 4
    it "空 df の nrows は 0" $
      nrows (M.empty :: Map Text ColData) `shouldBe` 0

  describe "(|>>) バインド" $ do
    it "BoundPlot に resolver/spec を載せ、 診断は空 (A3 時点)" $ do
      let bp = xyMap |>> layer (scatter "x" "y")
      bpDiagnostics bp `shouldBe` []
      fmap colLen (bpResolver bp "x") `shouldBe` Just 3
    it "純値: unBound で (resolver, spec) を取り出せる" $ do
      let (r, _) = unBound (xyAssoc |>> layer (scatter "x" "y"))
      fmap colLen (r "y") `shouldBe` Just 3
    it "全列存在・数値なら診断ゼロ" $
      bpDiagnostics (xyMap |>> layer (scatter "x" "y")) `shouldBe` []

  describe "(|>>) 検証 (A5、 案1: 純値・例外なし)" $ do
    it "存在しない列 → ColumnNotFound Error (例外は投げない)" $ do
      let diags = bpDiagnostics (xyAssoc |>> layer (scatter "x" "nope"))
          isNopeNotFound (PlotError (ColumnNotFound n _) _) = n == "nope"
          isNopeNotFound _                                  = False
      any isNopeNotFound diags `shouldBe` True
      any ((== SevError) . diagnosticSeverity) diags `shouldBe` True
    it "空 df → PlotInfo (lenient、 Error ではない)" $ do
      let emptyAssoc = [] :: [(Text, ColData)]
          diags = bpDiagnostics (emptyAssoc |>> layer (scatter "x" "y"))
          isInfo (PlotInfo _) = True
          isInfo _            = False
      any isInfo diags `shouldBe` True
