-- |
-- Module      : Hgg.Plot.Validate
-- Description : Layer 3.5 ─ compile / validate / 診断 (Phase 11 A1 core hardening)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- 設計方針:
--
--   * 「'VisualSpec' は直接描画しない」 を型で固定する。 backend に渡す前に
--     'compilePlot' を通し、 必須 aesthetic 欠落 / 列解決失敗 / 型不一致 を検出。
--   * 診断は **actionable** であること (= 「Missing y」 ではなく
--     「scatter は x と y が必要。 y 列が未指定。 `y "yield"` を足してください」)。
--   * 列名解決失敗には **編集距離 suggestion** を添える ('validatePlotWith' に
--     既知列名を渡したとき)。
--   * 'BackendCapability' で backend 非対応機能を compile 時に検出する。
--
-- 本 module は render を一切呼ばない (= 出力中立)。 既存 backend は当面そのまま
-- 動き、 段階的に 'compilePlot' 経由へ寄せる。
--
-- 既知の制約: 「1 layer に mark 2 個 (`scatter x y <> line x y`) を合成して 2 個目が
-- 黙って消える」 footgun は、 'Layer' の `lyKind :: First MarkKind` が合成時点で
-- 不可逆に潰れるため **post-hoc には検出できない**。 検出には Layer に診断用
-- フィールドを足す必要があり、 Phase 11 A2 (Monoid 明文化) で扱う。
{-# LANGUAGE OverloadedStrings #-}
module Hgg.Plot.Validate
  ( -- * Aesthetic / 型
    Aesthetic(..)
  , aesName
  , ExpectedType(..)
  , ActualType(..)
    -- * 診断
  , Severity(..)
  , DiagnosticContext(..)
  , PlotErrorKind(..)
  , PlotWarningKind(..)
  , PlotDiagnostic(..)
  , diagnosticSeverity
  , renderDiagnostic
    -- * 必須 aesthetic
  , requiredAes
  , layerCols
    -- * validate / compile
  , validatePlot
  , validatePlotWith
  , suggest
  , CompiledPlot
  , compiledSpec
  , compilePlot
  , compilePlotWith
    -- * Backend capability matrix (= §5.5)
  , BackendName(..)
  , FeatureName(..)
  , BackendCapability(..)
  , svgCapability
  , pngCapability
  , pdfCapability
  , canvasCapability
  , webglCapability
  , checkCapability
  ) where

import           Data.List   (foldl', sortOn)
import           Data.Maybe  (isJust, mapMaybe)
import           Data.Monoid (First (..), Last (..))
import           Data.Text   (Text)
import qualified Data.Text   as T

import           Hgg.Plot.Spec

-- ===========================================================================
-- Aesthetic / 型
-- ===========================================================================

-- | mark が要求しうる aesthetic 種別 (= 診断メッセージ用)。
data Aesthetic
  = AesX | AesY | AesY2 | AesColor | AesErrorX | AesErrorY
  | AesSize | AesShape | AesDAG | AesCols
  | AesU | AesV   -- Phase 26 A2: vector field (quiver) の成分
  deriving (Show, Eq)

-- | 診断文に出す aesthetic 名 (= setter 名に寄せる)。
aesName :: Aesthetic -> Text
aesName a = case a of
  AesX      -> "x"
  AesY      -> "y"
  AesY2     -> "y2 (upper)"
  AesColor  -> "color"
  AesErrorX -> "errorX"
  AesErrorY -> "errorY"
  AesSize   -> "size"
  AesShape  -> "shape"
  AesDAG    -> "dag"
  AesCols   -> "columns"
  AesU      -> "u"
  AesV      -> "v"

data ExpectedType = ExpNumeric | ExpCategorical | ExpAny
  deriving (Show, Eq)

data ActualType = ActNumeric | ActCategorical | ActUnresolved
  deriving (Show, Eq)

-- ===========================================================================
-- 診断
-- ===========================================================================

data Severity = SevError | SevWarning | SevInfo
  deriving (Show, Eq, Ord)

-- | どの layer / mark で起きたか (= メッセージの文脈)。
data DiagnosticContext = DiagnosticContext
  { dcLayer :: Maybe Int        -- ^ 0 始まりの layer index (Nothing = 図全体)
  , dcMark  :: Maybe MarkKind
  } deriving (Show, Eq)

topCtx :: DiagnosticContext
topCtx = DiagnosticContext Nothing Nothing

data PlotErrorKind
  = MissingAesthetic MarkKind Aesthetic
  | ColumnNotFound Text [Text]          -- ^ 見つからない列名 + 候補 (編集距離)
  | ColumnTypeMismatch Text Aesthetic ExpectedType ActualType
  | EmptyPlot                           -- ^ layer が 1 つも無い
  | DistColsNonDistribution MarkKind    -- ^ ★ Phase 36 D3: distCols のレーンが分布 mark でない
  deriving (Show, Eq)

data PlotWarningKind
  = BackendUnsupported BackendName FeatureName
  | TooFewColumns Aesthetic Int Int     -- ^ 必要数 / 実数 (parallel 等)
  deriving (Show, Eq)

data PlotDiagnostic
  = PlotError   PlotErrorKind   DiagnosticContext
  | PlotWarning PlotWarningKind DiagnosticContext
  | PlotInfo    Text
  deriving (Show, Eq)

diagnosticSeverity :: PlotDiagnostic -> Severity
diagnosticSeverity PlotError{}   = SevError
diagnosticSeverity PlotWarning{} = SevWarning
diagnosticSeverity PlotInfo{}    = SevInfo

-- | 人間が読める actionable メッセージ (= §5.4 Diagnostics Policy)。
renderDiagnostic :: PlotDiagnostic -> Text
renderDiagnostic d = case d of
  PlotError k ctx   -> sev "error"   <> ctxStr ctx <> errMsg k
  PlotWarning k ctx -> sev "warning" <> ctxStr ctx <> warnMsg k
  PlotInfo t        -> sev "info" <> t
 where
  sev s = "[" <> s <> "] "
  ctxStr (DiagnosticContext ml mm) =
    let lp = maybe "" (\i -> "layer " <> tshow i <> " ") ml
        mp = maybe "" (\m -> "(" <> markName m <> ") ") mm
    in lp <> mp
  errMsg k = case k of
    MissingAesthetic m a ->
      markName m <> " は " <> reqList m <> " が必要です。 "
        <> aesName a <> " が未指定です。 setter `" <> aesName a
        <> " ...` を足してください。"
    ColumnNotFound n [] ->
      "列 \"" <> n <> "\" が Resolver で解決できません。 列名と Resolver の供給列を確認してください。"
    ColumnNotFound n cs ->
      "列 \"" <> n <> "\" が見つかりません。 もしかして: "
        <> T.intercalate " / " (map (\c -> "\"" <> c <> "\"") cs) <> " ?"
    ColumnTypeMismatch n a exp_ act ->
      "列 \"" <> n <> "\" を " <> aesName a <> " に使えません。 "
        <> expName exp_ <> " が必要ですが " <> actName act <> " でした。"
    EmptyPlot ->
      "layer が 1 つもありません。 `layer (scatter x y)` 等を合成してください。"
    DistColsNonDistribution m ->
      "distCols のレーンは 1D 分布 mark (box/violin/strip/swarm/raincloud) 専用です。 "
        <> markName m <> " は描画されません。 レーンを分布 mark にしてください。"
  warnMsg k = case k of
    BackendUnsupported b f ->
      backendName b <> " backend は " <> featureName f
        <> " 非対応です (fallback または無視されます)。"
    TooFewColumns a need got ->
      aesName a <> " は最低 " <> tshow need <> " 列必要ですが " <> tshow got <> " 列でした。"
  expName ExpNumeric     = "数値列"
  expName ExpCategorical = "カテゴリ列"
  expName ExpAny         = "任意の列"
  actName ActNumeric     = "数値列"
  actName ActCategorical = "カテゴリ (文字列) 列"
  actName ActUnresolved  = "未解決"
  reqList m = T.intercalate "+" (map aesName (requiredAes m))

tshow :: Show a => a -> Text
tshow = T.pack . show

markName :: MarkKind -> Text
markName = T.pack . drop 1 . show   -- "MScatter" -> "Scatter"

-- ===========================================================================
-- 必須 aesthetic (= constructor 定義から導出した事実、 Spec.hs L673-993)
-- ===========================================================================

-- | mark が描画に最低限要求する aesthetic。 これが欠けると 'validatePlot' が
-- 'MissingAesthetic' を返す。 categorical/numeric の別は型チェックで別途見る。
requiredAes :: MarkKind -> [Aesthetic]
requiredAes m = case m of
  MScatter    -> [AesX, AesY]
  MLine       -> [AesX, AesY]
  MBar        -> [AesX, AesY]
  MStep       -> [AesX, AesY]
  MStem       -> [AesX, AesY]
  MPie        -> [AesX, AesY]
  MWaterfall  -> [AesX, AesY]
  MTrace      -> [AesX, AesY]
  MViolin     -> [AesX, AesY]
  MStrip      -> [AesX, AesY]
  MSwarm      -> [AesX, AesY]
  MRaincloud  -> [AesX, AesY]
  MRidge      -> [AesX, AesY]
  MEss        -> [AesX, AesY]
  MHistogram  -> [AesX]
  MDensity    -> [AesX]
  MFreqPoly   -> [AesX]
  MAutocorr   -> [AesX]
  MBox        -> [AesY]
  MStatMean   -> [AesY]
  MStatMedian -> [AesY]
  MBand       -> [AesX, AesY, AesY2]
  MContour    -> [AesX, AesY, AesColor]
  MContourFilled -> [AesX, AesY, AesColor]
  MBin2d      -> [AesX, AesY]   -- z (AesColor) は任意 (無ければ count = geom_bin2d 既定)
  MTile       -> [AesX, AesY]   -- Phase 60: fill (AesColor) は任意 (colorBy で離散/連続)
  MHexbin     -> [AesX, AesY]   -- Phase 40: count は自動集計 (z 不要)
  MHeatmap    -> [AesX, AesY, AesColor]
  MCount      -> [AesX, AesY]
  MForest     -> [AesX, AesY, AesErrorX]
  MFunnel     -> [AesX, AesY]
  MParallel   -> [AesCols]
  MDAG        -> [AesDAG]
  MScatter3D  -> [AesX, AesY]
  -- Phase 11 A6: geom_text / geom_label (label 列は Aes ではないので x/y のみ)
  MText       -> [AesX, AesY]
  MLabel      -> [AesX, AesY]
  -- Phase 11 A6-2: geom_qq は sample 列のみ (encY)、 x は理論分位点を内部算出
  MQQ         -> [AesY]
  -- Phase 11 A6-4: stat_ecdf は sample 列のみ (encX)、 y は #(≤x)/n を内部算出
  MEcdf       -> [AesX]
  -- Phase 11 A6-4b: 区間 geom は x/y/errorY (= y±err)
  MLineRange  -> [AesX, AesY, AesErrorY]
  MPointRange -> [AesX, AesY, AesErrorY]
  MCrossbar   -> [AesX, AesY, AesErrorY]
  -- 半導体特化 (spec のみ、 render 未): 暫定の最小要求
  MWaferMap   -> [AesX, AesY]
  MControl    -> [AesY]
  -- Phase 16: stat-in (= ggplot stat_smooth)。 x/y 必須。 描画前に bridge resolveStats が
  -- band/line に展開する (未解決のまま描くと renderer は skip)。
  MStatLM     -> [AesX, AesY]
  MStatSmooth -> [AesX, AesY]
  -- Phase 16 B3: 多項式回帰 / 残差診断。 ともに x/y 必須。 resolveStats が band+line / scatter に展開。
  MStatPoly   -> [AesX, AesY]
  MStatResid  -> [AesX, AesY]
  -- Phase 52.D2: streamgraph は x/y/color (= 系列分割) 必須
  MStream     -> [AesX, AesY, AesColor]
  -- Phase 26 A2: vector field (quiver) は x/y/u/v 必須
  MQuiver     -> [AesX, AesY, AesU, AesV]
  -- Phase 51: custom mark は必須 aesthetic なし (データは closure/resolver/options 経由)。
  MCustom     -> []

-- ===========================================================================
-- validate
-- ===========================================================================

-- | 既知列名なしの検証 (= 列解決の成否のみ、 suggestion 無し)。
validatePlot :: Resolver -> VisualSpec -> [PlotDiagnostic]
validatePlot = validatePlotWith []

-- | 既知列名 (= Resolver が供給できる列の一覧) を渡すと 'ColumnNotFound' に
-- 編集距離 suggestion が付く。
validatePlotWith :: [Text] -> Resolver -> VisualSpec -> [PlotDiagnostic]
validatePlotWith known r spec =
  emptyCheck ++ layerDiags ++ subDiags
 where
  ls = vsLayers spec
  emptyCheck
    | null ls && null (vsSubplots spec) = [PlotError EmptyPlot topCtx]
    | otherwise                         = []
  layerDiags = concat (zipWith (validateLayer known r) [0 ..] ls)
  -- subplots は独立 spec なので再帰 (layer index は各 sub で 0 始まり)
  subDiags = concatMap (validatePlotWith known r) (vsSubplots spec)

-- | 1 layer の検証: 必須 aesthetic 欠落 + 列解決 + 型チェック。
validateLayer :: [Text] -> Resolver -> Int -> Layer -> [PlotDiagnostic]
validateLayer known r i ly =
  case getFirst (lyKind ly) of
    Nothing   -> []   -- mark 未指定の attribute-only layer (合成途中) はスキップ
    Just mark ->
      let ctx     = DiagnosticContext (Just i) (Just mark)
          present = layerCols ly
          missing =
            [ PlotError (MissingAesthetic mark a) ctx
            | a <- requiredAes mark
            , a `notElem` map fst present
            , a `notElem` [AesDAG, AesCols]   -- DAG/cols は別チェック
            ]
          dagMiss =
            [ PlotError (MissingAesthetic mark AesDAG) ctx
            | AesDAG `elem` requiredAes mark
            , Nothing <- [getLast (lyDAG ly)] ]
          colsMiss =
            [ PlotWarning (TooFewColumns AesCols 2 (length (lyHover ly))) ctx
            | AesCols `elem` requiredAes mark
            , length (lyHover ly) < 2 ]
          resolveDiags = concatMap (uncurry (checkCol known r ctx)) present
          -- ★ Phase 36 D3 ②: distCols(= 合成が複数の値列)のサブマークは 1D 分布 mark 専用。
          distColsDiags
            | length (compositeLanes ly) > 1 =
                [ PlotError (DistColsNonDistribution k) (DiagnosticContext (Just i) (Just k))
                | sub <- ly : lyOverlay ly
                , Just k <- [getFirst (lyKind sub)]
                , k `notElem` [MBox, MViolin, MStrip, MSwarm, MRaincloud] ]
            | otherwise = []
      in missing ++ dagMiss ++ colsMiss ++ resolveDiags ++ distColsDiags

-- | layer に実際に設定済みの (aesthetic, 列) 組を取り出す。
layerCols :: Layer -> [(Aesthetic, ColRef)]
layerCols ly = mapMaybe pick
  [ (AesX,      getLast (lyEncX ly))
  , (AesY,      getLast (lyEncY ly))
  , (AesY2,     getLast (lyEncY2 ly))
  , (AesErrorX, getLast (lyErrorX ly))
  , (AesErrorY, getLast (lyErrorY ly))
  , (AesU,      getLast (lyEncU ly))   -- Phase 26 A2: quiver u
  , (AesV,      getLast (lyEncV ly))   -- Phase 26 A2: quiver v
  ] ++ colorCol
 where
  pick (a, Just c) = Just (a, c)
  pick (_, Nothing) = Nothing
  colorCol = case getLast (lyColor ly) of
    Just (ColorByCol c)        -> [(AesColor, c)]
    Just (ColorByContinuous c) -> [(AesColor, c)]
    _                          -> []

-- | 列の解決可否 + 型チェック。 数値要求 aesthetic に文字列列が来たら型不一致。
checkCol :: [Text] -> Resolver -> DiagnosticContext -> Aesthetic -> ColRef -> [PlotDiagnostic]
checkCol known r ctx aes cr = case cr of
  ColByName n
    | not (isJust (resolveCol r cr)) ->
        [PlotError (ColumnNotFound n (suggest known n)) ctx]
    | otherwise -> typeCheck n
  _ -> typeCheck ""   -- inline は常に解決可、 型のみ
 where
  typeCheck n = case (expectedFor aes, resolveCol r cr) of
    (ExpNumeric, Just (TxtData _)) ->
      [PlotError (ColumnTypeMismatch n aes ExpNumeric ActCategorical) ctx]
    _ -> []

-- | aesthetic が数値を要求するか。 x/y は mark により categorical 可なので緩く ExpAny。
-- color (continuous 経路で来たもの) と error bar は数値必須。
expectedFor :: Aesthetic -> ExpectedType
expectedFor AesErrorX = ExpNumeric
expectedFor AesErrorY = ExpNumeric
expectedFor AesU      = ExpNumeric   -- Phase 26 A2: quiver 成分は数値
expectedFor AesV      = ExpNumeric
expectedFor _         = ExpAny

-- ===========================================================================
-- 編集距離 suggestion (= Levenshtein、 距離 ≤ 3 を近い順に最大 3 件)
-- ===========================================================================

suggest :: [Text] -> Text -> [Text]
suggest known target =
  take 3 . map fst . sortOn snd $
    [ (k, d) | k <- known, let d = levenshtein target k, d <= maxDist ]
 where
  maxDist = max 2 (T.length target `div` 2)

-- 標準 Levenshtein (Rosetta Code Haskell 版): 各行を scanl で構築。
levenshtein :: Text -> Text -> Int
levenshtein a b = last (foldl' transform [0 .. length s1] s2)
 where
  s1 = T.unpack a
  s2 = T.unpack b
  transform prev@(p0 : _) c =
    scanl calc (p0 + 1) (zip3 s1 prev (tail prev))
   where
    calc left (c1, diag, up) =
      minimum [up + 1, left + 1, diag + fromEnum (c1 /= c)]
  transform [] _ = []

-- ===========================================================================
-- compile (= VisualSpec を「検証済」 でラップ)
-- ===========================================================================

-- | 検証を通過した 'VisualSpec'。 backend はこれを受け取る形に寄せられる
-- (現状は 'compiledSpec' で素の VisualSpec を取り出して既存 backend に渡せる)。
newtype CompiledPlot = CompiledPlot { compiledSpec :: VisualSpec }
  deriving (Show)

-- | error が無ければ 'CompiledPlot'、 あれば error 一覧を返す
-- (warning は通過させる)。
compilePlot :: Resolver -> VisualSpec -> Either [PlotDiagnostic] CompiledPlot
compilePlot = compilePlotWith []

compilePlotWith :: [Text] -> Resolver -> VisualSpec
                -> Either [PlotDiagnostic] CompiledPlot
compilePlotWith known r spec =
  case filter ((== SevError) . diagnosticSeverity) (validatePlotWith known r spec) of
    []   -> Right (CompiledPlot spec)
    errs -> Left errs

-- ===========================================================================
-- Backend capability matrix (= §5.5)
-- ===========================================================================

data BackendName = BackendSVG | BackendPNG | BackendPDF | BackendCanvas | BackendWebGL
  deriving (Show, Eq)

backendName :: BackendName -> Text
backendName b = case b of
  BackendSVG    -> "SVG"
  BackendPNG    -> "PNG"
  BackendPDF    -> "PDF"
  BackendCanvas -> "Canvas"
  BackendWebGL  -> "WebGL"

data FeatureName
  = FeatTransparency | FeatHover | FeatInteractive3D | FeatProjected3D
  deriving (Show, Eq)

featureName :: FeatureName -> Text
featureName f = case f of
  FeatTransparency  -> "透明度 (alpha)"
  FeatHover         -> "hover tooltip"
  FeatInteractive3D -> "interactive 3D"
  FeatProjected3D   -> "3D (CPU projection)"

-- | backend ごとの対応機能 (= §5.5)。
data BackendCapability = BackendCapability
  { capName          :: BackendName
  , capTransparency  :: Bool
  , capHover         :: Bool
  , capProjected3D   :: Bool
  , capInteractive3D :: Bool
  } deriving (Show, Eq)

svgCapability, pngCapability, pdfCapability, canvasCapability, webglCapability
  :: BackendCapability
svgCapability    = BackendCapability BackendSVG    True  False True  False
pngCapability    = BackendCapability BackendPNG    True  False True  False
pdfCapability    = BackendCapability BackendPDF    True  False True  False
canvasCapability = BackendCapability BackendCanvas True  True  True  False
webglCapability  = BackendCapability BackendWebGL  True  True  True  True

-- | spec が使う機能のうち backend 非対応なものを warning 化。
checkCapability :: BackendCapability -> VisualSpec -> [PlotDiagnostic]
checkCapability cap spec = concatMap layerCap (vsLayers spec)
                        ++ concatMap (checkCapability cap) (vsSubplots spec)
 where
  b = capName cap
  layerCap ly =
    let ctx = DiagnosticContext Nothing (getFirst (lyKind ly))
        alphaUsed = case getLast (lyAlpha ly) of
          Just a  -> a < 1.0
          Nothing -> False
        hoverUsed = not (null (lyHover ly))
        is3D = getFirst (lyKind ly) == Just MScatter3D
    in  [ PlotWarning (BackendUnsupported b FeatTransparency) ctx
        | alphaUsed, not (capTransparency cap) ]
     ++ [ PlotWarning (BackendUnsupported b FeatHover) ctx
        | hoverUsed, not (capHover cap) ]
     ++ [ PlotWarning (BackendUnsupported b FeatProjected3D) ctx
        | is3D, not (capProjected3D cap) ]
