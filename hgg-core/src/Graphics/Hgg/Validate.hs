-- |
-- Module      : Graphics.Hgg.Validate
-- Description : Layer 3.5 — compile / validate / diagnostics (core hardening)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- [日本語]: 設計方針:
--
--     * 「'VisualSpec' は直接描画しない」 を型で固定する。 backend に渡す前に
--       'compilePlot' を通し、 必須 aesthetic 欠落 / 列解決失敗 / 型不一致 を検出。
--     * 診断は __actionable__ であること (= 「Missing y」 ではなく
--       「scatter は x と y が必要。 y 列が未指定。 `y "yield"` を足してください」)。
--     * 列名解決失敗には __編集距離 suggestion__ を添える ('validatePlotWith' に
--       既知列名を渡したとき)。
--     * 'BackendCapability' で backend 非対応機能を compile 時に検出する。
--
--   本 module は render を一切呼ばない (= 出力中立)。 既存 backend は当面そのまま
--   動き、 段階的に 'compilePlot' 経由へ寄せる。
--
--   既知の制約: 「1 layer に mark 2 個 (`scatter x y <> line x y`) を合成して 2
--   個目が黙って消える」 footgun は、 'Layer' の `lyKind :: First MarkKind` が
--   合成時点で不可逆に潰れるため __post-hoc には検出できない__。 検出には Layer
--   に診断用フィールドを足す必要があり、 別途扱う (Monoid 明文化)。
-- [English]: Design policy:
--
--     * Fixes "'VisualSpec' does not render directly" at the type level.
--       Before handing off to a backend, it passes through 'compilePlot',
--       which detects missing required aesthetics, column-resolution
--       failures, and type mismatches.
--     * Diagnostics must be __actionable__ (not "Missing y" but "scatter
--       requires x and y. The y column is unset. Add \`y \"yield\"\`.").
--     * Column-resolution failures come with an __edit-distance suggestion__
--       (when known column names are passed to 'validatePlotWith').
--     * 'BackendCapability' detects backend-unsupported features at compile
--       time.
--
--   This module never calls render (it is output-neutral). Existing
--   backends keep working unchanged for now, and are migrated to go through
--   'compilePlot' incrementally.
--
--   A known limitation: the footgun where composing two marks into one
--   layer (\`scatter x y <> line x y\`) silently drops the second one
--   __cannot be detected post-hoc__, because the \`lyKind :: First MarkKind\`
--   field of 'Layer' collapses irreversibly at composition time. Detecting
--   it would require adding a diagnostic field to Layer, which is handled
--   separately (as part of making the Monoid semantics explicit).
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Hgg.Validate
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
  , facetInlineDiagnostics        -- ★ Phase 62 A4 (§3)
  , reportFacetInlineWarnings     -- ★ Phase 62 A4: backend save 系共用の stderr 報告
  , ternaryMarkWarningsFor        -- ★ Phase 64 A13
  , reportTernaryMarkWarnings     -- ★ Phase 64 A13: coordTernary 非対応 mark の stderr 報告
  , suggest
  , CompiledPlot
  , compiledSpec
  , compilePlot
  , compilePlotWith
    -- * Backend capability matrix
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
import qualified Data.Vector as V
import           System.IO   (hPutStrLn, stderr)

import           Graphics.Hgg.Spec

-- ===========================================================================
-- Aesthetic / 型
-- ===========================================================================

-- | [日本語]: mark が要求しうる aesthetic 種別 (= 診断メッセージ用)。
--   [English]: The kinds of aesthetic a mark may require (for diagnostic
--   messages).
data Aesthetic
  = AesX | AesY | AesY2 | AesColor | AesErrorX | AesErrorY
  | AesSize | AesShape | AesDAG | AesCols
  | AesU | AesV   -- Phase 26 A2: vector field (quiver) の成分
  deriving (Show, Eq)

-- | [日本語]: 診断文に出す aesthetic 名 (= setter 名に寄せる)。
--   [English]: The aesthetic name shown in diagnostic text (matches the
--   setter name).
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

-- | [日本語]: どの layer / mark で起きたか (= メッセージの文脈)。
--   [English]: Which layer / mark the diagnostic occurred in (message
--   context).
data DiagnosticContext = DiagnosticContext
  { dcLayer :: Maybe Int        -- ^ [日本語]: 0 始まりの layer index (Nothing = 図全体)。
                                 --   [English]: The 0-based layer index (Nothing means the whole figure).
  , dcMark  :: Maybe MarkKind
  } deriving (Show, Eq)

topCtx :: DiagnosticContext
topCtx = DiagnosticContext Nothing Nothing

data PlotErrorKind
  = MissingAesthetic MarkKind Aesthetic
  | ColumnNotFound Text [Text]
    -- ^ [日本語]: 見つからない列名 + 候補 (編集距離)。
    --   [English]: The column name that could not be found, plus candidates (by edit distance).
  | ColumnTypeMismatch Text Aesthetic ExpectedType ActualType
  | EmptyPlot
    -- ^ [日本語]: layer が 1 つも無い。
    --   [English]: There is not a single layer.
  | DistColsNonDistribution MarkKind
    -- ^ [日本語]: ★ distCols のレーンが分布 mark でない。
    --   [English]: A distCols lane whose mark is not a distribution mark.
  deriving (Show, Eq)

data PlotWarningKind
  = BackendUnsupported BackendName FeatureName
  | TooFewColumns Aesthetic Int Int
    -- ^ [日本語]: 必要数 / 実数 (parallel 等)。
    --   [English]: The required count vs. the actual count (for e.g. parallel coordinates).
  | FacetInlineLengthMismatch Aesthetic Int Int
    -- ^ [日本語]: ★ facet 列と長さの異なる inline 列 (inline 長 / facet 長)。
    --   facet 分割がこの列に効かず全 panel に同一データが描かれる。 描画は継続する。
    --   [English]: An inline column whose length differs from the facet
    --   column (inline length / facet length). Facet splitting has no
    --   effect on this column, so the same data is drawn on every panel.
    --   Rendering continues regardless.
  | TernaryUnsupportedMark MarkKind
    -- ^ [日本語]: ★ Phase 64 A13: 三角座標 (coordTernary) で意味を成さない mark。
    --   ternary は point/line/area/text 系の mark のみ意味を持つ。 それ以外の mark は
    --   投影自体は行われる (= 黙って Cartesian に落ちはしない) が結果は意味を持たない。
    --   描画は継続する。
    --   [English]: A mark meaningless under ternary coordinates (coordTernary).
    --   Ternary only makes sense for point/line/area/text marks; other marks
    --   are still projected (they do not silently fall back to Cartesian) but
    --   the result is not meaningful. Rendering continues regardless.
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

-- | [日本語]: 人間が読める actionable メッセージ (= §5.4 Diagnostics Policy)。
--   [English]: A human-readable, actionable message (§5.4 Diagnostics
--   Policy).
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
    FacetInlineLengthMismatch a m n ->
      "facet 列 (" <> tshow n <> " 行) と長さの異なる inline " <> aesName a
        <> " 列 (" <> tshow m <> " 行) があります。 facet 分割がこの列に効かず、"
        <> "全 panel に同一データが描かれます。 列長を facet 列と揃えるか、"
        <> "名前参照 + Resolver (saveSVGWith / savePNGWith 等) を使ってください。"
    TernaryUnsupportedMark m ->
      "三角座標 (coordTernary) は point / line / area / text 系の mark のみ意味を持ちます。 "
        <> markName m <> " は三角座標で意味を持ちません (投影は行われますが結果は不定です)。 "
        <> "scatter / line / band / text へ変えるか、 coordTernary を外してください。"
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

-- | [日本語]: mark が描画に最低限要求する aesthetic。 これが欠けると
--   'validatePlot' が 'MissingAesthetic' を返す。 categorical/numeric の別は
--   型チェックで別途見る。
--   [English]: The minimum aesthetics a mark requires to render. Missing one
--   causes 'validatePlot' to return 'MissingAesthetic'. Whether a value is
--   categorical or numeric is checked separately, by type checking.
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

-- | [日本語]: 既知列名なしの検証 (= 列解決の成否のみ、 suggestion 無し)。
--   [English]: Validates without known column names (only whether columns
--   resolve; no suggestions).
validatePlot :: Resolver -> VisualSpec -> [PlotDiagnostic]
validatePlot = validatePlotWith []

-- | [日本語]: 既知列名 (= Resolver が供給できる列の一覧) を渡すと
--   'ColumnNotFound' に編集距離 suggestion が付く。
--   [English]: Passing known column names (the columns the Resolver can
--   supply) attaches an edit-distance suggestion to 'ColumnNotFound'.
validatePlotWith :: [Text] -> Resolver -> VisualSpec -> [PlotDiagnostic]
validatePlotWith known r spec =
  emptyCheck ++ layerDiags ++ ternaryDiags ++ subDiags
 where
  ls = vsLayers spec
  emptyCheck
    | null ls && null (vsSubplots spec) = [PlotError EmptyPlot topCtx]
    | otherwise                         = []
  layerDiags = concat (zipWith (validateLayer known r) [0 ..] ls)
  -- ★ Phase 64 A13: coordTernary で point/line/area/text 以外の mark を警告 (= 黙って
  --   Cartesian に落とさない・A13 の要件)。 subplot 再帰は subDiags 側が担う。
  ternaryDiags = ternaryMarkWarningsFor spec
  -- subplots は独立 spec なので再帰 (layer index は各 sub で 0 始まり)
  subDiags = concatMap (validatePlotWith known r) (vsSubplots spec)

-- | [日本語]: ★ Phase 64 A13: 単一 spec について、 coordTernary 下で 'ternaryValidMarks'
--   に無い mark (= point/line/area/text 以外) を 'TernaryUnsupportedMark' 警告にする。
--   subplot 再帰はしない (呼出側が担う)。 mark 未指定 layer / 'MCustom' は対象外。
--   [English]: Phase 64 A13. For a single spec, flags any mark not in
--   'ternaryValidMarks' (i.e. not point/line/area/text) under coordTernary as a
--   'TernaryUnsupportedMark' warning. Does not recurse into subplots (the caller
--   does). Mark-less layers and 'MCustom' are exempt.
ternaryMarkWarningsFor :: VisualSpec -> [PlotDiagnostic]
ternaryMarkWarningsFor spec
  -- ★ Phase 69 A4: CoordTernary が opts を持つようになったのでパターンで判定
  --   (明示 coordTernary 指定時のみ・従来挙動を維持)。
  | Just (CoordTernary _) <- getLast (vsCoord spec) =
      [ PlotWarning (TernaryUnsupportedMark m) (DiagnosticContext (Just i) (Just m))
      | (i, ly) <- zip [0 ..] (vsLayers spec)
      , Just m <- [getFirst (lyKind ly)]
      , m `notElem` ternaryValidMarks ]
  | otherwise = []

-- | [日本語]: ★ Phase 64 A13: 'ternaryMarkWarningsFor' を stderr へ報告する backend 共用
--   helper (SVG / PNG / PDF / TeX の save 系入口から 'reportFacetInlineWarnings' と並べて
--   呼ぶ)。 診断ゼロなら無音。 描画は止めない (= 描画継続 + 警告)。 'validatePlot' 経由で
--   subplot も再帰的に拾い、 'TernaryUnsupportedMark' 警告のみに絞る。
--   [English]: Phase 64 A13. A backend-shared helper that reports
--   'ternaryMarkWarningsFor' to stderr (called alongside
--   'reportFacetInlineWarnings' from the SVG / PNG / PDF / TeX save entry
--   points). Silent when there are none. Does not stop rendering. Goes through
--   'validatePlot' to also pick up subplots recursively, filtering to only the
--   'TernaryUnsupportedMark' warnings.
reportTernaryMarkWarnings :: Resolver -> VisualSpec -> IO ()
reportTernaryMarkWarnings r spec =
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        [ d | d@(PlotWarning (TernaryUnsupportedMark _) _) <- validatePlot r spec ]

-- | [日本語]: ★ Phase 64 A13: 三角座標 (ternary) で意味を持つ mark。 point (scatter) /
--   line (line/trace) / area (band) / text (text/label)。 user 定義の 'MCustom' は
--   判定不能ゆえ対象外 (警告しない)。
--   [English]: Marks meaningful under ternary coordinates: point (scatter),
--   line (line/trace), area (band), text (text/label). User-defined 'MCustom'
--   is exempt (undecidable, so not warned).
ternaryValidMarks :: [MarkKind]
ternaryValidMarks = [MScatter, MLine, MTrace, MBand, MText, MLabel, MCustom]

-- | [日本語]: 1 layer の検証: 必須 aesthetic 欠落 + 列解決 + 型チェック。
--   [English]: Validates a single layer: missing required aesthetics,
--   column resolution, and type checking.
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

-- | [日本語]: layer に実際に設定済みの (aesthetic, 列) 組を取り出す。
--   [English]: Extracts the (aesthetic, column) pairs actually set on a
--   layer.
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

-- | [日本語]: ★ facet 列と長さの異なる inline encoding の検出。
--   inline 列は Resolver を通らないため、 facet の行分割 (@subsetInlineSpec@) は
--   __facet 列と同じ長さの inline のみ__に効く。 長さが違う inline が encoding
--   に残っていると、 その列は分割されず全 panel に同一データが描かれる —
--   それを明示検出する (検出しても描画は継続 = 非破壊、 user 決定)。
--   判定は 'applyDiscreteLimits' 適用後の姿で行う (= 経路 2 の bake / limits に
--   よる行 drop の後、 実際に render が見る spec と同条件。 limits の行 drop で
--   facet 列と layer が desync するケースもこれで捕まる)。
--   [English]: Detects inline encodings whose length differs from the
--   facet column. Since inline columns bypass the Resolver, facet row
--   splitting (@subsetInlineSpec@) applies __only to inline columns whose length matches the facet column__.
--   If a mismatched-length inline
--   remains in the encoding, that column is not split and the same data is
--   drawn on every panel — this function explicitly detects that case
--   (detection does not stop rendering: it is non-destructive, per user
--   decision). The check is performed on the spec after
--   'applyDiscreteLimits' has been applied (that is, after row drops from
--   path-2 baking / limits — the same condition the actual renderer sees.
--   This also catches cases where limits' row drops desync the facet column
--   from a layer).
facetInlineDiagnostics :: Resolver -> VisualSpec -> [PlotDiagnostic]
facetInlineDiagnostics r spec0 = go (applyDiscreteLimits r spec0) ++ subDiags
 where
  -- subplots は独立 spec (自分の facet を持てる) なので再帰
  subDiags = concatMap (facetInlineDiagnostics r) (vsSubplots spec0)
  go spec = case facetLens spec of
    []      -> []
    (n : _) -> concat (zipWith (layerMismatch n) [0 ..] (vsLayers spec))
  facetLens spec = mapMaybe colLen
    (mapMaybe getLast [vsFacet spec, vsFacetRow spec, vsFacetCol spec])
  colLen cr = case resolveCol r cr of
    Just (NumData v) -> Just (V.length v)
    Just (TxtData v) -> Just (V.length v)
    Nothing          -> Nothing
  layerMismatch n i ly =
    let ctx = DiagnosticContext (Just i) (getFirst (lyKind ly))
    in [ PlotWarning (FacetInlineLengthMismatch a m n) ctx
       | (a, cr) <- rowCols ly
       , Just m <- [inlineLen cr]
       , m /= n ]
  inlineLen (ColNum v) = Just (V.length v)
  inlineLen (ColTxt v) = Just (V.length v)
  inlineLen _          = Nothing
  -- layerCols (aes 付き encoding) + size/shape encoding。 chain/label/hover 等は
  -- Aesthetic tag が無いため対象外 (要るなら Aesthetic 追加とセットで拡張)。
  rowCols ly = layerCols ly
    ++ [ (AesSize,  c) | Just c <- [getLast (lySizeBy ly)] ]
    ++ [ (AesShape, c) | Just c <- [getLast (lyShapeBy ly)] ]

-- | [日本語]: ★ 'facetInlineDiagnostics' を stderr へ報告する backend 共用
--   helper (SVG / PNG / PDF / TeX の save 系入口から呼ぶ)。 診断ゼロなら無音。
--   描画は止めない (= 描画継続 + 警告)。
--   [English]: A backend-shared helper that reports
--   'facetInlineDiagnostics' to stderr (called from the SVG / PNG / PDF /
--   TeX save entry points). Silent when there are no diagnostics. Does not
--   stop rendering (rendering continues, with a warning).
reportFacetInlineWarnings :: Resolver -> VisualSpec -> IO ()
reportFacetInlineWarnings r spec =
  mapM_ (hPutStrLn stderr . T.unpack . renderDiagnostic)
        (facetInlineDiagnostics r spec)

-- | [日本語]: 列の解決可否 + 型チェック。 数値要求 aesthetic に文字列列が来たら
--   型不一致。
--   [English]: Checks column resolvability plus type. A textual column
--   supplied to a numeric-required aesthetic is a type mismatch.
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

-- | [日本語]: aesthetic が数値を要求するか。 x/y は mark により categorical
--   可なので緩く ExpAny。 color (continuous 経路で来たもの) と error bar は
--   数値必須。
--   [English]: Whether an aesthetic requires a numeric value. x/y are left
--   loose as ExpAny since they may be categorical depending on the mark.
--   color (when it arrives via the continuous path) and error bars require
--   numeric values.
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

-- | [日本語]: 検証を通過した 'VisualSpec'。 backend はこれを受け取る形に寄せ
--   られる (現状は 'compiledSpec' で素の VisualSpec を取り出して既存 backend
--   に渡せる)。
--   [English]: A 'VisualSpec' that has passed validation. Backends can be
--   migrated to accept this type (currently, 'compiledSpec' extracts the
--   plain VisualSpec to pass to existing backends).
newtype CompiledPlot = CompiledPlot { compiledSpec :: VisualSpec }
  deriving (Show)

-- | [日本語]: error が無ければ 'CompiledPlot'、 あれば error 一覧を返す
--   (warning は通過させる)。
--   [English]: Returns a 'CompiledPlot' if there are no errors, or the list
--   of errors otherwise (warnings are allowed through).
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

-- | [日本語]: backend ごとの対応機能 (= §5.5)。
--   [English]: The features each backend supports (§5.5).
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

-- | [日本語]: spec が使う機能のうち backend 非対応なものを warning 化。
--   [English]: Turns any feature used by the spec that the backend does not
--   support into a warning.
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
