# 08. 助けの求め方 (R4DS 2e Ch.8 "Workflow: getting help")

> 一次情報: **R for Data Science 2e, Ch.8 "Workflow: getting help"**
> <https://r4ds.hadley.nz/workflow-help>
> データ: なし(散文の運用章)。 実演コードは [`GettingHelp.hs`](GettingHelp.hs)。

R4DS 第 8 章は **図を描かない** 散文の運用章で、 「Whole Game」 パートを締めくくります。
主題は次の 3 つ:

1. **Google is your friend** — 検索(とくにエラーメッセージ)と Stack Overflow の使い方。
2. **Making a reprex** — 最小・再現可能な例(**repr**oducible **ex**ample)の作り方。
3. **Investing in yourself** — 日々の学習とコミュニティの追い方。

どれも **R / RStudio / tidyverse 固有の助言** なので、 ここでは近似や置換ではなく
「**R の運用 → Haskell の等価運用**」 という honest な対応づけで翻案します。
実演できる部分(reprex の最小例)だけ [`GettingHelp.hs`](GettingHelp.hs) で実際に動かします。

```sh
cd docs/tutorials/08-getting-help
cabal run tut-08-getting-help
```

★このファイル群自体が R4DS §8.2 のいう **reprex の見本** です(先頭に import をまとめ、
データをインラインに埋め込み、 問題に関係するコードだけを最小に書いています)。

---

## §8.1 Google is your friend(検索を使う)

R: 詰まったらまず Google。 クエリに "R" を足すと R 関連に絞れる。 さらに "tidyverse" /
"ggplot2" のようなパッケージ名を足すと、 馴染みのあるコードに辿り着きやすい。 とくに
**エラーメッセージの検索** が有効。 英語以外のエラーは `Sys.setenv(LANGUAGE = "en")` で
英語化してから検索すると見つかりやすい。 Google で出なければ **Stack Overflow** で
`[R]` タグ付きで探す。

Haskell の等価運用:

| R / tidyverse の助言 | Haskell の等価 |
|---|---|
| クエリに "R" を足す | クエリに "Haskell" を足す |
| "ggplot2" / "tidyverse" でさらに絞る | "dataframe" / "hgg" / ライブラリ名でさらに絞る |
| エラーメッセージをそのまま検索 | GHC のエラー/警告メッセージをそのまま検索 |
| 関数の使い方を知りたい | **Hoogle** (<https://hoogle.haskell.org>) で型・関数名から検索 |
| パッケージのドキュメント | **Hackage** (<https://hackage.haskell.org>) の haddock |
| `?function` でヘルプ | GHCi の `:doc 関数名` / `:info 関数名` / `:type 関数名` |
| Stack Overflow `[R]` タグ | Stack Overflow `[haskell]` タグ |

★Hoogle は R に無い強力な検索です。 **型シグネチャから関数を逆引き** できます
(例: `(a -> b) -> [a] -> [b]` で検索すると `map` が出る)。 「やりたい変換の型は
分かるが関数名が思い出せない」 ときの定石です。 エラーメッセージ検索は R と同じく有効で、
GHC のエラーは英語が既定なので言語切替(R の `Sys.setenv` 相当)は不要です。

---

## §8.2 Making a reprex(最小・再現可能な例を作る)

R: 検索で見つからなければ **reprex**(minimal **repr**oducible **ex**ample)を作るのが
良い。 reprex には 2 つの要件がある:

1. **再現可能(reproducible)**: 必要な `library()` 呼び出しと、 必要なオブジェクトの
   生成を **すべて含める**。 tidyverse の `reprex` パッケージを使うと取りこぼしを防げる。
2. **最小(minimal)**: 問題に直接関係しないものを **すべて削る**。 実データより小さく
   単純なオブジェクト(あるいは組み込みデータ)に置き換える。

R4DS は `reprex::reprex()` で、 コードと **その出力** を `#>` 付き Markdown に整形して
クリップボードに入れる手順を示します:

```r
# R
y <- 1:4
mean(y)
#> [1] 2.5
```

### Haskell の等価 = 自己完結した最小コード + その出力

Haskell には tidyverse の `reprex` パッケージのような専用ツールはありませんが、
**reprex の原則はそのまま当てはまります**。 同じ例を Haskell で書くと:

```haskell
-- 必要な import を先頭に (= 必要パッケージを明示)
import qualified DataFrame as DF

main :: IO ()
main = do
  let y = [1 .. 4] :: [Double]
  print (sum y / fromIntegral (length y))   -- = R の mean(y)
  -- #> 2.5
```

GHCi なら式を打てば下に結果が出る(R のコンソールと同じ)ので、 そのコピペが
そのまま reprex になります。 [`GettingHelp.hs`](GettingHelp.hs) を `cabal run` すると、
この `#> 2.5` を実際に再現します。

R4DS が挙げる「再現可能にする 3 要素」 を Haskell に対応づけると:

| R の 3 要素 | Haskell の等価 |
|---|---|
| **Packages**: 必要な `library()` を先頭に。 最新版か確認(`tidyverse_update()`) | 必要な `import` を先頭に。 `.cabal` の `build-depends` で依存を宣言。 最新版確認は `cabal outdated` / `ghcup` |
| **Data**: `dput(df)` でデータ再生成コードを吐かせて貼る | データを `fromNamedColumns` / `fromList` で **リテラルとして埋め込む**(下記) |
| **Code**: 空白・短く分かる変数名・コメントで問題箇所を示す・無関係を削る | 同じ(本リポジトリの [Coding Style](../../../CLAUDE.md))。 GHC 警告(`-Wall`)で無駄も拾える |

### データの埋め込み(= R の `dput()`)

R の `dput(mtcars)` は「データを再生成する R コード」 を吐きます。 Haskell の等価は、
データを **コード中のリテラルとして組む** ことです。 これなら相手は CSV を別途用意せず、
貼って即実行できます:

```haskell
let toy = DF.fromNamedColumns
      [ ("id",    DF.fromList ([1, 2, 3]          :: [Int]))
      , ("group", DF.fromList (["a", "b", "a"]    :: [Text]))
      , ("value", DF.fromList ([10.0, 20.0, 30.0] :: [Double])) ]
-- #> sum(value) = 60.0
```

R の助言どおり、 問題を示す **最小の部分集合** を使います(実データ全量ではなく、
バグが再現する一番小さい toy)。

### 最後に「まっさらな環境で動くか」 を確認する

R4DS は「fresh な R セッションを起動してコピペし、 本当に再現するか確認せよ」 と
締めくくります。 Haskell の等価は **別シェルで `cabal run`**(依存も含めビルドし直し)です。
同じ出力が出れば、 その例は本当に self-contained・再現可能です。

---

## §8.3 Investing in yourself(自分に投資する)

R: 問題が起きる前に、 日々少しずつ学ぶ。 tidyverse チームの動向は
[tidyverse blog](https://www.tidyverse.org/blog/)、 R コミュニティ全体は
[R Weekly](https://rweekly.org) で追う。

Haskell の等価(コミュニティ・情報源の対応):

| R の情報源 | Haskell の等価 |
|---|---|
| tidyverse blog | [GHC blog](https://www.haskell.org/ghc/blog.html) / 各ライブラリの CHANGELOG・GitHub |
| R Weekly(週次まとめ) | [Haskell Weekly](https://haskellweekly.news)(週次ニュースレター) |
| Stack Overflow / コミュニティ Q&A | [Haskell Discourse](https://discourse.haskell.org) / r/haskell / Libera の `#haskell` |
| CRAN Task Views | [Hackage](https://hackage.haskell.org) のカテゴリ・[Stackage](https://www.stackage.org) |

本プロジェクト固有では、 hanalyze / hgg の `src/` 実装と `test/` が
一次情報です([CLAUDE.md](../../../CLAUDE.md) の「事実か憶測かを明示する」 の精神どおり、
仕様の根拠はソース実装を grep で確かめます)。

---

## §8.4 Summary(まとめ)

R4DS の本章は「Whole Game」 パートの結びです。 ここまでで可視化・変換・整形・読み込みと、
データサイエンスのプロセス全体を一巡しました。 次のパート以降は各要素を深掘りします。

本チュートリアル群でも同じ流れで、 Ch.1〜Ch.8 で全体像をなぞりました。 以降の章では
grammar of graphics やレイヤ、 探索的データ解析(EDA)へと細部に入っていきます。

---

## R↔Haskell 対応のまとめ(本章で示した写像)

| R / RStudio / tidyverse | Haskell の等価 |
|---|---|
| Google に "R" + パッケージ名 | Google に "Haskell" + ライブラリ名 |
| `?function`(関数ヘルプ) | GHCi `:doc` / `:info` / `:type`、 Hoogle、 Hackage haddock |
| 型から関数を引く手段(なし) | **Hoogle**(型シグネチャで逆引き) |
| `reprex::reprex()` | 専用ツールはなし。 ただし reprex の原則(import を先頭・最小・出力同梱)は同じ |
| `dput(df)`(データ再生成コード) | `fromNamedColumns` / `fromList` でデータをリテラル埋め込み |
| fresh な R セッションで再確認 | 別シェルで `cabal run`(依存込みビルドし直し) |
| `tidyverse_update()` / 最新版確認 | `cabal outdated` / `ghcup` |
| R Weekly / tidyverse blog | Haskell Weekly / GHC blog / Haskell Discourse |

## できないこと / 近似せず記録した相違

- **`reprex` パッケージの専用糖衣はなし**: R はコードを `reprex()` に通すと、 出力を `#>`
  付き Markdown に整形してクリップボードへ自動コピーします。 Haskell にこの自動整形ツールは
  ありません。 ただし本章の眼目は「再現可能で最小な例を作る」 という **規律** であり、
  それは import を先頭にまとめ・データを埋め込み・別シェルで `cabal run` する運用で
  完全に満たせます(整形を自動化するツールが無いだけ)。
- **R/RStudio 固有の UI**: RStudio Viewer のプレビュー・クリップボード連携・Server/Cloud
  での選択コピー等は RStudio 固有の操作で、 本章では対応する Haskell の運用(GHCi / Hoogle /
  `cabal run`)を表で示すにとどめます(R4DS でも本章に解析図はゼロ = 散文の助言章)。
