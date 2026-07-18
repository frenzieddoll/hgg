# Releasing hgg to Hackage

This monorepo contains 14 packages, but **only a subset is published to
Hackage**. The rest stay repository-only (buildable from source via
`cabal.project`, not uploaded).

## Publish set

| Package | Hackage | Notes |
|---|---|---|
| `hgg-core` | ✅ | no deps beyond base/vector/text/containers |
| `hgg-frame` | ✅ | DataFrame integration (`PlotData` / `\|>>`); required by every backend below |
| `hgg-svg` | ✅ | SVG backend |
| `hgg-pdf` | ✅ | PDF backend |
| `hgg-rasterific` | ✅ | PNG backend |
| `hgg-latex` | ✅ | LaTeX (TikZ) backend |
| `hgg-3d` | ✅ | 3D plotting (depends on svg/pdf/rasterific) |
| `hgg-ihaskell` | ✅ | Jupyter (IHaskell) inline display |
| `hgg-custom` | ✅ | custom marks (dendrogram etc.) |
| `hgg-analyze-bridge` | ✅ | depends on Hackage `hanalyze >= 0.2` |
| `hgg-semi` | — repo only | |
| `hgg-doe` | — repo only | |
| `hgg-dataframe` | — repo only | |
| `hgg-tutorials` | — repo only | figure generators for the docs, not a library |

Dependency-closure check (library sections only): every published package
depends only on other published packages plus Hackage. `hgg-frame` is a
library dependency of svg / pdf / rasterific / latex / 3d / ihaskell /
analyze-bridge, so it must be part of the publish set. No published package
depends on the four repo-only packages.

## Upload order

`hanalyze` must be on Hackage first (`hgg-analyze-bridge` depends on it).
Then, respecting intra-repo dependencies:

1. `hgg-core`
2. `hgg-frame`
3. `hgg-svg`, `hgg-pdf`, `hgg-rasterific`, `hgg-latex`, `hgg-custom`
4. `hgg-3d`, `hgg-ihaskell`
5. `hgg-analyze-bridge`

## Procedure (per package)

```bash
cabal sdist hgg-<pkg>
cabal upload dist-newstyle/sdist/hgg-<pkg>-<ver>.tar.gz   # candidate first
# review the candidate page (haddock, README, metadata), then:
cabal upload --publish dist-newstyle/sdist/hgg-<pkg>-<ver>.tar.gz
```

Verify each sdist in a clean environment (round-trip: unpack + build + test
against the already-uploaded packages) before publishing.

## Caveats

- `hgg-tutorials` readme-image generation requires `hanalyze` built with the
  `+plot-integration` cabal flag; it is repo-only and never uploaded.
- `hgg-analyze-bridge` needs `hanalyze >= 0.2 && < 0.3` and
  `dataframe-core ^>= 1.1` available on Hackage.
