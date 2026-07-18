# Releasing hgg to Hackage

This monorepo contains 15 packages, but **only a subset is published to
Hackage**. The rest stay repository-only (buildable from source via
`cabal.project`, not uploaded).

## Publish set

| Package | Hackage | Notes |
|---|---|---|
| `hgg` | ✅ | umbrella: exact-pins core/frame/svg, one re-export module `Graphics.Hgg`; flags `pdf`/`png`/`latex`/`3d` |
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
6. `hgg` (umbrella — always last; it exact-pins `hgg-core`/`hgg-frame`/`hgg-svg`
   to the release version, and its optional flags reference `hgg-pdf`/
   `hgg-rasterific`/`hgg-latex`/`hgg-3d`, so all of those must be up first.
   On every release bump, update the `== x.y.z.w` pins in `hgg/hgg.cabal`.)

## Per-release: bump the pinned URLs in hgg/README.md

The umbrella README (`hgg/README.md`) embeds figures and doc links as
absolute URLs pinned to the release tag (raw.githubusercontent /
github.com blob), because Hackage cannot resolve relative paths. On every
release, replace the old tag with the new one before building the sdist:

```bash
sed -i 's|/hgg/v0\.1\.0\.0/|/hgg/vX.Y.Z.W/|g' hgg/README.md
```

then verify every URL still resolves (all must print nothing):

```bash
grep -o 'https://[^")<> ]*' hgg/README.md | sed 's/#.*//' | sort -u | \
  while read u; do [ "$(curl -s -o /dev/null -w '%{http_code}' "$u")" != 200 ] && echo "FAIL $u"; done
```

(The tag must already point at a commit that contains the referenced
`docs/` files — push the tag before uploading so Hackage renders images
immediately.)

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
