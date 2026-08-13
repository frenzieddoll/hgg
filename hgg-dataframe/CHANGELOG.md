# Changelog for `hgg-dataframe`

## 0.2.0.0 — 2026-08-13

- DataFrame column access via `columnAsVector`; guard for empty DataFrames
  (external PR #1).
- Bilingual (English / Japanese) haddock.

## 0.1.0.0 — 2026-08-05

First public release on Hackage.

- `PlotData` instance for the Hackage `dataframe` package's `DataFrame`,
  so `df |>> spec` works directly on a `DataFrame` read by `readCsv`.
