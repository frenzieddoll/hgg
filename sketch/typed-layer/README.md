# typed-layer sketch

A small experiment following a Discord discussion: can compile-time column
checking against a typed dataframe schema coexist with hgg's ordinary
`Semigroup` composition?

The approach tried here: index `TypedLayer` by the *ambient* dataframe schema
(a phantom) instead of the layer's *required* columns, and let the
requirements live in the constraint context. `hgg-core` is untouched — the
whole sketch is `TypedLayerSketch.hs` loaded on top of the released modules.

The schema kind used here (`[(Symbol, Type)]`) is only a placeholder; a real
integration would follow whatever representation the typed dataframe already
uses, with a small glue layer between that and `HasColumn`.

## Running it

From the repo root:

```
printf ':l sketch/typed-layer/TypedLayerSketch.hs\nmain\n' | cabal repl hgg-dataframe -v0
```

(If your repl's working directory ends up inside the package, use an absolute
path in `:l`.)

## Observed session (GHC 9.6.7)

Rendering end-to-end through the existing `dfResolver`, with the same
polymorphic `plotT` applied to two different schemas (the second has an extra
column and a different column order):

```
mpg  schema primitives = 41
wide schema primitives = 48
```

Constraint accumulation on `<>` — the inferred type of an unsigned binding
(ghci defaults to `NoMonomorphismRestriction`):

```
ghci> plotNoSig = scatterT @"x" @"y" <> colorByT @"grp"
ghci> :t plotNoSig
plotNoSig
  :: (HasColumn "x" Double s, HasColumn "y" Double s,
      HasColumn "grp" T.Text s) =>
     TypedLayer s
```

Missing column:

```
ghci> renderTypedCount sampleDF (scatterT @"x" @"nope")
<interactive>: error: [GHC-64725]
    • Schema has no column "nope" :: Double
```

Column type mismatch:

```
ghci> renderTypedCount sampleDF (colorByT @"x")
<interactive>: error: [GHC-64725]
    • Column "x" has type Double, but the layer needs T.Text
```

With `MonomorphismRestriction` on (i.e. in a module), the unsigned top-level
binding instead fails with `Could not solve: HasColumn "x" Double s0, …` — so
argument-less top-level definitions want type signatures.

## Notes / limitations

- `TDF` here only mimics the interface of a typed dataframe (the type
  annotation is matched to `fromNamedColumns` by hand); connecting to a real
  typed dataframe is exactly the glue this sketch leaves open.
- The `HasColumn` constraints on the typed constructors are
  specification-only, so `-Wredundant-constraints` flags them; the sketch
  silences that warning locally.
- The runtime resolver still runs (it re-checks what the types already
  guarantee); a typed fast path that pulls vectors straight from the frame
  could bypass it later, but isn't needed for the safety property.
