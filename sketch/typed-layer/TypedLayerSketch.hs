-- | A small sketch of a typed wrapper over hgg's untyped 'Layer', exploring
-- whether compile-time column checking against a dataframe schema can coexist
-- with the ordinary 'Semigroup' composition.
--
-- The idea: index 'TypedLayer' by the /ambient/ dataframe schema (a phantom),
-- not by the layer's required columns. The requirements then live in the
-- constraint context, where GHC accumulates and dedups them on @<>@ for free,
-- so @TypedLayer s@ stays a plain 'Semigroup' at every fixed @s@.
--
-- Run from the repo root:
--
-- > printf ':l sketch/typed-layer/TypedLayerSketch.hs\nmain\n' \
-- >   | cabal repl hgg-dataframe -v0
--
-- (See README.md next to this file for the observed repl transcript,
-- including the error messages of the two failing cases.)
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- HasColumn constraints on the typed constructors are specification-only
-- (unused by the bodies), which -Wredundant-constraints would flag:
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Main where

import           Data.Kind                    (Constraint, Type)
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import           GHC.TypeLits                 (ErrorMessage (..), KnownSymbol,
                                               Symbol, TypeError, symbolVal)

import           Graphics.Hgg.DataFrame       (dfResolver)
import           Graphics.Hgg.Layout          (computeLayout)
import           Graphics.Hgg.Render          (renderToPrimitives)
import           Graphics.Hgg.Spec            (ColRef (..), Layer, colorBy,
                                               layer, scatter)
import qualified DataFrame.Internal.Column    as DF
import qualified DataFrame.Internal.DataFrame as DF

-- ===========================================================================
-- The core of the idea: a schema kind + HasColumn as a closed family
-- with custom TypeErrors.
--
-- The kind @[(Symbol, Type)]@ is a placeholder here — in a real integration
-- it would follow whatever representation the typed dataframe already uses.
-- ===========================================================================

type family HasColumn (n :: Symbol) (a :: Type) (s :: [(Symbol, Type)])
    :: Constraint where
  HasColumn n a ('(n, a) ': rest) = ()
  HasColumn n b ('(n, a) ': rest) =
    TypeError ('Text "Column \"" ':<>: 'Text n ':<>: 'Text "\" has type "
               ':<>: 'ShowType a ':<>: 'Text ", but the layer needs "
               ':<>: 'ShowType b)
  HasColumn n a (other ': rest) = HasColumn n a rest
  HasColumn n a '[] =
    TypeError ('Text "Schema has no column \"" ':<>: 'Text n
               ':<>: 'Text "\" :: " ':<>: 'ShowType a)

-- The schema is only a phantom, so Semigroup/Monoid can be delegated to the
-- untyped Layer unchanged.
newtype TypedLayer (s :: [(Symbol, Type)]) = TypedLayer Layer
  deriving newtype (Semigroup, Monoid)

colName :: forall n. KnownSymbol n => ColRef
colName = ColByName (T.pack (symbolVal (Proxy @n)))

scatterT :: forall x y s. ( HasColumn x Double s, HasColumn y Double s
                          , KnownSymbol x, KnownSymbol y )
         => TypedLayer s
scatterT = TypedLayer (scatter (colName @x) (colName @y))

colorByT :: forall n s. (HasColumn n T.Text s, KnownSymbol n) => TypedLayer s
colorByT = TypedLayer (colorBy (colName @n))

-- ===========================================================================
-- Composition: the requirements of both sides accumulate in the inferred
-- constraint context. GHC accepting this signature is the check; leaving the
-- signature off in ghci (NoMonomorphismRestriction) shows the same context
-- inferred:
--
--   plotT :: (HasColumn "x" Double s, HasColumn "y" Double s,
--             HasColumn "grp" T.Text s) => TypedLayer s
-- ===========================================================================

plotT :: forall s. ( HasColumn "x" Double s, HasColumn "y" Double s
                   , HasColumn "grp" T.Text s )
      => TypedLayer s
plotT = scatterT @"x" @"y" <> colorByT @"grp"

-- ===========================================================================
-- Pinning the schema: rendering end-to-end through the existing resolver.
-- TDF only mimics the interface of a typed dataframe (the annotation is
-- matched to fromNamedColumns by hand) — the real one would live outside hgg.
-- ===========================================================================

newtype TDF (s :: [(Symbol, Type)]) = TDF DF.DataFrame

renderTypedCount :: TDF s -> TypedLayer s -> Int
renderTypedCount (TDF df) (TypedLayer ly) =
  let spec = layer ly
      r    = dfResolver df
  in length (renderToPrimitives r (computeLayout r spec) spec)

type MpgSchema = '[ '("x", Double), '("y", Double), '("grp", T.Text) ]

sampleDF :: TDF MpgSchema
sampleDF = TDF (DF.fromNamedColumns
  [ ("x",   DF.fromList ([1.0, 2.0, 3.0, 4.0] :: [Double]))
  , ("y",   DF.fromList ([2.0, 4.0, 1.0, 3.0] :: [Double]))
  , ("grp", DF.fromList (["a", "a", "b", "b"] :: [T.Text]))
  ])

-- A different schema with an extra column and different order: HasColumn is
-- just membership, so the same polymorphic plotT applies.
type WideSchema = '[ '("extra", Int), '("grp", T.Text)
                   , '("y", Double), '("x", Double) ]

sampleWideDF :: TDF WideSchema
sampleWideDF = TDF (DF.fromNamedColumns
  [ ("extra", DF.fromList ([9, 8, 7] :: [Int]))
  , ("grp",   DF.fromList (["a", "b", "a"] :: [T.Text]))
  , ("y",     DF.fromList ([1.0, 2.0, 3.0] :: [Double]))
  , ("x",     DF.fromList ([3.0, 1.0, 2.0] :: [Double]))
  ])

main :: IO ()
main = do
  putStrLn ("mpg  schema primitives = " <> show (renderTypedCount sampleDF plotT))
  putStrLn ("wide schema primitives = " <> show (renderTypedCount sampleWideDF plotT))
  putStrLn "done"
