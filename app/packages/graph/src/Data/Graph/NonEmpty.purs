module Data.Graph.NonEmpty
  ( NonEmptyGraph
  , edges
  , make
  , singleton
  , toGraph
  , toMap
  , uncons
  , vertices
  ) where

import Prelude

import Data.Graph (Edge, Graph)
import Data.Graph as Graph
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as ListNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

data NonEmptyGraph k v = NonEmptyGraph k (v /\ Set k) (Graph k v)

derive instance (Eq k, Eq v) ⇒ Eq (NonEmptyGraph k v)
derive instance (Ord k, Ord v) ⇒ Ord (NonEmptyGraph k v)

instance
  ( Arbitrary k
  , Arbitrary v
  , Ord k
  ) ⇒
  Arbitrary (NonEmptyGraph k v) where
  arbitrary ∷ Gen (NonEmptyGraph k v)
  arbitrary = do
    firstKey ← arbitrary
    firstValue ← arbitrary
    firstConnectionsEnds ← Set.fromFoldable <$> Gen.arrayOf arbitrary
    graph ← arbitrary
    pure $ make firstKey (firstValue /\ firstConnectionsEnds) graph

instance (Show k, Show v) ⇒ Show (NonEmptyGraph k v) where
  show (NonEmptyGraph key value graph) =
    "NonEmptyGraph " <> show key <> " " <> show value <> " " <> show
      graph

make ∷ ∀ k v. k → v /\ Set k → Graph k v → NonEmptyGraph k v
make = NonEmptyGraph

uncons
  ∷ ∀ k v
  . NonEmptyGraph k v
  → { head ∷ k /\ (v /\ Set k), tail ∷ Graph k v }
uncons (NonEmptyGraph firstKey firstEntry graph) =
  { head: firstKey /\ firstEntry, tail: graph }

singleton ∷ ∀ k v. k → v /\ Set k → NonEmptyGraph k v
singleton key entry = make key entry Graph.empty

toGraph ∷ ∀ k v. Ord k ⇒ NonEmptyGraph k v → Graph k v
toGraph (NonEmptyGraph key value graph) =
  Graph.fromMap $ Map.insert key value (Graph.toMap graph)

toMap ∷ ∀ k v. Ord k ⇒ NonEmptyGraph k v → Map k (v /\ Set k)
toMap = Graph.toMap <<< toGraph

vertices ∷ ∀ k v. NonEmptyGraph k v → NonEmptyList v
vertices (NonEmptyGraph _ entry graph) =
  ListNE.cons' (Tuple.fst entry) (Graph.vertices graph)

edges ∷ ∀ k v. Ord k ⇒ NonEmptyGraph k v → Set (Edge k)
edges = Graph.edges <<< toGraph
