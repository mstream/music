module Data.Graph.NonEmpty
  ( NonEmptyGraph
  , make
  , singleton
  , toGraph
  , toMap
  , vertices
  ) where

import Prelude

import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as ListNE
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\))

data NonEmptyGraph k v = NonEmptyGraph k (v /\ List k) (Graph k v)

make ∷ ∀ k v. k → v /\ List k → Graph k v → NonEmptyGraph k v
make = NonEmptyGraph

singleton ∷ ∀ k v. k → v /\ List k → NonEmptyGraph k v
singleton key entry = make key entry Graph.empty

toGraph ∷ ∀ k v. Ord k ⇒ NonEmptyGraph k v → Graph k v
toGraph (NonEmptyGraph key value graph) =
  Graph.fromMap $ Map.insert key value (Graph.toMap graph)

toMap ∷ ∀ k v. Ord k ⇒ NonEmptyGraph k v → Map k (v /\ List k)
toMap = Graph.toMap <<< toGraph

vertices ∷ ∀ k v. NonEmptyGraph k v → NonEmptyList v
vertices (NonEmptyGraph _ entry graph) =
  ListNE.cons' (Tuple.fst entry) (Graph.vertices graph)

derive instance (Eq k, Eq v) ⇒ Eq (NonEmptyGraph k v)
derive instance (Ord k, Ord v) ⇒ Ord (NonEmptyGraph k v)

instance (Show k, Show v) ⇒ Show (NonEmptyGraph k v) where
  show (NonEmptyGraph key value graph) =
    "NonEmptyGraph " <> show key <> " " <> show value <> " " <> show
      graph
