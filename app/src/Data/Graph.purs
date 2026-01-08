module Data.Graph
  ( Graph
  , Edge
  , unfoldGraph
  , fromMap
  , toMap
  , vertices
  , edges
  , empty
  , lookup
  , outEdges
  , topologicalSort
  ) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Bifunctor (lmap)
import Data.CatList (CatList)
import Data.CatList as CL
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (arrayOf, elements, listOf) as Gen

newtype Graph k v = Graph (Map k (Tuple v (List k)))

derive newtype instance (Eq k, Eq v) ⇒ Eq (Graph k v)
derive newtype instance (Ord k, Ord v) ⇒ Ord (Graph k v)
derive newtype instance (Show k, Show v) ⇒ Show (Graph k v)

instance (Arbitrary k, Arbitrary v, Ord k) ⇒ Arbitrary (Graph k v) where
  arbitrary ∷ Gen (Graph k v)
  arbitrary = gen arbitrary arbitrary

instance Functor (Graph k) where
  map ∷ ∀ a b. (a → b) → Graph k a → Graph k b
  map f (Graph m) = Graph (map (lmap f) m)

instance Foldable (Graph k) where
  foldl ∷ ∀ a b. (b → a → b) → b → Graph k a → b
  foldl f z (Graph m) = foldl (\acc (k /\ _) → f acc k) z (Map.values m)
  foldr ∷ ∀ a b. (a → b → b) → b → Graph k a → b
  foldr f z (Graph m) = foldr (\(k /\ _) acc → f k acc) z (Map.values m)
  foldMap ∷ ∀ a m. Monoid m ⇒ (a → m) → Graph k a → m
  foldMap f (Graph m) = foldMap (f <<< fst) (Map.values m)

instance Traversable (Graph k) where
  traverse
    ∷ ∀ a b m. Applicative m ⇒ (a → m b) → Graph k a → m (Graph k b)
  traverse f (Graph m) = Graph
    <$> (traverse (\(v /\ ks) → (_ /\ ks) <$> (f v)) m)
  sequence ∷ ∀ a m. Applicative m ⇒ Graph k (m a) → m (Graph k a)
  sequence = traverse identity

gen ∷ ∀ k v. Ord k ⇒ Gen k → Gen v → Gen (Graph k v)
gen genKey genValue = do
  keys ← List.fromFoldable <$> Gen.arrayOf genKey
  values ← Gen.listOf (List.length keys) genValue
  let
    genEntries ∷ Gen (List (k /\ (v /\ List k)))
    genEntries = case ArrayNE.fromFoldable keys of
      Just nonEmptyKeys → do
        edgeEnds ← Gen.listOf
          (ArrayNE.length nonEmptyKeys)
          ( List.fromFoldable <$>
              (Gen.arrayOf $ Gen.elements nonEmptyKeys)
          )
        pure $ List.zip keys (List.zip values edgeEnds)
      Nothing →
        pure Nil
  fromMap <<< Map.fromFoldable <$> genEntries

type Edge k = { start ∷ k, end ∷ k }

unfoldGraph
  ∷ ∀ f k v out
  . Ord k
  ⇒ Functor f
  ⇒ Foldable f
  ⇒ Foldable out
  ⇒ f k
  → (k → v)
  → (k → out k)
  → Graph k v
unfoldGraph ks label theEdges =
  Graph
    ( Map.fromFoldable
        ( map
            ( \k →
                Tuple k
                  (Tuple (label k) (List.fromFoldable (theEdges k)))
            )
            ks
        )
    )

empty ∷ ∀ k v. Graph k v
empty = Graph Map.empty

fromMap ∷ ∀ k v. Map k (Tuple v (List k)) → Graph k v
fromMap = Graph

toMap ∷ ∀ k v. Graph k v → Map k (Tuple v (List k))
toMap (Graph g) = g

vertices ∷ ∀ k v. Graph k v → List v
vertices (Graph g) = map fst (Map.values g)

edges ∷ ∀ k v. Graph k v → List (Edge k)
edges (Graph g) = foldlWithIndex edges' Nil g
  where
  edges' ∷ k → List (Edge k) → Tuple v (List k) → List (Edge k)
  edges' src acc (_ /\ dests) =
    foldl (mkEdge src) acc dests

  mkEdge ∷ k → List (Edge k) → k → List (Edge k)
  mkEdge src acc dest = { start: src, end: dest } : acc

lookup ∷ ∀ k v. Ord k ⇒ k → Graph k v → Maybe v
lookup k (Graph g) = map fst (Map.lookup k g)

outEdges ∷ ∀ k v. Ord k ⇒ k → Graph k v → Maybe (List k)
outEdges k (Graph g) = map snd (Map.lookup k g)

type SortState k v =
  { unvisited ∷ Map k (Tuple v (List k))
  , result ∷ List k
  }

data SortStep a = Emit a | Visit a

topologicalSort ∷ ∀ k v. Ord k ⇒ Graph k v → List k
topologicalSort (Graph g) =
  go initialState
  where
  go ∷ SortState k v → List k
  go state@{ unvisited, result } =
    case Map.findMin unvisited of
      Just { key } → go (visit state (CL.fromFoldable [ Visit key ]))
      Nothing → result

  visit ∷ SortState k v → CatList (SortStep k) → SortState k v
  visit state stack =
    case CL.uncons stack of
      Nothing → state
      Just (Tuple (Emit k) ks) →
        let
          state' =
            { result: Cons k state.result
            , unvisited: state.unvisited
            }
        in
          visit state' ks
      Just (Tuple (Visit k) ks)
        | k `Map.member` state.unvisited →
            let
              start ∷ SortState k v
              start =
                { result: state.result
                , unvisited: Map.delete k state.unvisited
                }

              next ∷ List k
              next = maybe mempty snd (Map.lookup k g)
            in
              visit start
                ( CL.fromFoldable (map Visit next) <> CL.cons (Emit k)
                    ks
                )
        | otherwise → visit state ks

  initialState ∷ SortState k v
  initialState =
    { unvisited: g
    , result: Nil
    }
