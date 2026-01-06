module Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  , GroupProperties
  , genGroupBlock
  , groupBlockIds
  , columnsToString
  ) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Test.QuickCheck.Arbitrary
  ( class Arbitrary
  , arbitrary
  , genericArbitrary
  )
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (chooseInt, elements, oneOf, resize) as Gen

data BlockDef = Group GroupBlock | Node String

derive instance Eq BlockDef
derive instance Ord BlockDef

instance Show BlockDef where
  show ∷ BlockDef → String
  show (Group groupBlock) = show groupBlock
  show (Node s) = s

genBlockDef
  ∷ Int
  → Set BlockId
  → Gen (BlockDef /\ Set BlockId)
genBlockDef 0 used = do
  label ← Gen.elements $ ArrayNE.cons'
    "contents1"
    [ "contents2", "contents3" ]
  pure $ Node label /\ used
genBlockDef n used = Gen.oneOf $ ArrayNE.cons'
  (genBlockDef 0 used)
  [ do
      groupBlock /\ updated ← genGroupBlock used (n - 1)
      pure $ Group groupBlock /\ updated
  ]

type GroupBlock =
  { children ∷ Graph BlockId BlockDef
  , properties ∷ GroupProperties
  , spacedOut ∷ Boolean
  }

groupBlockIds ∷ GroupBlock → Set BlockId
groupBlockIds { children } = local `Set.union` nested
  where
  local ∷ Set BlockId
  local = Map.keys $ Graph.toMap children

  nested ∷ Set BlockId
  nested = foldl
    ( \acc → case _ of
        Group groupBlock →
          acc `Set.union` groupBlockIds groupBlock
        Node _ →
          acc
    )
    Set.empty
    (Graph.vertices children)

genGroupBlock
  ∷ Set BlockId
  → Int
  → Gen (GroupBlock /\ Set BlockId)
genGroupBlock used 0 = do
  properties ← arbitrary
  spacedOut ← arbitrary
  pure
    ( { children: Graph.fromMap Map.empty
      , properties
      , spacedOut
      } /\ used
    )
genGroupBlock used n = do
  properties ← arbitrary
  spacedOut ← arbitrary
  Gen.oneOf $ ArrayNE.cons'
    ( pure
        ( { children: Graph.fromMap Map.empty
          , properties
          , spacedOut
          } /\ used
        )
    )
    [ do
        children /\ updatedUsed ← genChildren used
        pure
          ( { children
            , properties
            , spacedOut
            } /\ updatedUsed
          )
    ]
  where
  genChildren
    ∷ Set BlockId
    → Gen (Graph BlockId BlockDef /\ Set BlockId)
  genChildren currentUsed = do
    count ← Gen.resize 3 (Gen.chooseInt 0 3)
    go count currentUsed Map.empty

  go
    ∷ Int
    → Set BlockId
    → Map BlockId (BlockDef /\ List BlockId)
    → Gen (Graph BlockId BlockDef /\ Set BlockId)
  go remaining usedIds acc = case remaining of
    0 →
      pure (Graph.fromMap acc /\ usedIds)
    _ → do
      freshId /\ nextUsed ← freshBlockId usedIds
      blockDef /\ finalUsed ← genBlockDef (n - 1) nextUsed
      go (remaining - 1) finalUsed
        (Map.insert freshId (blockDef /\ Nil) acc)

freshBlockId ∷ Set BlockId → Gen (BlockId /\ Set BlockId)
freshBlockId used = do
  candidate ← arbitrary
  if Set.member candidate used then
    freshBlockId used
  else
    pure (candidate /\ Set.insert candidate used)

type GroupProperties = { columns ∷ Maybe Columns }

data Columns = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9

derive instance Eq Columns
derive instance Generic Columns _
derive instance Ord Columns

instance Arbitrary Columns where
  arbitrary = genericArbitrary

instance Show Columns where
  show = genericShow

columnsToString ∷ Columns → String
columnsToString = case _ of
  C1 → "1"
  C2 → "2"
  C3 → "3"
  C4 → "4"
  C5 → "5"
  C6 → "6"
  C7 → "7"
  C8 → "8"
  C9 → "9"
