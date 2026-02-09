module Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  , GroupProperties
  , NodeBlock
  , Shape(..)
  , genGroupBlock
  , groupBlockIds
  , columnsToString
  ) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Graph.NonEmpty (NonEmptyGraph)
import Data.Graph.NonEmpty as GraphNE
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as SetNE
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Gen (genSet, genUnique) as Gen
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Test.QuickCheck.Arbitrary
  ( class Arbitrary
  , arbitrary
  , genericArbitrary
  )
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (elements, oneOf) as Gen

data BlockDef = Group GroupBlock | Node NodeBlock

type NodeBlock =
  { contents ∷ String
  , shape ∷ Shape
  }

data Shape = Rectangle | Circle

derive instance Eq Shape
derive instance Generic Shape _
derive instance Ord Shape

instance Arbitrary Shape where
  arbitrary = genericArbitrary

instance Show Shape where
  show = genericShow

type GroupBlock =
  { children ∷ NonEmptyGraph BlockId BlockDef
  , properties ∷ GroupProperties
  , spacedOut ∷ Boolean
  }

derive instance Eq BlockDef
derive instance Generic BlockDef _
derive instance Ord BlockDef

instance Show BlockDef where
  show x = genericShow x

genBlockDef
  ∷ Int
  → Set BlockId
  → Gen (BlockDef /\ Set BlockId)
genBlockDef 0 _ = do
  node ← genNode
  pure $ node /\ Set.empty
genBlockDef n existingIds = Gen.oneOf $ ArrayNE.cons'
  (genBlockDef 0 existingIds)
  [ do
      group /\ groupInnerIds ← genGroup (n - 1) existingIds
      pure $ group /\ (Set.fromFoldable groupInnerIds)
  ]

groupBlockIds ∷ GroupBlock → Set BlockId
groupBlockIds { children } = local `Set.union` nested
  where
  local ∷ Set BlockId
  local = Map.keys $ GraphNE.toMap children

  nested ∷ Set BlockId
  nested = foldl
    ( \acc → case _ of
        Group groupBlock →
          acc `Set.union` groupBlockIds groupBlock
        Node _ →
          acc
    )
    Set.empty
    (GraphNE.vertices children)

genNode ∷ Gen BlockDef
genNode = do
  contents ← genLabel
  shape ← arbitrary
  pure $ Node { contents, shape }

genGroup ∷ Int → Set BlockId → Gen (BlockDef /\ NonEmptySet BlockId)
genGroup n existingIds = do
  groupBlock /\ groupBlockInnerIds ← genGroupBlock n existingIds
  pure $ Group groupBlock /\ groupBlockInnerIds

genGroupBlock
  ∷ Int → Set BlockId → Gen (GroupBlock /\ NonEmptySet BlockId)
genGroupBlock n existingIds = do
  properties ← arbitrary
  spacedOut ← arbitrary
  children /\ childrenInnerIds ← genChildren
  pure $ { children, properties, spacedOut } /\ childrenInnerIds
  where
  genChildren
    ∷ Gen (NonEmptyGraph BlockId BlockDef /\ NonEmptySet BlockId)
  genChildren = do
    firstChildId ← genBlockId existingIds
    firstChild /\ childInternalIds ← genBlockDef (n - 1) existingIds
    firstChildConnectionEnds ← genConnectionEnds existingIds
    pure $
      ( GraphNE.singleton firstChildId
          (firstChild /\ firstChildConnectionEnds)
      ) /\
        (SetNE.cons firstChildId childInternalIds)

genLabel ∷ Gen String
genLabel = Gen.elements $ ArrayNE.cons'
  "contents1"
  [ "contents2", "contents3" ]

genBlockId ∷ Set BlockId → Gen BlockId
genBlockId = Gen.genUnique

genConnectionEnds ∷ Set BlockId → Gen (Set BlockId)
genConnectionEnds = SetNE.fromSet >>> case _ of
  Just nonEmptyExistingIds →
    let
      genExistingId ∷ Gen BlockId
      genExistingId = Gen.elements
        $ ArrayNE.fromFoldable1 nonEmptyExistingIds
    in
      Gen.genSet genExistingId

  Nothing →
    pure Set.empty

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
