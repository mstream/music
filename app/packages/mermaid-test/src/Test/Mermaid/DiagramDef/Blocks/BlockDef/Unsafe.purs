module Test.Mermaid.DiagramDef.Blocks.BlockDef.Unsafe
  ( unsafeGroup
  , unsafeGroupBlockChildren
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph as Graph
import Data.Graph.NonEmpty (NonEmptyGraph)
import Data.Graph.NonEmpty as GraphNE
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , GroupProperties
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Partial.Unsafe (unsafeCrashWith)
import Test.Mermaid.DiagramDef.Blocks.BlockId.Unsafe (unsafeBlockId)

unsafeGroup
  ∷ { children ∷ Array (String /\ (BlockDef /\ Array String))
    , properties ∷ GroupProperties
    , spacedOut ∷ Boolean
    }
  → BlockDef
unsafeGroup { children, properties, spacedOut } =
  Group
    { children: unsafeGroupBlockChildren children
    , spacedOut
    , properties
    }

unsafeGroupBlockChildren
  ∷ Array (String /\ (BlockDef /\ Array String))
  → NonEmptyGraph BlockId BlockDef
unsafeGroupBlockChildren = Array.uncons >>> case _ of
  Just { head, tail } →
    let
      firstChildId ∷ BlockId
      firstChildId = unsafeBlockId $ Tuple.fst head

      firstChild ∷ BlockDef
      firstChild = Tuple.fst $ Tuple.snd head

      firstChildConnectionEnds ∷ Set BlockId
      firstChildConnectionEnds =
        unsafeConnectionEnds $ Tuple.snd $ Tuple.snd head

      otherChildrenById ∷ Map BlockId (BlockDef /\ Set BlockId)
      otherChildrenById = foldlWithIndex
        ( \idStr acc (blockDef /\ connectionEnds) → Map.insert
            (unsafeBlockId idStr)
            (blockDef /\ unsafeConnectionEnds connectionEnds)
            acc
        )
        Map.empty
        (Map.fromFoldable tail)
    in
      GraphNE.make firstChildId (firstChild /\ firstChildConnectionEnds)
        (Graph.fromMap otherChildrenById)
  Nothing →
    unsafeCrashWith
      "there should be at least one child in a group block"
  where
  unsafeConnectionEnds ∷ Array String → Set BlockId
  unsafeConnectionEnds = Set.fromFoldable <<< map unsafeBlockId
