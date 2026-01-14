module Test.Mermaid.DiagramDef.Blocks.BlockDef
  ( unsafeGroupBlockChildren
  ) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef (BlockDef)
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Partial.Unsafe (unsafeCrashWith)
import Test.Mermaid.DiagramDef.Blocks.BlockId (unsafeBlockId) as BlockId

unsafeGroupBlockChildren
  ∷ Array (String /\ (BlockDef /\ Array String))
  → Graph BlockId BlockDef
unsafeGroupBlockChildren blockEntries =
  if Array.length blockEntries == Map.size blocksById then
    Graph.fromMap blocksById
  else unsafeCrashWith $
    "there are some duplicate identifiers in the block entries:"
      <> "\n  input entries: "
      <> show blockEntries
      <> "\n  output blocks: "
      <> show blocksById

  where
  blocksById ∷ Map BlockId (BlockDef /\ List BlockId)
  blocksById = foldlWithIndex f Map.empty
    (Map.fromFoldable blockEntries)

  f
    ∷ String
    → Map BlockId (BlockDef /\ List BlockId)
    → (BlockDef /\ Array String)
    → Map BlockId (BlockDef /\ List BlockId)
  f idStr acc (blockDef /\ connectionEnds) = Map.insert
    (BlockId.unsafeBlockId idStr)
    ( blockDef /\
        (List.fromFoldable $ BlockId.unsafeBlockId <$> connectionEnds)
    )
    acc

