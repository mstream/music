module Mermaid.DiagramDef.Blocks (Def(..)) where

import Prelude

import Data.Graph (Graph)
import Data.Graph as Graph
import Gen as Gen
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Def = Def (Graph BlockId String)

instance Arbitrary Def where
  arbitrary = do
    nodes ← Gen.arbitraryMap
    pure $ Def $ Graph.fromMap nodes

instance Eq Def where
  eq (Def graph1) (Def graph2) = eq
    (Graph.toMap graph1)
    (Graph.toMap graph2)

instance Ord Def where
  compare (Def graph1) (Def graph2) = compare
    (Graph.toMap graph1)
    (Graph.toMap graph2)

instance Show Def where
  show (Def graph) = show $ Graph.toMap graph

