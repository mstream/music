module Test.Mermaid.DiagramDef (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence_)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid as Mermaid
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Partial.Unsafe (unsafePartial)
import Random.LCG (mkSeed)
import Test.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockId (spec) as BlockId
import Test.Mermaid.DiagramDef.Blocks.BlockId (unsafeBlockId)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.Spec (Spec, describe, it)
import Test.Utils (lines, orderTestSuite)

spec ∷ Spec Unit
spec = do
  BlockId.spec
  codecTestSuite
    { codec: DiagramDef.stringCodec
    , encoderOpts: true
    , examples: Map.fromFoldable
        [ parsedBlockDiagramDefExampleOfEmptyDiagram /\ lines
            [ "block"
            , ""
            ]
        , parsedBlockDiagramDefExampleOfDiagramWithTwoColumns /\ lines
            [ "block"
            , "  columns 2"
            ]
        , parsedBlockDiagramDefExampleOfDiagramWithSpacing /\ lines
            [ "block"
            , "  blockNode1[\"blockNode1 contents\"]"
            , "  space"
            , "  blockNode4[\"blockNode4 contents\"]"
            , "  space"
            , "  block:groupNode1"
            , "    blockNode2[\"blockNode2 contents\"]"
            , "    blockNode3[\"blockNode3 contents\"]"
            , "  end"
            , "  blockNode1 --> blockNode2"
            , "  blockNode3 --> blockNode4"
            ]
        , parsedBlockDiagramDefExampleOfDiagramWithoutSpacing /\ lines
            [ "block"
            , "  blockNode1[\"blockNode1 contents\"]"
            , "  blockNode4[\"blockNode4 contents\"]"
            , "  block:groupNode1"
            , "    blockNode2[\"blockNode2 contents\"]"
            , "    blockNode3[\"blockNode3 contents\"]"
            , "  end"
            , "  blockNode1 --> blockNode2"
            , "  blockNode3 --> blockNode4"
            ]
        ]
    , name: "Mermaid.DiagramDef/string/with indentation"
    }
  codecTestSuite
    { codec: DiagramDef.stringCodec
    , encoderOpts: false
    , examples: Map.fromFoldable
        [ parsedBlockDiagramDefExampleOfEmptyDiagram /\ lines
            [ "block"
            , ""
            ]
        , parsedBlockDiagramDefExampleOfDiagramWithTwoColumns /\ lines
            [ "block"
            , "columns 2"
            ]
        , parsedBlockDiagramDefExampleOfDiagramWithSpacing /\ lines
            [ "block"
            , "blockNode1[\"blockNode1 contents\"]"
            , "space"
            , "blockNode4[\"blockNode4 contents\"]"
            , "space"
            , "block:groupNode1"
            , "blockNode2[\"blockNode2 contents\"]"
            , "blockNode3[\"blockNode3 contents\"]"
            , "end"
            , "blockNode1 --> blockNode2"
            , "blockNode3 --> blockNode4"
            ]
        , parsedBlockDiagramDefExampleOfDiagramWithoutSpacing /\ lines
            [ "block"
            , "blockNode1[\"blockNode1 contents\"]"
            , "blockNode4[\"blockNode4 contents\"]"
            , "block:groupNode1"
            , "blockNode2[\"blockNode2 contents\"]"
            , "blockNode3[\"blockNode3 contents\"]"
            , "end"
            , "blockNode1 --> blockNode2"
            , "blockNode3 --> blockNode4"
            ]
        ]
    , name: "Mermaid.DiagramDef/string/without indentation"
    }
  orderTestSuite
    { examples: Set.empty ∷ Set (Array DiagramDef), name: "DiagramDef" }
  renderingTestSuite 20

renderingTestSuite ∷ Int → Spec Unit
renderingTestSuite quantity = describe
  "mermaid diagram rendering"
  (sequence_ testCases)
  where
  testCases ∷ Array (Spec Unit)
  testCases = Array.mapWithIndex testCase testInputs

  testInputs ∷ Array DiagramDef
  testInputs = Gen.evalGen
    (Gen.vectorOf quantity arbitrary)
    { newSeed: mkSeed 123, size: 10 }

  testCase ∷ Int → DiagramDef → Spec Unit
  testCase index diagramDef = it
    ("diagram " <> show index)
    (void $ Mermaid.render diagramDef)

parsedBlockDiagramDefExampleOfEmptyDiagram ∷ DiagramDef
parsedBlockDiagramDefExampleOfEmptyDiagram =
  unsafeParsedBlockDiagramDefExample
    { children: Graph.empty
    , properties: { columns: Nothing }
    , spacedOut: false
    }

parsedBlockDiagramDefExampleOfDiagramWithTwoColumns ∷ DiagramDef
parsedBlockDiagramDefExampleOfDiagramWithTwoColumns =
  unsafeParsedBlockDiagramDefExample
    { children: Graph.empty
    , properties: { columns: Just C2 }
    , spacedOut: false
    }

parsedBlockDiagramDefExampleOfDiagramWithSpacing ∷ DiagramDef
parsedBlockDiagramDefExampleOfDiagramWithSpacing =
  unsafeParsedBlockDiagramDefExample
    { children: exampleChildren
    , properties: { columns: Nothing }
    , spacedOut: true
    }

parsedBlockDiagramDefExampleOfDiagramWithoutSpacing ∷ DiagramDef
parsedBlockDiagramDefExampleOfDiagramWithoutSpacing =
  unsafeParsedBlockDiagramDefExample
    { children: exampleChildren
    , properties: { columns: Nothing }
    , spacedOut: false
    }

unsafeParsedBlockDiagramDefExample ∷ GroupBlock → DiagramDef
unsafeParsedBlockDiagramDefExample =
  DiagramDef.Blocks <<< unsafeBlocksDef

exampleChildren ∷ Graph BlockId BlockDef
exampleChildren = Graph.fromMap $ Map.fromFoldable
  [ nodeId1 /\ node1
  , groupId1 /\ group1
  , nodeId4 /\ node4
  ]
  where
  group1 ∷ BlockDef /\ List BlockId
  group1 =
    Group
      { children: Graph.fromMap $ Map.fromFoldable
          [ nodeId2 /\ node2
          , nodeId3 /\ node3
          ]
      , properties: { columns: Nothing }
      , spacedOut: false
      } /\ Nil

  node1 ∷ BlockDef /\ List BlockId
  node1 = Node "blockNode1 contents" /\ nodeId2 : Nil

  node2 ∷ BlockDef /\ List BlockId
  node2 = Node "blockNode2 contents" /\ Nil

  node3 ∷ BlockDef /\ List BlockId
  node3 = Node "blockNode3 contents" /\ nodeId4 : Nil

  node4 ∷ BlockDef /\ List BlockId
  node4 = Node "blockNode4 contents" /\ Nil

  groupId1 ∷ BlockId
  groupId1 = unsafeBlockId "groupNode1"

  nodeId1 ∷ BlockId
  nodeId1 = unsafeBlockId "blockNode1"

  nodeId2 ∷ BlockId
  nodeId2 = unsafeBlockId "blockNode2"

  nodeId3 ∷ BlockId
  nodeId3 = unsafeBlockId "blockNode3"

  nodeId4 ∷ BlockId
  nodeId4 = unsafeBlockId "blockNode4"

unsafeBlocksDef ∷ GroupBlock → Blocks.Def
unsafeBlocksDef groupBlock = unsafePartial case Blocks.def groupBlock of
  Right def →
    def

