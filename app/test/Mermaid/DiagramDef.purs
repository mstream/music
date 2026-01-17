module Test.Mermaid.DiagramDef (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Map (Map)
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
import Test.Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Test.Mermaid.DiagramDef.Blocks.BlockId (spec) as BlockId
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.Spec (Spec, describe, it)
import Test.Utils (lines, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  BlockId.spec
  stringCodecTestSuite $ Map.fromFoldable
    [ unsafeDiagramDef
        { children: BlockDef.unsafeGroupBlockChildren []
        , properties: { columns: Nothing }
        , spacedOut: false
        } /\
        { linesWithIndentation:
            [ "block"
            , ""
            ]
        , linesWithoutIndentation:
            [ "block"
            , ""
            ]
        }
    , unsafeDiagramDef
        { children: BlockDef.unsafeGroupBlockChildren []
        , properties: { columns: Just C2 }
        , spacedOut: false
        } /\
        { linesWithIndentation:
            [ "block"
            , "  columns 2"
            ]
        , linesWithoutIndentation:
            [ "block"
            , "columns 2"
            ]
        }
    , unsafeDiagramDef
        { children: exampleChildren
        , properties: { columns: Nothing }
        , spacedOut: true
        } /\
        { linesWithIndentation:
            [ "block"
            , "  node1[\"node1 contents\"]"
            , "  space"
            , "  node4[\"node4 contents\"]"
            , "  space"
            , "  block:group1"
            , "    node2[\"node2 contents\"]"
            , "    node3[\"node3 contents\"]"
            , "  end"
            , "  space"
            , "  block:group2"
            , "  end"
            , "  node1 --> node2"
            , "  node3 --> node4"
            ]
        , linesWithoutIndentation:
            [ "block"
            , "node1[\"node1 contents\"]"
            , "space"
            , "node4[\"node4 contents\"]"
            , "space"
            , "block:group1"
            , "node2[\"node2 contents\"]"
            , "node3[\"node3 contents\"]"
            , "end"
            , "space"
            , "block:group2"
            , "end"
            , "node1 --> node2"
            , "node3 --> node4"
            ]
        }
    , unsafeDiagramDef
        { children: exampleChildren
        , properties: { columns: Nothing }
        , spacedOut: false
        } /\
        { linesWithIndentation:
            [ "block"
            , "  node1[\"node1 contents\"]"
            , "  node4[\"node4 contents\"]"
            , "  block:group1"
            , "    node2[\"node2 contents\"]"
            , "    node3[\"node3 contents\"]"
            , "  end"
            , "  block:group2"
            , "  end"
            , "  node1 --> node2"
            , "  node3 --> node4"
            ]
        , linesWithoutIndentation:
            [ "block"
            , "node1[\"node1 contents\"]"
            , "node4[\"node4 contents\"]"
            , "block:group1"
            , "node2[\"node2 contents\"]"
            , "node3[\"node3 contents\"]"
            , "end"
            , "block:group2"
            , "end"
            , "node1 --> node2"
            , "node3 --> node4"
            ]
        }
    ]
  orderedTestSuite
    { examples: Set.empty ∷ Set (NonEmptyArray DiagramDef)
    , name: "DiagramDef"
    }
  renderingTestSuite 25

type StringCodecTestSuiteConf = Map DiagramDef
  { linesWithIndentation ∷ Array String
  , linesWithoutIndentation ∷ Array String
  }

stringCodecTestSuite ∷ StringCodecTestSuiteConf → Spec Unit
stringCodecTestSuite expectationsByDiagram = do
  codecTestSuite
    { codec: DiagramDef.stringCodec
    , encoderOpts: true
    , examples: lines <<< (_.linesWithIndentation) <$>
        expectationsByDiagram
    , name: "Mermaid.DiagramDef/string/with indentation"
    }
  codecTestSuite
    { codec: DiagramDef.stringCodec
    , encoderOpts: false
    , examples: lines <<< (_.linesWithoutIndentation) <$>
        expectationsByDiagram
    , name: "Mermaid.DiagramDef/string/without indentation"
    }

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

exampleChildren ∷ Graph BlockId BlockDef
exampleChildren = BlockDef.unsafeGroupBlockChildren
  [ "node1" /\ (Node "node1 contents" /\ [ "node2" ])
  , "group1" /\ group1
  , "node4" /\ (Node "node4 contents" /\ [])
  , "group2" /\ group2
  ]
  where
  group1 ∷ BlockDef /\ Array String
  group1 =
    Group
      { children: BlockDef.unsafeGroupBlockChildren
          [ "node2" /\ (Node "node2 contents" /\ [])
          , "node3" /\
              (Node "node3 contents" /\ [ "node4" ])
          ]
      , properties: { columns: Nothing }
      , spacedOut: false
      } /\ []

  group2 ∷ BlockDef /\ Array String
  group2 =
    Group
      { children: BlockDef.unsafeGroupBlockChildren []
      , properties: { columns: Nothing }
      , spacedOut: false
      } /\ []

unsafeDiagramDef ∷ GroupBlock → DiagramDef
unsafeDiagramDef = DiagramDef.Blocks <<< unsafeBlocksDef

unsafeBlocksDef ∷ GroupBlock → Blocks.Def
unsafeBlocksDef groupBlock = unsafePartial case Blocks.def groupBlock of
  Right def →
    def

