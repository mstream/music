module Test.Mermaid.DiagramDef (spec) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec as Codec
import Data.Graph.NonEmpty (NonEmptyGraph)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.Blocks.BlockDef (BlockDef(..))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Parsing.String (eof)
import Test.Data.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef.Unsafe
  ( unsafeGroupBlockChildren
  )
import Test.Mermaid.DiagramDef.Blocks.BlockId (spec) as BlockId
import Test.Mermaid.DiagramDef.Unsafe (unsafeBlockDiagramDef)
import Test.Spec (Spec)
import Test.Utils (lines, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  BlockId.spec
  stringCodecTestSuite $ Map.fromFoldable
    [ unsafeBlockDiagramDef
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
            , "    node5[\"node5 contents\"]"
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
            , "node5[\"node5 contents\"]"
            , "end"
            , "node1 --> node2"
            , "node3 --> node4"
            ]
        }
    , unsafeBlockDiagramDef
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
            , "    node5[\"node5 contents\"]"
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
            , "node5[\"node5 contents\"]"
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

type StringCodecTestSuiteConf = Map DiagramDef
  { linesWithIndentation ∷ Array String
  , linesWithoutIndentation ∷ Array String
  }

stringCodecTestSuite ∷ StringCodecTestSuiteConf → Spec Unit
stringCodecTestSuite expectationsByDiagram = do
  codecTestSuite
    { codec: Codec.codec
        (Codec.decoder DiagramDef.stringCodec <* eof)
        (Codec.encoder DiagramDef.stringCodec)
    , counterExamples: Set.empty
    , encoderOpts: true
    , examples: lines <<< (_.linesWithIndentation) <$>
        expectationsByDiagram
    , name: "Mermaid.DiagramDef/string/with indentation"
    }
  codecTestSuite
    { codec: Codec.codec
        (Codec.decoder DiagramDef.stringCodec <* eof)
        (Codec.encoder DiagramDef.stringCodec)
    , counterExamples: Set.empty
    , encoderOpts: false
    , examples: lines <<< (_.linesWithoutIndentation) <$>
        expectationsByDiagram
    , name: "Mermaid.DiagramDef/string/without indentation"
    }

exampleChildren ∷ NonEmptyGraph BlockId BlockDef
exampleChildren = unsafeGroupBlockChildren
  [ "node1" /\ (Node "node1 contents" /\ [ "node2" ])
  , "group1" /\ group1
  , "node4" /\ (Node "node4 contents" /\ [])
  , "group2" /\ group2
  ]
  where
  group1 ∷ BlockDef /\ Array String
  group1 =
    Group
      { children: unsafeGroupBlockChildren
          [ "node2" /\ (Node "node2 contents" /\ [])
          , "node3" /\
              (Node "node3 contents" /\ [ "node4" ])
          ]
      , properties: { columns: Nothing }
      , spacedOut: false
      }
      /\ []

  group2 ∷ BlockDef /\ Array String
  group2 =
    Group
      { children: unsafeGroupBlockChildren
          [ "node5" /\ (Node "node5 contents" /\ []) ]
      , properties: { columns: Nothing }
      , spacedOut: false
      }
      /\ []
