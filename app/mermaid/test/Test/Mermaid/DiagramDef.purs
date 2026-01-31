module Test.Mermaid.DiagramDef (spec) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Foldable (fold)
import Data.Graph.NonEmpty (NonEmptyGraph)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (sequence_)
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.Blocks.BlockDef (BlockDef(..), Shape(..))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId (stringCodec) as BlockId
import Parsing.String (eof)
import Random.LCG (mkSeed)
import Test.Data.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef.Unsafe
  ( unsafeGroup
  , unsafeGroupBlockChildren
  )
import Test.Mermaid.DiagramDef.Blocks.BlockId (spec) as BlockId
import Test.Mermaid.DiagramDef.Unsafe (unsafeBlockDiagramDef)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (elements, evalGen, vectorOf) as Gen
import Test.Spec (Spec)
import Test.Utils (lines, orderedTestSuite, unsafeGenSorted7)

spec ∷ Spec Unit
spec = do
  BlockId.spec
  stringCodecSpec
  orderedTestSuite
    { examples: Set.empty ∷ Set (NonEmptyArray DiagramDef)
    , name: "DiagramDef"
    }

stringCodecSpec ∷ Spec Unit
stringCodecSpec = stringCodecTestSuite 5 do
  id1 /\ id2 /\ id3 /\ id4 /\ id5 /\ id6 /\ id7 ← unsafeGenSorted7
  node1Contents ← genContents
  node2Contents ← genContents
  node3Contents ← genContents
  node4Contents ← genContents
  node5Contents ← genContents
  let
    id1RectNode = renderBlockId id1
    id2RectNode = renderBlockId id2
    id3CircNode = renderBlockId id3
    id4CircNode = renderBlockId id4
    id5CircNode = renderBlockId id5
    id6Group1 = renderBlockId id6
    id7Group2 = renderBlockId id7

    children ∷ NonEmptyGraph BlockId BlockDef
    children = unsafeGroupBlockChildren
      [ id1RectNode /\
          ( Node { contents: node1Contents, shape: Rectangle } /\
              [ id2RectNode ]
          )
      , id6Group1
          /\
            unsafeGroup
              { children:
                  [ id2RectNode /\
                      ( Node
                          { contents: node2Contents
                          , shape: Rectangle
                          }
                          /\ []
                      )
                  , id3CircNode /\
                      ( Node
                          { contents: node3Contents
                          , shape: Circle
                          }
                          /\ [ id4CircNode ]
                      )
                  ]
              , properties: { columns: Nothing }
              , spacedOut: false
              }
          /\ []

      , id4CircNode /\
          (Node { contents: node4Contents, shape: Circle } /\ [])
      , id7Group2
          /\ unsafeGroup
            { children:
                [ id5CircNode /\
                    ( Node { contents: node5Contents, shape: Circle }
                        /\ []
                    )
                ]
            , properties: { columns: Nothing }
            , spacedOut: false
            }
          /\ []
      ]
  pure $ Map.fromFoldable
    [ unsafeBlockDiagramDef
        { children
        , properties: { columns: Nothing }
        , spacedOut: true
        } /\
        { linesWithIndentation: fold <$>
            [ [ "block" ]
            , [ "  ", id1RectNode, "[\"", node1Contents, "\"]" ]
            , [ "  space" ]
            , [ "  ", id4CircNode, "((\"", node4Contents, "\"))" ]
            , [ "  space" ]
            , [ "  block:", id6Group1 ]
            , [ "    ", id2RectNode, "[\"", node2Contents, "\"]" ]
            , [ "    ", id3CircNode, "((\"", node3Contents, "\"))" ]
            , [ "  end" ]
            , [ "  space" ]
            , [ "  block:", id7Group2 ]
            , [ "    ", id5CircNode, "((\"", node5Contents, "\"))" ]
            , [ "  end" ]
            , [ "  ", id1RectNode, " --> ", id2RectNode ]
            , [ "  ", id3CircNode, " --> ", id4CircNode ]
            ]
        , linesWithoutIndentation: fold <$>
            [ [ "block" ]
            , [ id1RectNode, "[\"", node1Contents, "\"]" ]
            , [ "space" ]
            , [ id4CircNode, "((\"", node4Contents, "\"))" ]
            , [ "space" ]
            , [ "block:", id6Group1 ]
            , [ id2RectNode, "[\"", node2Contents, "\"]" ]
            , [ id3CircNode, "((\"", node3Contents, "\"))" ]
            , [ "end" ]
            , [ "space" ]
            , [ "block:", id7Group2 ]
            , [ id5CircNode, "((\"", node5Contents, "\"))" ]
            , [ "end" ]
            , [ id1RectNode, " --> ", id2RectNode ]
            , [ id3CircNode, " --> ", id4CircNode ]
            ]

        }
    , unsafeBlockDiagramDef
        { children
        , properties: { columns: Nothing }
        , spacedOut: false
        } /\
        { linesWithIndentation: fold <$>
            [ [ "block" ]
            , [ "  ", id1RectNode, "[\"", node1Contents, "\"]" ]
            , [ "  ", id4CircNode, "((\"", node4Contents, "\"))" ]
            , [ "  block:", id6Group1 ]
            , [ "    ", id2RectNode, "[\"", node2Contents, "\"]" ]
            , [ "    ", id3CircNode, "((\"", node3Contents, "\"))" ]
            , [ "  end" ]
            , [ "  block:", id7Group2 ]
            , [ "    ", id5CircNode, "((\"", node5Contents, "\"))" ]
            , [ "  end" ]
            , [ "  ", id1RectNode, " --> ", id2RectNode ]
            , [ "  ", id3CircNode, " --> ", id4CircNode ]
            ]
        , linesWithoutIndentation: fold <$>
            [ [ "block" ]
            , [ id1RectNode, "[\"", node1Contents, "\"]" ]
            , [ id4CircNode, "((\"", node4Contents, "\"))" ]
            , [ "block:", id6Group1 ]
            , [ id2RectNode, "[\"", node2Contents, "\"]" ]
            , [ id3CircNode, "((\"", node3Contents, "\"))" ]
            , [ "end" ]
            , [ "block:", id7Group2 ]
            , [ id5CircNode, "((\"", node5Contents, "\"))" ]
            , [ "end" ]
            , [ id1RectNode, " --> ", id2RectNode ]
            , [ id3CircNode, " --> ", id4CircNode ]
            ]
        }
    ]

genContents ∷ Gen String
genContents = Gen.elements $ ArrayNE.cons' "some contents 1"
  [ "some contents 2", "some contents 3" ]

renderBlockId ∷ BlockId → String
renderBlockId = Codec.encoder BlockId.stringCodec unit

type StringCodecTestSuiteConf = Map DiagramDef
  { linesWithIndentation ∷ Array String
  , linesWithoutIndentation ∷ Array String
  }

stringCodecTestSuite ∷ Int → Gen StringCodecTestSuiteConf → Spec Unit
stringCodecTestSuite quantity genSuiteConf = sequence_ testSuites
  where
  testSuites ∷ Array (Spec Unit)
  testSuites = Array.mapWithIndex testSuite suiteConfs

  suiteConfs ∷ Array StringCodecTestSuiteConf
  suiteConfs = Gen.evalGen
    (Gen.vectorOf quantity genSuiteConf)
    { newSeed: mkSeed 123, size: 10 }

  testSuite ∷ Int → StringCodecTestSuiteConf → Spec Unit
  testSuite index suiteConf = do
    withIndentationTestSuite index
      ((_.linesWithIndentation) <$> suiteConf)
    withoutIndentationTestSuite index
      ((_.linesWithoutIndentation) <$> suiteConf)

  withIndentationTestSuite
    ∷ Int → Map DiagramDef (Array String) → Spec Unit
  withIndentationTestSuite index examples =
    codecTestSuite
      { codec
      , counterExamples
      , encoderOpts: true
      , examples: lines <$> examples
      , name: name "with indentation" index
      }

  withoutIndentationTestSuite
    ∷ Int → Map DiagramDef (Array String) → Spec Unit
  withoutIndentationTestSuite index examples = codecTestSuite
    { codec
    , counterExamples
    , encoderOpts: false
    , examples: lines <$> examples
    , name: name "without indentation" index
    }

  codec ∷ Codec DiagramDef String Boolean
  codec = Codec.codec
    (Codec.decoder DiagramDef.stringCodec <* eof)
    (Codec.encoder DiagramDef.stringCodec)

  counterExamples ∷ Set String
  counterExamples = Set.empty

  name ∷ String → Int → String
  name suffix index = "Mermaid.DiagramDef/string/" <> suffix <> " " <>
    show index

