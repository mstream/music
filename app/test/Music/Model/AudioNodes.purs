module Test.Music.Model.AudioNodes (spec) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph as Graph
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence_)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (stringCodec) as BlockId
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Partial.Unsafe (unsafeCrashWith)
import Random.LCG (mkSeed)
import Test.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Test.Mermaid.DiagramDef.Blocks.BlockId (unsafeBlockId) as BlockId
import Test.Music.Model.AudioNodes.AudioNode as AudioNode
import Test.Music.Model.AudioNodes.AudioNodeName as AudioNodeName
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.Spec (Spec, describe)
import Test.Utils (lines, unsafeGenSorted8)

spec ∷ Spec Unit
spec = do
  AudioNode.spec
  AudioNodeName.spec
  codecsTestSuite 2 do
    id1 /\ id2 /\ id3 /\ id4 /\ id5 /\ id6 /\ id7 /\ id8 ←
      unsafeGenSorted8
    let
      id1SeqFreqConnectedMultiple = renderAudioNodeId id1
      id2SeqFreqConnectedSingle = renderAudioNodeId id2
      id3SeqFreqDisconnected = renderAudioNodeId id3
      id4SeqGainConnectedMultiple = renderAudioNodeId id4
      id5SeqGainFreqConnectedSingle = renderAudioNodeId id5
      id6SeqGainFreqDisconnected = renderAudioNodeId id6
      id7OscSine = renderAudioNodeId id7
      id8OscSquare = renderAudioNodeId id8
    pure $ Map.fromFoldable
      [ unsafeAudioNodes [] /\
          { groupBlock:
              { children: BlockDef.unsafeGroupBlockChildren
                  [ "output" /\ (Node "output" /\ []) ]
              , properties: { columns: Just C1 }
              , spacedOut: false
              }
          , lines: [ "" ]
          }
      , unsafeAudioNodes
          [ ("def_" <> id1SeqFreqConnectedMultiple) /\
              ( AudioNode.unsafeFrequencySequencer 1
                  [ 100.0, 200.0, 300.0 ]
                  /\ [ "def_" <> id7OscSine, "def_" <> id8OscSquare ]
              )
          , ("def_" <> id2SeqFreqConnectedSingle) /\
              ( AudioNode.unsafeFrequencySequencer 1
                  [ 200.0, 300.0, 400.0 ]
                  /\ [ "def_" <> id7OscSine ]
              )
          , ("def_" <> id3SeqFreqDisconnected) /\
              ( AudioNode.unsafeFrequencySequencer 1
                  [ 300.0, 400.0, 500.0 ]
                  /\ []
              )
          , ("def_" <> id4SeqGainConnectedMultiple) /\
              ( AudioNode.unsafeGainSequencer 2 [ 0.0, 0.1 ] /\
                  [ "def_" <> id7OscSine, "def_" <> id8OscSquare ]
              )
          , ("def_" <> id5SeqGainFreqConnectedSingle) /\
              ( AudioNode.unsafeGainSequencer 2 [ 0.1, 0.2 ] /\
                  [ "def_" <> id8OscSquare ]
              )
          , ("def_" <> id6SeqGainFreqDisconnected) /\
              (AudioNode.unsafeGainSequencer 2 [ 0.2, 0.3 ] /\ [])
          , ("def_" <> id7OscSine) /\
              (Oscillator { wave: Sine } /\ [ "output" ])
          , ("def_" <> id8OscSquare) /\
              (Oscillator { wave: Square } /\ [])
          ] /\
          { groupBlock:
              { children: BlockDef.unsafeGroupBlockChildren
                  [ "oscillators" /\
                      ( Group
                          { children: BlockDef.unsafeGroupBlockChildren
                              [ ("def_" <> id7OscSine) /\
                                  ( Group
                                      { children:
                                          BlockDef.unsafeGroupBlockChildren
                                            [ ( "def_" <> id7OscSine <>
                                                  "_frequency"
                                              )
                                                /\
                                                  (Node "f" /\ [])
                                            , ( "def_" <> id7OscSine <>
                                                  "_gain"
                                              ) /\
                                                (Node "g" /\ [])
                                            , ( "def_" <> id7OscSine <>
                                                  "_wave"
                                              ) /\
                                                (Node "w=sine" /\ [])
                                            ]
                                      , properties: { columns: Just C2 }
                                      , spacedOut: false
                                      } /\ [ "output" ]
                                  )
                              , ("def_" <> id8OscSquare) /\
                                  ( Group
                                      { children:
                                          BlockDef.unsafeGroupBlockChildren
                                            [ ( "def_" <> id8OscSquare
                                                  <> "_frequency"
                                              )
                                                /\
                                                  (Node "f" /\ [])
                                            , ( "def_" <> id8OscSquare
                                                  <> "_gain"
                                              ) /\
                                                (Node "g" /\ [])
                                            , ( "def_" <> id8OscSquare
                                                  <> "_wave"
                                              ) /\
                                                (Node "w=square" /\ [])
                                            ]
                                      , properties: { columns: Just C2 }
                                      , spacedOut: false
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Nothing }
                          , spacedOut: false
                          } /\ []
                      )
                  , "sequencers" /\
                      ( Group
                          { children: BlockDef.unsafeGroupBlockChildren
                              [ ("def_" <> id1SeqFreqConnectedMultiple)
                                  /\
                                    ( Group
                                        { children:
                                            BlockDef.unsafeGroupBlockChildren
                                              [ ( "def_"
                                                    <>
                                                      id1SeqFreqConnectedMultiple
                                                    <> "_duration"
                                                )
                                                  /\
                                                    (Node "d=1" /\ [])
                                              , ( "def_"
                                                    <>
                                                      id1SeqFreqConnectedMultiple
                                                    <> "_sequence"
                                                )
                                                  /\
                                                    ( Node
                                                        "s=[100.0 200.0 300.0]"
                                                        /\ []
                                                    )
                                              ]
                                        , properties:
                                            { columns: Just C2 }
                                        , spacedOut: false
                                        } /\
                                        [ "def_" <> id7OscSine <>
                                            "_frequency"
                                        , "def_" <> id8OscSquare <>
                                            "_frequency"
                                        ]
                                    )
                              , ("def_" <> id2SeqFreqConnectedSingle) /\
                                  ( Group
                                      { children:
                                          BlockDef.unsafeGroupBlockChildren
                                            [ ( "def_"
                                                  <>
                                                    id2SeqFreqConnectedSingle
                                                  <> "_duration"
                                              )
                                                /\
                                                  (Node "d=1" /\ [])
                                            , ( "def_"
                                                  <>
                                                    id2SeqFreqConnectedSingle
                                                  <> "_sequence"
                                              )
                                                /\
                                                  ( Node
                                                      "s=[200.0 300.0 400.0]"
                                                      /\ []
                                                  )
                                            ]
                                      , properties: { columns: Just C2 }
                                      , spacedOut: false
                                      } /\
                                      [ "def_" <> id7OscSine <>
                                          "_frequency"
                                      ]
                                  )
                              , ("def_" <> id3SeqFreqDisconnected) /\
                                  ( Group
                                      { children:
                                          BlockDef.unsafeGroupBlockChildren
                                            [ ( "def_"
                                                  <>
                                                    id3SeqFreqDisconnected
                                                  <> "_duration"
                                              )
                                                /\
                                                  (Node "d=1" /\ [])
                                            , ( "def_"
                                                  <>
                                                    id3SeqFreqDisconnected
                                                  <> "_sequence"
                                              )
                                                /\
                                                  ( Node
                                                      "s=[300.0 400.0 500.0]"
                                                      /\ []
                                                  )
                                            ]
                                      , properties: { columns: Just C2 }
                                      , spacedOut: false
                                      } /\
                                      []
                                  )
                              , ("def_" <> id4SeqGainConnectedMultiple)
                                  /\
                                    ( Group
                                        { children:
                                            BlockDef.unsafeGroupBlockChildren
                                              [ ( "def_"
                                                    <>
                                                      id4SeqGainConnectedMultiple
                                                    <> "_duration"
                                                )
                                                  /\
                                                    (Node "d=2" /\ [])
                                              , ( "def_"
                                                    <>
                                                      id4SeqGainConnectedMultiple
                                                    <> "_sequence"
                                                )
                                                  /\
                                                    ( Node "s=[0.0 0.1]"
                                                        /\
                                                          []
                                                    )
                                              ]
                                        , properties:
                                            { columns: Just C2 }
                                        , spacedOut: false
                                        } /\
                                        [ "def_" <> id7OscSine <>
                                            "_gain"
                                        , "def_" <> id8OscSquare <>
                                            "_gain"
                                        ]
                                    )
                              , ( "def_" <>
                                    id5SeqGainFreqConnectedSingle
                                ) /\
                                  ( Group
                                      { children:
                                          BlockDef.unsafeGroupBlockChildren
                                            [ ( "def_"
                                                  <>
                                                    id5SeqGainFreqConnectedSingle
                                                  <> "_duration"
                                              )
                                                /\
                                                  (Node "d=2" /\ [])
                                            , ( "def_"
                                                  <>
                                                    id5SeqGainFreqConnectedSingle
                                                  <> "_sequence"
                                              )
                                                /\
                                                  ( Node "s=[0.1 0.2]"
                                                      /\
                                                        []
                                                  )
                                            ]
                                      , properties: { columns: Just C2 }
                                      , spacedOut: false
                                      } /\
                                      [ "def_" <> id8OscSquare <>
                                          "_gain"
                                      ]
                                  )
                              , ("def_" <> id6SeqGainFreqDisconnected)
                                  /\
                                    ( Group
                                        { children:
                                            BlockDef.unsafeGroupBlockChildren
                                              [ ( "def_"
                                                    <>
                                                      id6SeqGainFreqDisconnected
                                                    <> "_duration"
                                                )
                                                  /\
                                                    (Node "d=2" /\ [])
                                              , ( "def_"
                                                    <>
                                                      id6SeqGainFreqDisconnected
                                                    <> "_sequence"
                                                )
                                                  /\
                                                    ( Node "s=[0.2 0.3]"
                                                        /\
                                                          []
                                                    )
                                              ]
                                        , properties:
                                            { columns: Just C2 }
                                        , spacedOut: false
                                        } /\
                                        []
                                    )
                              ]
                          , properties: { columns: Nothing }
                          , spacedOut: false
                          } /\ []
                      )
                  , "output" /\ (Node "output" /\ [])
                  ]
              , properties: { columns: Just C1 }
              , spacedOut: true
              }
          , lines:
              [ id1SeqFreqConnectedMultiple
                  <> " fseq{d=1,s=[100.0 200.0 300.0]}"
              , id2SeqFreqConnectedSingle
                  <> " fseq{d=1,s=[200.0 300.0 400.0]}"
              , id3SeqFreqDisconnected
                  <> " fseq{d=1,s=[300.0 400.0 500.0]}"
              , id4SeqGainConnectedMultiple
                  <> " gseq{d=2,s=[0.0 0.1]}"
              , id5SeqGainFreqConnectedSingle
                  <> " gseq{d=2,s=[0.1 0.2]}"
              , id6SeqGainFreqDisconnected
                  <> " gseq{d=2,s=[0.2 0.3]}"
              , id7OscSine <> " osc{w=sine}"
              , id8OscSquare <> " osc{w=square}"
              , id1SeqFreqConnectedMultiple <> "->" <> id7OscSine
              , id1SeqFreqConnectedMultiple <> "->" <> id8OscSquare
              , id2SeqFreqConnectedSingle <> "->" <> id7OscSine
              , id4SeqGainConnectedMultiple <> "->" <> id7OscSine
              , id4SeqGainConnectedMultiple <> "->" <> id8OscSquare
              , id5SeqGainFreqConnectedSingle <> "->" <> id8OscSquare
              , id7OscSine <> "->output"
              ]
          }
      ]

renderAudioNodeId ∷ AudioNodeId → String
renderAudioNodeId = Codec.encoder BlockId.stringCodec unit

type CodecsTestSuiteConf = Map AudioNodes
  { groupBlock ∷ GroupBlock
  , lines ∷ Array String
  }

codecsTestSuite ∷ Int → Gen CodecsTestSuiteConf → Spec Unit
codecsTestSuite quantity genSuiteConf = sequence_ testSuites
  where
  testSuites ∷ Array (Spec Unit)
  testSuites = Array.mapWithIndex testSuite suiteConfs

  suiteConfs ∷ Array CodecsTestSuiteConf
  suiteConfs = Gen.evalGen
    (Gen.vectorOf quantity genSuiteConf)
    { newSeed: mkSeed 123, size: 10 }

  testSuite ∷ Int → CodecsTestSuiteConf → Spec Unit
  testSuite index suiteConf = describe
    ("codecs suite " <> show index)
    do
      groupBlockCodecTestSuite
        ((_.groupBlock) <$> suiteConf)
      stringCodecTestSuite ((_.lines) <$> suiteConf)

groupBlockCodecTestSuite
  ∷ Map AudioNodes GroupBlock → Spec Unit
groupBlockCodecTestSuite examples = codecTestSuite
  { codec: AudioNodes.groupBlockCodec
  , encoderOpts: unit
  , examples
  , name: "audioNodes/groupBlock"
  }

stringCodecTestSuite ∷ Map AudioNodes (Array String) → Spec Unit
stringCodecTestSuite examples = codecTestSuite
  { codec: AudioNodes.stringCodec
  , encoderOpts: unit
  , examples: lines <$> examples
  , name: "audioNodes/string"
  }

unsafeAudioNodes
  ∷ Array (String /\ (AudioNode /\ Array String)) → AudioNodes
unsafeAudioNodes audioNodeEntries =
  if Array.length audioNodeEntries == Map.size audioNodesById then
    case AudioNodes.fromGraph $ Graph.fromMap audioNodesById of
      Left violationsById →
        unsafeCrashWith $
          "audio node entries violate integrity constraints: " <>
            show violationsById
      Right audioNodes →
        audioNodes
  else unsafeCrashWith $
    "there are some duplicate identifiers in the audio node entries:"
      <> "\n  input entries: "
      <> show audioNodeEntries
      <> "\n  output audio nodes: "
      <> show audioNodesById
  where
  audioNodesById ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  audioNodesById = foldlWithIndex f Map.empty
    (Map.fromFoldable audioNodeEntries)

  f
    ∷ String
    → Map AudioNodeId (AudioNode /\ List AudioNodeId)
    → (AudioNode /\ Array String)
    → Map AudioNodeId (AudioNode /\ List AudioNodeId)
  f idStr acc (audioNode /\ connectionEnds) = Map.insert
    (BlockId.unsafeBlockId idStr)
    ( audioNode /\
        (List.fromFoldable $ BlockId.unsafeBlockId <$> connectionEnds)
    )
    acc

