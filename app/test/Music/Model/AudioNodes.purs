module Test.Music.Model.AudioNodes (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph as Graph
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Partial.Unsafe (unsafeCrashWith)
import Test.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Test.Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Test.Music.Model.AudioNodes.AudioNode as AudioNode
import Test.Music.Model.AudioNodes.AudioNodeName as AudioNodeName
import Test.Spec (Spec)
import Test.Utils (lines)

spec ∷ Spec Unit
spec = do
  AudioNode.spec
  AudioNodeName.spec
  codecsTestSuite $ Map.fromFoldable
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
        [ "def-seq-freq-connected-multiple" /\
            ( AudioNode.unsafeFrequencySequencer 1
                [ 100.0, 200.0, 300.0 ]
                /\ [ "def-osc-sine", "def-osc-square" ]
            )
        , "def-seq-freq-connected-single" /\
            ( AudioNode.unsafeFrequencySequencer 1
                [ 100.0, 200.0, 300.0 ]
                /\ [ "def-osc-sine" ]
            )
        , "def-seq-freq-disconnected" /\
            ( AudioNode.unsafeFrequencySequencer 1
                [ 100.0, 200.0, 300.0 ]
                /\ []
            )
        , "def-seq-gain" /\
            (AudioNode.unsafeGainSequencer 2 [ 0.0, 0.5 ] /\ [])
        , "def-osc-sine" /\ (Oscillator { wave: Sine } /\ [ "output" ])
        , "def-osc-square" /\ (Oscillator { wave: Square } /\ [])
        ] /\
        { groupBlock:
            { children: BlockDef.unsafeGroupBlockChildren
                [ "oscillators" /\
                    ( Group
                        { children: BlockDef.unsafeGroupBlockChildren
                            [ "def-osc-sine" /\
                                ( Group
                                    { children:
                                        BlockDef.unsafeGroupBlockChildren
                                          [ "def-osc-sine-frequency" /\
                                              (Node "f" /\ [])
                                          , "def-osc-sine-gain" /\
                                              (Node "g" /\ [])
                                          , "def-osc-sine-wave" /\
                                              (Node "w=sine" /\ [])
                                          ]
                                    , properties: { columns: Just C2 }
                                    , spacedOut: false
                                    } /\ [ "output" ]
                                )
                            , "def-osc-square" /\
                                ( Group
                                    { children:
                                        BlockDef.unsafeGroupBlockChildren
                                          [ "def-osc-square-frequency"
                                              /\
                                                (Node "f" /\ [])
                                          , "def-osc-square-gain" /\
                                              (Node "g" /\ [])
                                          , "def-osc-square-wave" /\
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
                            [ "def-seq-freq-connected-multiple" /\
                                ( Group
                                    { children:
                                        BlockDef.unsafeGroupBlockChildren
                                          [ "def-seq-freq-connected-multiple-duration"
                                              /\
                                                (Node "d=1" /\ [])
                                          , "def-seq-freq-connected-multiple-sequence"
                                              /\
                                                ( Node
                                                    "s=[100.0 200.0 300.0]"
                                                    /\ []
                                                )
                                          ]
                                    , properties: { columns: Just C2 }
                                    , spacedOut: false
                                    } /\
                                    [ "def-osc-sine-frequency"
                                    , "def-osc-square-frequency"
                                    ]
                                )
                            , "def-seq-freq-connected-single" /\
                                ( Group
                                    { children:
                                        BlockDef.unsafeGroupBlockChildren
                                          [ "def-seq-freq-connected-single-duration"
                                              /\
                                                (Node "d=1" /\ [])
                                          , "def-seq-freq-connected-single-sequence"
                                              /\
                                                ( Node
                                                    "s=[100.0 200.0 300.0]"
                                                    /\ []
                                                )
                                          ]
                                    , properties: { columns: Just C2 }
                                    , spacedOut: false
                                    } /\
                                    [ "def-osc-sine-frequency" ]
                                )
                            , "def-seq-freq-connected-disconnected" /\
                                ( Group
                                    { children:
                                        BlockDef.unsafeGroupBlockChildren
                                          [ "def-seq-freq-connected-disconnected-duration"
                                              /\
                                                (Node "d=1" /\ [])
                                          , "def-seq-freq-connected-disconnected-sequence"
                                              /\
                                                ( Node
                                                    "s=[100.0 200.0 300.0]"
                                                    /\ []
                                                )
                                          ]
                                    , properties: { columns: Just C2 }
                                    , spacedOut: false
                                    } /\
                                    []
                                )
                            , "def-seq-gain" /\
                                ( Group
                                    { children:
                                        BlockDef.unsafeGroupBlockChildren
                                          [ "def-seq-gain-duration" /\
                                              (Node "d=2" /\ [])
                                          , "def-seq-gain-sequence" /\
                                              (Node "s=[0.0 0.5]" /\ [])
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
                , "output" /\ (Node "output" /\ [])
                ]
            , properties: { columns: Just C1 }
            , spacedOut: true
            }
        , lines:
            [ "osc-sine osc{w=sine}"
            , "osc-square osc{w=square}"
            , "seq-freq-connected-single fsec{d=1,s=[100.0 200.0 300.0]}"
            , "seq-freq-connected-multiple fsec{d=1,s=[100.0 200.0 300.0]}"
            , "seq-freq-disconnected fsec{d=1,s=[100.0 200.0 300.0]}"
            , "seq-gain gsec{d=2,s=[0.0 0.5]}"
            , "osc-sine->output"
            , "seq-freq-connected-single->osc-sine"
            , "seq-freq-connected-multiple->osc-sine"
            , "seq-freq-connected-multiple->osc-square"
            ]
        }
    ]

type CodecsTestSuiteConf = Map AudioNodes
  { groupBlock ∷ GroupBlock
  , lines ∷ Array String
  }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite examples = do
  groupBlockCodecTestSuite
    ((_.groupBlock) <$> examples)
  stringCodecTestSuite ((_.lines) <$> examples)

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

