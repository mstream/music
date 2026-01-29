module Test.Music.Model.AudioNodes (spec) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (sequence_)
import Data.Tuple.Nested ((/\))
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
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Random.LCG (mkSeed)
import Test.Data.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef.Unsafe
  ( unsafeGroup
  , unsafeGroupBlockChildren
  )
import Test.Music.Model.AudioNodes.AudioNode as AudioNode
import Test.Music.Model.AudioNodes.AudioNodeName as AudioNodeName
import Test.Music.Model.AudioNodes.Unsafe (unsafeAudioNodes)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.Spec (Spec, describe)
import Test.Utils (lines, unsafeGenSorted8)

spec ∷ Spec Unit
spec = do
  AudioNode.spec
  AudioNodeName.spec
  codecsSpec

codecsSpec ∷ Spec Unit
codecsSpec = codecsTestSuite 5 do
  id1 /\ id2 /\ id3 /\ id4 /\ id5 /\ id6 /\ id7 /\ id8 ←
    unsafeGenSorted8
  let
    id1SeqFreqConnectedMultiple = renderAudioNodeId id1
    id2SeqFreqConnectedSingle = renderAudioNodeId id2
    id3SeqFreqDisconnected = renderAudioNodeId id3
    id4SeqGainConnectedMultiple = renderAudioNodeId id4
    id5SeqGainConnectedSingle = renderAudioNodeId id5
    id6SeqGainDisconnected = renderAudioNodeId id6
    id7OscSine = renderAudioNodeId id7
    id8OscSquare = renderAudioNodeId id8
  pure $ Map.fromFoldable
    [ unsafeAudioNodes
        [ id1SeqFreqConnectedMultiple /\
            ( { audioNode: AudioNode.unsafeFrequencySequencer 1
                  [ 100.0, 200.0, 300.0 ]
              , isConnectedToOutput: false
              }
                /\ [ id7OscSine, id8OscSquare ]
            )
        , id2SeqFreqConnectedSingle /\
            ( { audioNode: AudioNode.unsafeFrequencySequencer 1
                  [ 200.0, 300.0, 400.0 ]
              , isConnectedToOutput: false
              }
                /\ [ id7OscSine ]
            )
        , id3SeqFreqDisconnected /\
            ( { audioNode: AudioNode.unsafeFrequencySequencer 1
                  [ 300.0, 400.0, 500.0 ]
              , isConnectedToOutput: false
              }
                /\ []
            )
        , id4SeqGainConnectedMultiple /\
            ( { audioNode: AudioNode.unsafeGainSequencer 2 [ 0.0, 0.1 ]
              , isConnectedToOutput: false
              } /\
                [ id7OscSine, id8OscSquare ]
            )
        , id5SeqGainConnectedSingle /\
            ( { audioNode: AudioNode.unsafeGainSequencer 2 [ 0.1, 0.2 ]
              , isConnectedToOutput: false
              } /\
                [ id8OscSquare ]
            )
        , id6SeqGainDisconnected /\
            ( { audioNode: AudioNode.unsafeGainSequencer 2 [ 0.2, 0.3 ]
              , isConnectedToOutput: false
              } /\ []
            )
        , id7OscSine /\
            ( { audioNode: Oscillator { wave: Sine }
              , isConnectedToOutput: true
              } /\ []
            )
        , id8OscSquare /\
            ( { audioNode: Oscillator { wave: Square }
              , isConnectedToOutput: false
              } /\ []
            )
        ] /\
        { groupBlock:
            { children: unsafeGroupBlockChildren
                [ "oscillators"
                    /\ unsafeGroup
                      { children:
                          [ ("def_" <> id7OscSine)
                              /\ unsafeGroup
                                { children:
                                    [ ( "def_" <> id7OscSine <>
                                          "_inputs"
                                      )
                                        /\ unsafeGroup
                                          { children:
                                              [ ( "def_"
                                                    <>
                                                      id7OscSine
                                                    <>
                                                      "_frequency"
                                                )
                                                  /\
                                                    ( Node
                                                        "f"
                                                        /\
                                                          []
                                                    )
                                              , ( "def_"
                                                    <>
                                                      id7OscSine
                                                    <>
                                                      "_gain"
                                                ) /\
                                                  ( Node "g"
                                                      /\
                                                        []
                                                  )
                                              ]
                                          , properties:
                                              { columns: Just
                                                  C2
                                              }
                                          , spacedOut: false
                                          }
                                        /\ []

                                    , ( "def_" <> id7OscSine <>
                                          "_parameters"
                                      )
                                        /\ unsafeGroup
                                          { children:
                                              [ ( "def_"
                                                    <>
                                                      id7OscSine
                                                    <>
                                                      "_wave"
                                                ) /\
                                                  ( Node
                                                      "w=sine"
                                                      /\ []
                                                  )
                                              ]
                                          , properties:
                                              { columns: Just
                                                  C1
                                              }
                                          , spacedOut: false
                                          }
                                        /\ []
                                    ]
                                , properties: { columns: Just C1 }
                                , spacedOut: false
                                }
                              /\ [ "output" ]
                          , ("def_" <> id8OscSquare)
                              /\ unsafeGroup
                                { children:
                                    [ ( "def_" <> id8OscSquare
                                          <>
                                            "_inputs"
                                      )
                                        /\ unsafeGroup
                                          { children:
                                              [ ( "def_"
                                                    <>
                                                      id8OscSquare
                                                    <>
                                                      "_frequency"
                                                )
                                                  /\
                                                    ( Node
                                                        "f"
                                                        /\
                                                          []
                                                    )
                                              , ( "def_"
                                                    <>
                                                      id8OscSquare
                                                    <>
                                                      "_gain"
                                                ) /\
                                                  ( Node "g"
                                                      /\
                                                        []
                                                  )
                                              ]
                                          , properties:
                                              { columns: Just
                                                  C2
                                              }
                                          , spacedOut: false
                                          }
                                        /\ []

                                    , ( "def_" <> id8OscSquare <>
                                          "_parameters"
                                      )
                                        /\ unsafeGroup
                                          { children:
                                              [ ( "def_"
                                                    <>
                                                      id8OscSquare
                                                    <>
                                                      "_wave"
                                                ) /\
                                                  ( Node
                                                      "w=square"
                                                      /\ []
                                                  )
                                              ]
                                          , properties:
                                              { columns: Just
                                                  C1
                                              }
                                          , spacedOut: false
                                          }
                                        /\ []
                                    ]
                                , properties: { columns: Just C1 }
                                , spacedOut: false
                                }
                              /\ []

                          ]
                      , properties: { columns: Nothing }
                      , spacedOut: false
                      }
                    /\ []
                , "sequencers" /\
                    ( unsafeGroup
                        { children:
                            [ ("def_" <> id1SeqFreqConnectedMultiple)
                                /\
                                  unsafeGroup
                                    { children:
                                        [ ( "def_"
                                              <>
                                                id1SeqFreqConnectedMultiple
                                              <> "_dummy"
                                          )
                                            /\ (Node " " /\ [])
                                        , ( "def_"
                                              <>
                                                id1SeqFreqConnectedMultiple
                                              <>
                                                "_parameters"
                                          )
                                            /\ unsafeGroup
                                              { children:
                                                  [ ( "def_"
                                                        <>
                                                          id1SeqFreqConnectedMultiple
                                                        <> "_duration"
                                                    )
                                                      /\
                                                        ( Node "d=1" /\
                                                            []
                                                        )
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
                                                  { columns: Just
                                                      C2
                                                  }
                                              , spacedOut: false
                                              }
                                            /\ []
                                        ]
                                    , properties:
                                        { columns: Just C1 }
                                    , spacedOut: false
                                    }
                                /\
                                  [ "def_" <> id7OscSine <>
                                      "_frequency"
                                  , "def_" <> id8OscSquare <>
                                      "_frequency"
                                  ]

                            , ("def_" <> id2SeqFreqConnectedSingle)
                                /\ unsafeGroup
                                  { children:
                                      [ ( "def_"
                                            <>
                                              id2SeqFreqConnectedSingle
                                            <> "_dummy"
                                        )
                                          /\ (Node " " /\ [])

                                      , ( "def_"
                                            <>
                                              id2SeqFreqConnectedSingle
                                            <>
                                              "_parameters"
                                        )
                                          /\ unsafeGroup
                                            { children:
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
                                            , properties:
                                                { columns: Just
                                                    C2
                                                }
                                            , spacedOut: false
                                            }
                                          /\ []
                                      ]
                                  , properties: { columns: Just C1 }
                                  , spacedOut: false
                                  }
                                /\
                                  [ "def_" <> id7OscSine <> "_frequency"
                                  ]
                            , ("def_" <> id3SeqFreqDisconnected)
                                /\ unsafeGroup
                                  { children:
                                      [ ( "def_"
                                            <>
                                              id3SeqFreqDisconnected
                                            <> "_dummy"
                                        )
                                          /\ (Node " " /\ [])

                                      , ( "def_"
                                            <>
                                              id3SeqFreqDisconnected
                                            <>
                                              "_parameters"
                                        )
                                          /\ unsafeGroup
                                            { children:
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
                                            , properties:
                                                { columns: Just
                                                    C2
                                                }
                                            , spacedOut: false
                                            }
                                          /\ []

                                      ]
                                  , properties: { columns: Just C1 }
                                  , spacedOut: false
                                  }
                                /\
                                  []
                            , ("def_" <> id4SeqGainConnectedMultiple)
                                /\
                                  unsafeGroup
                                    { children:
                                        [ ( "def_"
                                              <>
                                                id4SeqGainConnectedMultiple
                                              <> "_dummy"
                                          )
                                            /\ (Node " " /\ [])

                                        , ( "def_"
                                              <>
                                                id4SeqGainConnectedMultiple
                                              <>
                                                "_parameters"
                                          )
                                            /\ unsafeGroup
                                              { children:
                                                  [ ( "def_"
                                                        <>
                                                          id4SeqGainConnectedMultiple
                                                        <> "_duration"
                                                    )
                                                      /\
                                                        ( Node "d=2" /\
                                                            []
                                                        )
                                                  , ( "def_"
                                                        <>
                                                          id4SeqGainConnectedMultiple
                                                        <> "_sequence"
                                                    )
                                                      /\
                                                        ( Node
                                                            "s=[0.0 0.1]"
                                                            /\
                                                              []
                                                        )

                                                  ]
                                              , properties:
                                                  { columns: Just
                                                      C2
                                                  }
                                              , spacedOut: false
                                              }
                                            /\ []

                                        ]
                                    , properties:
                                        { columns: Just C1 }
                                    , spacedOut: false
                                    }
                                /\
                                  [ "def_" <> id7OscSine <>
                                      "_gain"
                                  , "def_" <> id8OscSquare <>
                                      "_gain"
                                  ]
                            , ( "def_" <>
                                  id5SeqGainConnectedSingle
                              )
                                /\ unsafeGroup
                                  { children:
                                      [ ( "def_"
                                            <>
                                              id5SeqGainConnectedSingle
                                            <> "_dummy"
                                        )
                                          /\ (Node " " /\ [])

                                      , ( "def_"
                                            <>
                                              id5SeqGainConnectedSingle
                                            <>
                                              "_parameters"
                                        )
                                          /\ unsafeGroup
                                            { children:
                                                [ ( "def_"
                                                      <>
                                                        id5SeqGainConnectedSingle
                                                      <> "_duration"
                                                  )
                                                    /\
                                                      (Node "d=2" /\ [])
                                                , ( "def_"
                                                      <>
                                                        id5SeqGainConnectedSingle
                                                      <> "_sequence"
                                                  )
                                                    /\
                                                      ( Node
                                                          "s=[0.1 0.2]"
                                                          /\
                                                            []
                                                      )

                                                ]
                                            , properties:
                                                { columns: Just
                                                    C2
                                                }
                                            , spacedOut: false
                                            }
                                          /\ []

                                      ]
                                  , properties: { columns: Just C1 }
                                  , spacedOut: false
                                  }
                                /\
                                  [ "def_" <> id8OscSquare <>
                                      "_gain"
                                  ]
                            , ("def_" <> id6SeqGainDisconnected)
                                /\
                                  unsafeGroup
                                    { children:
                                        [ ( "def_"
                                              <>
                                                id6SeqGainDisconnected
                                              <> "_dummy"
                                          )
                                            /\ (Node " " /\ [])

                                        , ( "def_"
                                              <>
                                                id6SeqGainDisconnected
                                              <>
                                                "_parameters"
                                          )
                                            /\ unsafeGroup
                                              { children:
                                                  [ ( "def_"
                                                        <>
                                                          id6SeqGainDisconnected
                                                        <> "_duration"
                                                    )
                                                      /\
                                                        ( Node "d=2" /\
                                                            []
                                                        )
                                                  , ( "def_"
                                                        <>
                                                          id6SeqGainDisconnected
                                                        <> "_sequence"
                                                    )
                                                      /\
                                                        ( Node
                                                            "s=[0.2 0.3]"
                                                            /\
                                                              []
                                                        )
                                                  ]
                                              , properties:
                                                  { columns: Just
                                                      C2
                                                  }
                                              , spacedOut: false
                                              }
                                            /\ []

                                        ]
                                    , properties:
                                        { columns: Just C1 }
                                    , spacedOut: false
                                    }
                                /\
                                  []
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
            , id5SeqGainConnectedSingle
                <> " gseq{d=2,s=[0.1 0.2]}"
            , id6SeqGainDisconnected
                <> " gseq{d=2,s=[0.2 0.3]}"
            , id7OscSine <> " osc{w=sine}"
            , id8OscSquare <> " osc{w=square}"
            , id1SeqFreqConnectedMultiple <> "->" <> id7OscSine
            , id1SeqFreqConnectedMultiple <> "->" <> id8OscSquare
            , id2SeqFreqConnectedSingle <> "->" <> id7OscSine
            , id4SeqGainConnectedMultiple <> "->" <> id7OscSine
            , id4SeqGainConnectedMultiple <> "->" <> id8OscSquare
            , id5SeqGainConnectedSingle <> "->" <> id8OscSquare
            , id7OscSine <> "->output"
            ]
        }
    ]

renderAudioNodeId ∷ AudioNodeId → String
renderAudioNodeId = Codec.encoder AudioNodeId.stringCodec unit

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
  , counterExamples: Set.empty
  , encoderOpts: unit
  , examples
  , name: "audioNodes/groupBlock"
  }

stringCodecTestSuite ∷ Map AudioNodes (Array String) → Spec Unit
stringCodecTestSuite examples = codecTestSuite
  { codec: AudioNodes.stringCodec
  , counterExamples: Set.empty
  , encoderOpts: unit
  , examples: lines <$> examples
  , name: "audioNodes/string"
  }
