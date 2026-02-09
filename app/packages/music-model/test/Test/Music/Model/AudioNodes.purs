module Test.Music.Model.AudioNodes (spec) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Foldable (fold)
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
  , Shape(..)
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
    id1SeqFreqConnMultiple = renderAudioNodeId id1
    id2SeqFreqConnSingle = renderAudioNodeId id2
    id3SeqFreqDisconn = renderAudioNodeId id3
    id4SeqGainConnMultiple = renderAudioNodeId id4
    id5SeqGainConnSingle = renderAudioNodeId id5
    id6SeqGainDisconn = renderAudioNodeId id6
    id7OscSine = renderAudioNodeId id7
    id8OscSquare = renderAudioNodeId id8
  pure $ Map.fromFoldable
    [ unsafeAudioNodes
        [ id1SeqFreqConnMultiple /\
            ( { audioNode: AudioNode.unsafeFrequencySequencer 1
                  [ "100.0", "200.0", "300.0" ]
              , isConnectedToOutput: false
              }
                /\ [ id7OscSine, id8OscSquare ]
            )
        , id2SeqFreqConnSingle /\
            ( { audioNode: AudioNode.unsafeFrequencySequencer 1
                  [ "200.0", "300.0", "400.0" ]
              , isConnectedToOutput: false
              }
                /\ [ id7OscSine ]
            )
        , id3SeqFreqDisconn /\
            ( { audioNode: AudioNode.unsafeFrequencySequencer 1
                  [ "300.0", "400.0", "500.0" ]
              , isConnectedToOutput: false
              }
                /\ []
            )
        , id4SeqGainConnMultiple /\
            ( { audioNode: AudioNode.unsafeGainSequencer 2
                  [ "0.0", "0.1" ]
              , isConnectedToOutput: false
              } /\
                [ id7OscSine, id8OscSquare ]
            )
        , id5SeqGainConnSingle /\
            ( { audioNode: AudioNode.unsafeGainSequencer 2
                  [ "0.1", "0.2" ]
              , isConnectedToOutput: false
              } /\
                [ id8OscSquare ]
            )
        , id6SeqGainDisconn /\
            ( { audioNode: AudioNode.unsafeGainSequencer 2
                  [ "0.2", "0.3" ]
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
                                                        { contents: "f"
                                                        , shape:
                                                            Rectangle
                                                        }
                                                        /\
                                                          []
                                                    )
                                              , ( "def_"
                                                    <>
                                                      id7OscSine
                                                    <>
                                                      "_gain"
                                                ) /\
                                                  ( Node
                                                      { contents: "g"
                                                      , shape: Rectangle
                                                      }
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
                                                      { contents:
                                                          "w=sine"
                                                      , shape: Rectangle
                                                      }
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
                                                        { contents: "f"
                                                        , shape:
                                                            Rectangle
                                                        }
                                                        /\
                                                          []
                                                    )
                                              , ( "def_"
                                                    <>
                                                      id8OscSquare
                                                    <>
                                                      "_gain"
                                                ) /\
                                                  ( Node
                                                      { contents: "g"
                                                      , shape: Rectangle
                                                      }
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
                                                      { contents:
                                                          "w=square"
                                                      , shape: Rectangle
                                                      }
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
                            [ ("def_" <> id1SeqFreqConnMultiple)
                                /\
                                  unsafeGroup
                                    { children:
                                        [ ( "def_"
                                              <>
                                                id1SeqFreqConnMultiple
                                              <> "_dummy"
                                          )
                                            /\
                                              ( Node
                                                  { contents: " "
                                                  , shape: Rectangle
                                                  } /\ []
                                              )
                                        , ( "def_"
                                              <>
                                                id1SeqFreqConnMultiple
                                              <>
                                                "_parameters"
                                          )
                                            /\ unsafeGroup
                                              { children:
                                                  [ ( "def_"
                                                        <>
                                                          id1SeqFreqConnMultiple
                                                        <> "_duration"
                                                    )
                                                      /\
                                                        ( Node
                                                            { contents:
                                                                "d=1"
                                                            , shape:
                                                                Rectangle
                                                            } /\
                                                            []
                                                        )
                                                  , ( "def_"
                                                        <>
                                                          id1SeqFreqConnMultiple
                                                        <> "_sequence"
                                                    )
                                                      /\
                                                        ( Node
                                                            { contents:
                                                                "s=[100.0 200.0 300.0]"
                                                            , shape:
                                                                Rectangle
                                                            }
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

                            , ("def_" <> id2SeqFreqConnSingle)
                                /\ unsafeGroup
                                  { children:
                                      [ ( "def_"
                                            <>
                                              id2SeqFreqConnSingle
                                            <> "_dummy"
                                        )
                                          /\
                                            ( Node
                                                { contents: " "
                                                , shape: Rectangle
                                                } /\ []
                                            )

                                      , ( "def_"
                                            <>
                                              id2SeqFreqConnSingle
                                            <>
                                              "_parameters"
                                        )
                                          /\ unsafeGroup
                                            { children:
                                                [ ( "def_"
                                                      <>
                                                        id2SeqFreqConnSingle
                                                      <> "_duration"
                                                  )
                                                    /\
                                                      ( Node
                                                          { contents:
                                                              "d=1"
                                                          , shape:
                                                              Rectangle
                                                          } /\ []
                                                      )
                                                , ( "def_"
                                                      <>
                                                        id2SeqFreqConnSingle
                                                      <> "_sequence"
                                                  )
                                                    /\
                                                      ( Node
                                                          { contents:
                                                              "s=[200.0 300.0 400.0]"
                                                          , shape:
                                                              Rectangle
                                                          }
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
                            , ("def_" <> id3SeqFreqDisconn)
                                /\ unsafeGroup
                                  { children:
                                      [ ( "def_"
                                            <>
                                              id3SeqFreqDisconn
                                            <> "_dummy"
                                        )
                                          /\
                                            ( Node
                                                { contents: " "
                                                , shape: Rectangle
                                                } /\ []
                                            )

                                      , ( "def_"
                                            <>
                                              id3SeqFreqDisconn
                                            <>
                                              "_parameters"
                                        )
                                          /\ unsafeGroup
                                            { children:
                                                [ ( "def_"
                                                      <>
                                                        id3SeqFreqDisconn
                                                      <> "_duration"
                                                  )
                                                    /\
                                                      ( Node
                                                          { contents:
                                                              "d=1"
                                                          , shape:
                                                              Rectangle
                                                          } /\ []
                                                      )
                                                , ( "def_"
                                                      <>
                                                        id3SeqFreqDisconn
                                                      <> "_sequence"
                                                  )
                                                    /\
                                                      ( Node
                                                          { contents:
                                                              "s=[300.0 400.0 500.0]"
                                                          , shape:
                                                              Rectangle
                                                          }
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
                            , ("def_" <> id4SeqGainConnMultiple)
                                /\
                                  unsafeGroup
                                    { children:
                                        [ ( "def_"
                                              <>
                                                id4SeqGainConnMultiple
                                              <> "_dummy"
                                          )
                                            /\
                                              ( Node
                                                  { contents: " "
                                                  , shape: Rectangle
                                                  } /\ []
                                              )

                                        , ( "def_"
                                              <>
                                                id4SeqGainConnMultiple
                                              <>
                                                "_parameters"
                                          )
                                            /\ unsafeGroup
                                              { children:
                                                  [ ( "def_"
                                                        <>
                                                          id4SeqGainConnMultiple
                                                        <> "_duration"
                                                    )
                                                      /\
                                                        ( Node
                                                            { contents:
                                                                "d=2"
                                                            , shape:
                                                                Rectangle
                                                            } /\
                                                            []
                                                        )
                                                  , ( "def_"
                                                        <>
                                                          id4SeqGainConnMultiple
                                                        <> "_sequence"
                                                    )
                                                      /\
                                                        ( Node
                                                            { contents:
                                                                "s=[0.0 0.1]"
                                                            , shape:
                                                                Rectangle
                                                            }
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
                                  id5SeqGainConnSingle
                              )
                                /\ unsafeGroup
                                  { children:
                                      [ ( "def_"
                                            <>
                                              id5SeqGainConnSingle
                                            <> "_dummy"
                                        )
                                          /\
                                            ( Node
                                                { contents: " "
                                                , shape: Rectangle
                                                } /\ []
                                            )

                                      , ( "def_"
                                            <>
                                              id5SeqGainConnSingle
                                            <>
                                              "_parameters"
                                        )
                                          /\ unsafeGroup
                                            { children:
                                                [ ( "def_"
                                                      <>
                                                        id5SeqGainConnSingle
                                                      <> "_duration"
                                                  )
                                                    /\
                                                      ( Node
                                                          { contents:
                                                              "d=2"
                                                          , shape:
                                                              Rectangle
                                                          } /\ []
                                                      )
                                                , ( "def_"
                                                      <>
                                                        id5SeqGainConnSingle
                                                      <> "_sequence"
                                                  )
                                                    /\
                                                      ( Node
                                                          { contents:
                                                              "s=[0.1 0.2]"
                                                          , shape:
                                                              Rectangle
                                                          }
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
                            , ("def_" <> id6SeqGainDisconn)
                                /\
                                  unsafeGroup
                                    { children:
                                        [ ( "def_"
                                              <>
                                                id6SeqGainDisconn
                                              <> "_dummy"
                                          )
                                            /\
                                              ( Node
                                                  { contents: " "
                                                  , shape: Rectangle
                                                  } /\ []
                                              )

                                        , ( "def_"
                                              <>
                                                id6SeqGainDisconn
                                              <>
                                                "_parameters"
                                          )
                                            /\ unsafeGroup
                                              { children:
                                                  [ ( "def_"
                                                        <>
                                                          id6SeqGainDisconn
                                                        <> "_duration"
                                                    )
                                                      /\
                                                        ( Node
                                                            { contents:
                                                                "d=2"
                                                            , shape:
                                                                Rectangle
                                                            } /\
                                                            []
                                                        )
                                                  , ( "def_"
                                                        <>
                                                          id6SeqGainDisconn
                                                        <> "_sequence"
                                                    )
                                                      /\
                                                        ( Node
                                                            { contents:
                                                                "s=[0.2 0.3]"
                                                            , shape:
                                                                Rectangle
                                                            }
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
                , "output" /\
                    ( Node { contents: "output", shape: Circle } /\
                        []
                    )
                ]
            , properties: { columns: Just C1 }
            , spacedOut: true
            }
        , lines: fold <$>
            [ [ id1SeqFreqConnMultiple
              , " fseq{d=1,s=[100.0 200.0 300.0]}"
              ]
            , [ id2SeqFreqConnSingle
              , " fseq{d=1,s=[200.0 300.0 400.0]}"
              ]
            , [ id3SeqFreqDisconn
              , " fseq{d=1,s=[300.0 400.0 500.0]}"
              ]
            , [ id4SeqGainConnMultiple
              , " gseq{d=2,s=[0.0 0.1]}"
              ]
            , [ id5SeqGainConnSingle
              , " gseq{d=2,s=[0.1 0.2]}"
              ]
            , [ id6SeqGainDisconn
              , " gseq{d=2,s=[0.2 0.3]}"
              ]
            , [ id7OscSine, " osc{w=sine}" ]
            , [ id8OscSquare, " osc{w=square}" ]
            , [ id1SeqFreqConnMultiple, "->" <> id7OscSine ]
            , [ id1SeqFreqConnMultiple, "->" <> id8OscSquare ]
            , [ id2SeqFreqConnSingle, "->" <> id7OscSine ]
            , [ id4SeqGainConnMultiple, "->" <> id7OscSine ]
            , [ id4SeqGainConnMultiple, "->" <> id8OscSquare ]
            , [ id5SeqGainConnSingle, "->" <> id8OscSquare ]
            , [ id7OscSine, "->output" ]
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
