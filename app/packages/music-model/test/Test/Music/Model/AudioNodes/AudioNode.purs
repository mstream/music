module Test.Music.Model.AudioNodes.AudioNode
  ( spec
  , unsafeFrequencySequencer
  , unsafeGainSequencer
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  , Shape(..)
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode as AudioNode
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Partial.Unsafe (unsafeCrashWith)
import Test.Data.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef.Unsafe
  ( unsafeGroup
  , unsafeGroupBlockChildren
  )
import Test.Mermaid.DiagramDef.Blocks.BlockId.Unsafe (unsafeBlockId)
import Test.Music.Model.AudioNodes.AudioNode.Frequency (spec) as Frequency
import Test.Music.Model.AudioNodes.AudioNode.Frequency (unsafeFrequency)
import Test.Music.Model.AudioNodes.AudioNode.Gain (spec) as Gain
import Test.Music.Model.AudioNodes.AudioNode.Gain (unsafeGain)
import Test.Music.Model.AudioNodes.AudioNode.Note (spec) as Note
import Test.Music.Model.AudioNodes.AudioNode.Note (unsafeNote)
import Test.Music.Model.AudioNodes.AudioNode.Oscillator as Oscillator
import Test.Music.Model.AudioNodes.AudioNode.Sequencer as Sequencer
import Test.Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Test.Spec (Spec)
import Test.Utils (lines, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  Frequency.spec
  Gain.spec
  Note.spec
  Oscillator.spec
  Sequencer.spec
  codecsSpec
  orderedTestSuite
    { examples: Set.empty ∷ Set (NonEmptyArray AudioNode)
    , name: "AudioNode"
    }

codecsSpec ∷ Spec Unit
codecsSpec = codecsTestSuite
  { examples:
      Map.fromFoldable
        [ unsafeFrequencySequencer 1 [ "100.0", "200.0", "300.0" ] /\
            { groupBlock:
                { children: unsafeGroupBlockChildren
                    [ "parent_dummy" /\
                        (Node { contents: " ", shape: Rectangle } /\ [])
                    , "parent_parameters"
                        /\ unsafeGroup
                          { children:
                              [ "parent_duration" /\
                                  ( Node
                                      { contents: "d=1"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              , "parent_sequence" /\
                                  ( Node
                                      { contents:
                                          "s=[100.0 200.0 300.0]"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C2 }
                          , spacedOut: false
                          }
                        /\ []
                    ]
                , properties: { columns: Just C1 }
                , spacedOut: false
                }
            , lines: [ "fseq{d=1,s=[100.0 200.0 300.0]}" ]
            }
        , unsafeGainSequencer 2 [ "0.0", "0.5" ] /\
            { groupBlock:
                { children: unsafeGroupBlockChildren
                    [ "parent_dummy" /\
                        (Node { contents: " ", shape: Rectangle } /\ [])
                    , "parent_parameters"
                        /\ unsafeGroup
                          { children:
                              [ "parent_duration" /\
                                  ( Node
                                      { contents: "d=2"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              , "parent_sequence" /\
                                  ( Node
                                      { contents: "s=[0.0 0.5]"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C2 }
                          , spacedOut: false
                          }
                        /\ []
                    ]
                , properties: { columns: Just C1 }
                , spacedOut: false
                }
            , lines: [ "gseq{d=2,s=[0.0 0.5]}" ]
            }
        , unsafeNoteSequencer 2 [ "C1", "C#1", "C2" ] /\
            { groupBlock:
                { children: unsafeGroupBlockChildren
                    [ "parent_dummy" /\
                        (Node { contents: " ", shape: Rectangle } /\ [])
                    , "parent_parameters"
                        /\ unsafeGroup
                          { children:
                              [ "parent_duration" /\
                                  ( Node
                                      { contents: "d=2"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              , "parent_sequence" /\
                                  ( Node
                                      { contents: "s=[C1 C#1 C2]"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C2 }
                          , spacedOut: false
                          }
                        /\ []
                    ]
                , properties: { columns: Just C1 }
                , spacedOut: false
                }
            , lines: [ "nseq{d=2,s=[C1 C#1 C2]}" ]
            }

        , Oscillator { wave: Sine } /\
            { groupBlock:
                { children: unsafeGroupBlockChildren
                    [ "parent_inputs"
                        /\ unsafeGroup
                          { children:
                              [ "parent_frequency" /\
                                  ( Node
                                      { contents: "f"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              , "parent_gain" /\
                                  ( Node
                                      { contents: "g"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C2 }
                          , spacedOut: false
                          }
                        /\ []
                    , "parent_parameters"
                        /\ unsafeGroup
                          { children:
                              [ "parent_wave" /\
                                  ( Node
                                      { contents: "w=sine"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C1 }
                          , spacedOut: false
                          }
                        /\ []
                    ]
                , properties: { columns: Just C1 }
                , spacedOut: false
                }
            , lines: [ "osc{w=sine}" ]
            }
        , Oscillator { wave: Square } /\
            { groupBlock:
                { children: unsafeGroupBlockChildren
                    [ "parent_inputs"
                        /\ unsafeGroup
                          { children:
                              [ "parent_frequency" /\
                                  ( Node
                                      { contents: "f"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              , "parent_gain" /\
                                  ( Node
                                      { contents: "g"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C2 }
                          , spacedOut: false
                          }
                        /\ []
                    , "parent_parameters"
                        /\ unsafeGroup
                          { children:
                              [ "parent_wave" /\
                                  ( Node
                                      { contents: "w=square"
                                      , shape: Rectangle
                                      } /\ []
                                  )
                              ]
                          , properties: { columns: Just C1 }
                          , spacedOut: false
                          }
                        /\ []
                    ]
                , properties: { columns: Just C1 }
                , spacedOut: false
                }
            , lines: [ "osc{w=square}" ]
            }
        ]
  , groupBlockParentId: unsafeBlockId "parent"
  }

type CodecsTestSuiteConf =
  { examples ∷
      Map AudioNode
        { groupBlock ∷ GroupBlock
        , lines ∷ Array String
        }
  , groupBlockParentId ∷ BlockId
  }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite conf = do
  groupBlockCodecTestSuite
    conf.groupBlockParentId
    ((_.groupBlock) <$> conf.examples)
  stringCodecTestSuite ((_.lines) <$> conf.examples)

groupBlockCodecTestSuite
  ∷ BlockId → Map AudioNode GroupBlock → Spec Unit
groupBlockCodecTestSuite parentBlockId examples = codecTestSuite
  { codec: AudioNode.groupBlockCodec
  , counterExamples: Set.empty
  , encoderOpts: parentBlockId
  , examples
  , name: "audioNode/groupBlock"
  }

stringCodecTestSuite ∷ Map AudioNode (Array String) → Spec Unit
stringCodecTestSuite examples = codecTestSuite
  { codec: AudioNode.stringCodec
  , counterExamples: Set.empty
  , encoderOpts: unit
  , examples: lines <$> examples
  , name: "audioNode/string"
  }

unsafeFrequencySequencer ∷ Int → Array String → AudioNode
unsafeFrequencySequencer = unsafeSequencer
  FrequencySequencer
  unsafeFrequency

unsafeGainSequencer ∷ Int → Array String → AudioNode
unsafeGainSequencer = unsafeSequencer GainSequencer unsafeGain

unsafeNoteSequencer ∷ Int → Array String → AudioNode
unsafeNoteSequencer = unsafeSequencer NoteSequencer unsafeNote

unsafeSequencer
  ∷ ∀ a
  . (Sequencer a → AudioNode)
  → (String → a)
  → Int
  → Array String
  → AudioNode
unsafeSequencer ctor valStringToEl durationValue sequenceValues =
  ctor { duration, sequence }
  where
  sequence ∷ Sequence a
  sequence = Sequence.fromFoldable1 elements

  duration ∷ Duration
  duration = Duration.unsafeDuration $ show durationValue

  elements ∷ NonEmptyArray a
  elements = case ArrayNE.fromArray sequenceValues of
    Just nonEmptySequenceValues →
      valStringToEl <$> nonEmptySequenceValues
    Nothing →
      unsafeCrashWith
        "there should be at least one sequence value in the sequencer"

