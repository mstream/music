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
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode as AudioNode
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( Sequence(..)
  )
import Partial.Unsafe (unsafeCrashWith)
import Test.Codec (codecTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Test.Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Test.Music.Model.AudioNodes.AudioNode.Oscillator as Oscillator
import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Test.Music.Model.AudioNodes.AudioNode.Sequencer as Sequencer
import Test.Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Test.Spec (Spec)
import Test.Utils (lines, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  Oscillator.spec
  Sequencer.spec
  codecsTestSuite
    { examples:
        Map.fromFoldable
          [ unsafeFrequencySequencer 1 [ 100.0, 200.0, 300.0 ] /\
              { groupBlock:
                  { children: BlockDef.unsafeGroupBlockChildren
                      [ "parent_duration" /\ (Node "d=1" /\ [])
                      , "parent_sequence" /\
                          (Node "s=[100.0 200.0 300.0]" /\ [])
                      ]
                  , properties: { columns: Just C2 }
                  , spacedOut: false
                  }
              , lines: [ "fseq{d=1,s=[100.0 200.0 300.0]}" ]
              }
          , unsafeGainSequencer 2 [ 0.0, 0.5 ] /\
              { groupBlock:
                  { children: BlockDef.unsafeGroupBlockChildren
                      [ "parent_duration" /\ (Node "d=2" /\ [])
                      , "parent_sequence" /\ (Node "s=[0.0 0.5]" /\ [])
                      ]
                  , properties: { columns: Just C2 }
                  , spacedOut: false
                  }
              , lines: [ "gseq{d=2,s=[0.0 0.5]}" ]
              }
          , Oscillator { wave: Sine } /\
              { groupBlock:
                  { children: BlockDef.unsafeGroupBlockChildren
                      [ "parent_frequency" /\ (Node "f" /\ [])
                      , "parent_gain" /\ (Node "g" /\ [])
                      , "parent_wave" /\ (Node "w=sine" /\ [])
                      ]
                  , properties: { columns: Just C2 }
                  , spacedOut: false
                  }
              , lines: [ "osc{w=sine}" ]
              }
          , Oscillator { wave: Square } /\
              { groupBlock:
                  { children: BlockDef.unsafeGroupBlockChildren
                      [ "parent_frequency" /\ (Node "f" /\ [])
                      , "parent_gain" /\ (Node "g" /\ [])
                      , "parent_wave" /\ (Node "w=square" /\ [])
                      ]
                  , properties: { columns: Just C2 }
                  , spacedOut: false
                  }
              , lines: [ "osc{w=square}" ]
              }
          ]
    , groupBlockParentId: BlockId.unsafeBlockId "parent"
    }
  orderedTestSuite
    { examples: Set.empty ∷ Set (NonEmptyArray AudioNode)
    , name: "AudioNode"
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
  , encoderOpts: parentBlockId
  , examples
  , name: "audioNode/groupBlock"
  }

stringCodecTestSuite ∷ Map AudioNode (Array String) → Spec Unit
stringCodecTestSuite examples = codecTestSuite
  { codec: AudioNode.stringCodec
  , encoderOpts: unit
  , examples: lines <$> examples
  , name: "audioNode/string"
  }

unsafeFrequencySequencer ∷ Int → Array Number → AudioNode
unsafeFrequencySequencer = unsafeSequencer
  FrequencySequencer
  Frequency.unsafeFrequency

unsafeGainSequencer ∷ Int → Array Number → AudioNode
unsafeGainSequencer = unsafeSequencer GainSequencer Gain.unsafeGain

unsafeSequencer
  ∷ ∀ a
  . (Sequencer a → AudioNode)
  → (String → a)
  → Int
  → Array Number
  → AudioNode
unsafeSequencer ctor valStringToEl durationValue sequenceValues =
  ctor { duration, sequence }
  where
  sequence ∷ Sequence a
  sequence = Sequence elements

  duration ∷ Duration
  duration = Duration.unsafeDuration $ "d=" <> show durationValue

  elements ∷ NonEmptyArray a
  elements = case ArrayNE.fromArray sequenceValues of
    Just nonEmptySequenceValues →
      valStringToEl <<< show <$> nonEmptySequenceValues
    Nothing →
      unsafeCrashWith
        "there should be at least one sequence value in the sequencer"

