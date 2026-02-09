module Test.Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( spec
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Test.Data.Codec (codecTestSuite)
import Test.Music.Model.AudioNodes.AudioNode.Frequency
  ( unsafeFrequency
  ) as Frequency
import Test.Music.Model.AudioNodes.AudioNode.Gain
  ( unsafeGain
  ) as Gain
import Test.Spec (Spec)
import Test.Utils (orderedTestSuite)

spec ∷ Spec Unit
spec = do
  frequencyCodecsTestSuite $ Map.fromFoldable
    [ frequencySequenceExample /\
        { parameter: "s=[100.0 200.0 300.0]"
        , value: "[100.0 200.0 300.0]"
        }
    ]
  gainCodecsTestSuite $ Map.fromFoldable
    [ gainSequenceExample /\
        { parameter: "s=[0.0 0.5 1.0]"
        , value: "[0.0 0.5 1.0]"
        }
    ]

  orderedTestSuite
    { examples: Set.fromFoldable [ orderedFrequencySequenceExamples ]
    , name: "Frequency Sequence"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedGainSequenceExamples ]
    , name: "Gain Sequence"
    }

type CodecsTestSuiteConf a = Map (Sequence a)
  { parameter ∷ String, value ∷ String }

frequencyCodecsTestSuite
  ∷ CodecsTestSuiteConf Frequency → Spec Unit
frequencyCodecsTestSuite examples = do
  frequencyParameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  frequencyValueStringCodecTestSuite ((_.value) <$> examples)

gainCodecsTestSuite
  ∷ CodecsTestSuiteConf Gain → Spec Unit
gainCodecsTestSuite examples = do
  gainParameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  gainValueStringCodecTestSuite ((_.value) <$> examples)

frequencyParameterStringCodecTestSuite
  ∷ Map (Sequence Frequency) String → Spec Unit
frequencyParameterStringCodecTestSuite examples = codecTestSuite
  { codec: Sequence.frequencyParameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=[100.0 200.0 300.0]"
      , "s = [100.0 200.0 300.0]"
      , "g:[100.0 200.0 300.0]"
      ]
  , encoderOpts: unit
  , examples
  , name: "frequency sequence parameter/string"
  }

gainParameterStringCodecTestSuite
  ∷ Map (Sequence Gain) String → Spec Unit
gainParameterStringCodecTestSuite examples = codecTestSuite
  { codec: Sequence.gainParameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=[0.0 0.5 1.0]"
      , "s = [0.0 0.5 1.0]"
      , "g:[0.0 0.5 1.0]"
      ]
  , encoderOpts: unit
  , examples
  , name: "gain sequence parameter/string"
  }

frequencyValueStringCodecTestSuite
  ∷ Map (Sequence Frequency) String → Spec Unit
frequencyValueStringCodecTestSuite examples = codecTestSuite
  { codec: Sequence.frequencyValueStringCodec
  , counterExamples: Set.fromFoldable
      [ "[aaa bbb ccc]", "[-100.0 0.0 100.0]" ]
  , encoderOpts: unit
  , examples
  , name: "frequency sequence value/string"
  }

gainValueStringCodecTestSuite
  ∷ Map (Sequence Gain) String → Spec Unit
gainValueStringCodecTestSuite examples = codecTestSuite
  { codec: Sequence.gainValueStringCodec
  , counterExamples: Set.fromFoldable
      [ "[aaa bbb]", "[-1.0 2.0]" ]
  , encoderOpts: unit
  , examples
  , name: "gain sequence value/string"
  }

orderedFrequencySequenceExamples ∷ NonEmptyArray (Sequence Frequency)
orderedFrequencySequenceExamples = ArrayNE.cons'
  frequencySequenceExample
  []

orderedGainSequenceExamples ∷ NonEmptyArray (Sequence Gain)
orderedGainSequenceExamples = ArrayNE.cons'
  gainSequenceExample
  []

frequencySequenceExample ∷ Sequence Frequency
frequencySequenceExample = Sequence.fromFoldable1 $ ArrayNE.cons'
  ( Frequency.unsafeFrequency
      "100.0"
  )
  [ Frequency.unsafeFrequency "200.0"
  , Frequency.unsafeFrequency "300.0"
  ]

gainSequenceExample ∷ Sequence Gain
gainSequenceExample = Sequence.fromFoldable1 $ ArrayNE.cons'
  (Gain.unsafeGain "0.0")
  [ Gain.unsafeGain "0.5"
  , Gain.unsafeGain "1.0"
  ]

