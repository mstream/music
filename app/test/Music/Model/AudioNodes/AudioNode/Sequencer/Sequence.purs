module Test.Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( spec
  ) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( Sequence(..)
  )
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Test.Codec (codecTestSuite)
import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Frequency
  ( unsafeFrequency
  ) as Frequency
import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Gain
  ( unsafeGain
  ) as Gain
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Sequence.frequencyStringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ frequencySequenceExample /\ "s=[100.0 200.0 300.0]"
        ]
    , name: "frequency sequence/string"
    }
  codecTestSuite
    { codec: Sequence.gainStringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ gainSequenceExample /\ "s=[0.0 0.5 1.0]"
        ]
    , name: "gain sequence/string"
    }

frequencySequenceExample ∷ Sequence Frequency
frequencySequenceExample = Sequence $ ArrayNE.cons'
  ( Frequency.unsafeFrequency
      "100.0"
  )
  [ Frequency.unsafeFrequency "200.0"
  , Frequency.unsafeFrequency "300.0"
  ]

gainSequenceExample ∷ Sequence Gain
gainSequenceExample = Sequence $ ArrayNE.cons'
  (Gain.unsafeGain "0.0")
  [ Gain.unsafeGain "0.5"
  , Gain.unsafeGain "1.0"
  ]

