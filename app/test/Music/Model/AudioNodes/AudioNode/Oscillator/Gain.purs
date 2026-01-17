module Test.Music.Model.AudioNodes.AudioNode.Oscillator.Gain
  ( spec
  , unsafeGain
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Gain.stringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ example00 /\ "0.0"
        , example05 /\ "0.5"
        , example10 /\ "1.0"
        ]
    , name: "gain/string"
    }
  boundedTestSuite
    { generator: Gen.elements orderedExamples
    , name: "Gain"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedExamples ]
    , name: "Gain"
    }

orderedExamples ∷ NonEmptyArray Gain
orderedExamples = ArrayNE.cons' example00
  [ example05, example10 ]

example00 ∷ Gain
example00 = unsafeGain "0.0"

example05 ∷ Gain
example05 = unsafeGain "0.5"

example10 ∷ Gain
example10 = unsafeGain "1.0"

unsafeGain ∷ String → Gain
unsafeGain = unsafeDecoded Gain.stringCodec
