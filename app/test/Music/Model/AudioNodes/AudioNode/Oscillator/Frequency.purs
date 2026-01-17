module Test.Music.Model.AudioNodes.AudioNode.Oscillator.Frequency
  ( spec
  , unsafeFrequency
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Frequency.stringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ example10 /\ "10.0"
        , example5000 /\ "5000.0"
        , example10000 /\ "10000.0"
        ]
    , name: "frequency/string"
    }
  boundedTestSuite
    { generator: Gen.oneOf $ ArrayNE.cons'
        (Gen.elements orderedExamples)
        [ Frequency.fromNote <$> arbitrary ]
    , name: "Frequency"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedExamples ]
    , name: "Frequency"
    }

orderedExamples ∷ NonEmptyArray Frequency
orderedExamples = ArrayNE.cons' example10
  [ example5000, example10000 ]

example10 ∷ Frequency
example10 = unsafeFrequency "10.0"

example5000 ∷ Frequency
example5000 = unsafeFrequency "5000.0"

example10000 ∷ Frequency
example10000 = unsafeFrequency "10000.0"

unsafeFrequency ∷ String → Frequency
unsafeFrequency = unsafeDecoded Frequency.stringCodec
