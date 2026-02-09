module Test.Music.Model.AudioNodes.AudioNode.Frequency
  ( spec
  , unsafeFrequency
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Frequency as Frequency
import Test.Data.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecsTestSuite $ Map.fromFoldable
    [ example10 /\ { parameter: "f=10.0", value: "10.0" }
    , example5000 /\ { parameter: "f=5000.0", value: "5000.0" }
    , example10000 /\ { parameter: "f=10000.0", value: "10000.0" }
    ]
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

type CodecsTestSuiteConf = Map Frequency
  { parameter ∷ String, value ∷ String }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite examples = do
  parameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  valueStringCodecTestSuite ((_.value) <$> examples)

parameterStringCodecTestSuite ∷ Map Frequency String → Spec Unit
parameterStringCodecTestSuite examples = codecTestSuite
  { codec: Frequency.parameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=5000.0", "f = 5000.0", "f:5000.0" ]
  , encoderOpts: unit
  , examples
  , name: "frequency parameter/string"
  }

valueStringCodecTestSuite ∷ Map Frequency String → Spec Unit
valueStringCodecTestSuite examples = codecTestSuite
  { codec: Frequency.valueStringCodec
  , counterExamples: Set.fromFoldable
      [ "abc", "-10.0", "0.0", "20000.0" ]
  , encoderOpts: unit
  , examples
  , name: "frequency value/string"
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
unsafeFrequency = unsafeDecoded Frequency.valueStringCodec
