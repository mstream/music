module Test.Music.Model.AudioNodes.AudioNode.Gain
  ( spec
  , unsafeGain
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Gain as Gain
import Test.Data.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecsTestSuite $ Map.fromFoldable
    [ example00 /\ { parameter: "g=0.0", value: "0.0" }
    , example05 /\ { parameter: "g=0.5", value: "0.5" }
    , example10 /\ { parameter: "g=1.0", value: "1.0" }
    ]

  boundedTestSuite
    { generator: Gen.elements orderedExamples
    , name: "Gain"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedExamples ]
    , name: "Gain"
    }

type CodecsTestSuiteConf = Map Gain
  { parameter ∷ String, value ∷ String }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite examples = do
  parameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  valueStringCodecTestSuite ((_.value) <$> examples)

parameterStringCodecTestSuite ∷ Map Gain String → Spec Unit
parameterStringCodecTestSuite examples = codecTestSuite
  { codec: Gain.parameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=0.5", "g = 0.5", "g:0.5" ]
  , encoderOpts: unit
  , examples
  , name: "gain parameter/string"
  }

valueStringCodecTestSuite ∷ Map Gain String → Spec Unit
valueStringCodecTestSuite examples = codecTestSuite
  { codec: Gain.valueStringCodec
  , counterExamples: Set.fromFoldable [ "abc", "-1", "2" ]
  , encoderOpts: unit
  , examples
  , name: "gain value/string"
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
unsafeGain = unsafeDecoded Gain.valueStringCodec
