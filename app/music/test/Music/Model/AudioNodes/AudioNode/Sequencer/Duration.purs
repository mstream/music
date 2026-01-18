module Test.Music.Model.AudioNodes.AudioNode.Sequencer.Duration
  ( spec
  , unsafeDuration
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Test.Data.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecsTestSuite $ Map.fromFoldable
    [ example1 /\ { parameter: "d=1", value: "1" }
    , example2 /\ { parameter: "d=2", value: "2" }
    , example3 /\ { parameter: "d=3", value: "3" }
    , example4 /\ { parameter: "d=4", value: "4" }
    ]
  boundedTestSuite
    { generator: Gen.elements orderedExamples
    , name: "Duration"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedExamples ]
    , name: "Duration"
    }

type CodecsTestSuiteConf = Map Duration
  { parameter ∷ String, value ∷ String }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite examples = do
  parameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  valueStringCodecTestSuite ((_.value) <$> examples)

parameterStringCodecTestSuite ∷ Map Duration String → Spec Unit
parameterStringCodecTestSuite examples = codecTestSuite
  { codec: Duration.parameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=1", "d = 1", "d:1" ]
  , encoderOpts: unit
  , examples
  , name: "duration parameter/string"
  }

valueStringCodecTestSuite ∷ Map Duration String → Spec Unit
valueStringCodecTestSuite examples = codecTestSuite
  { codec: Duration.valueStringCodec
  , counterExamples: Set.fromFoldable [ "abc", "-1", "0", "0.5", "1.5" ]
  , encoderOpts: unit
  , examples
  , name: "duration value/string"
  }

orderedExamples ∷ NonEmptyArray Duration
orderedExamples = ArrayNE.cons' example1
  [ example2, example3, example4 ]

example1 ∷ Duration
example1 = unsafeDuration "1"

example2 ∷ Duration
example2 = unsafeDuration "2"

example3 ∷ Duration
example3 = unsafeDuration "3"

example4 ∷ Duration
example4 = unsafeDuration "4"

unsafeDuration ∷ String → Duration
unsafeDuration = unsafeDecoded Duration.valueStringCodec
