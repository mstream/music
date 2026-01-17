module Test.Music.Model.AudioNodes.AudioNode.Sequencer.Duration
  ( spec
  , unsafeDuration
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Duration.stringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ example1 /\ "d=1"
        , example2 /\ "d=2"
        , example3 /\ "d=3"
        , example4 /\ "d=4"
        ]
    , name: "gain/string"
    }
  boundedTestSuite
    { generator: Gen.elements orderedExamples
    , name: "Duration"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedExamples ]
    , name: "Duration"
    }

orderedExamples ∷ NonEmptyArray Duration
orderedExamples = ArrayNE.cons' example1
  [ example2, example3, example4 ]

example1 ∷ Duration
example1 = unsafeDuration "d=1"

example2 ∷ Duration
example2 = unsafeDuration "d=2"

example3 ∷ Duration
example3 = unsafeDuration "d=3"

example4 ∷ Duration
example4 = unsafeDuration "d=4"

unsafeDuration ∷ String → Duration
unsafeDuration = unsafeDecoded Duration.stringCodec
