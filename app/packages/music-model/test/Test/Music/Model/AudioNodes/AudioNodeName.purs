module Test.Music.Model.AudioNodes.AudioNodeName (spec) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Set as Set
import Music.Model.AudioNodes.AudioNodeName (AudioNodeName(..))
import Test.Spec (Spec)
import Test.Utils (orderedTestSuite)

spec ∷ Spec Unit
spec = do
  orderedTestSuite
    { examples: Set.fromFoldable
        [ orderedExamples
        ]
    , name: "AudioNodeName"
    }

orderedExamples ∷ NonEmptyArray AudioNodeName
orderedExamples = ArrayNE.cons' Oscillator []

