module Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer, valueAt) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as ArrayNE
import Data.Maybe (Maybe(..))
import Data.Natural (Natural)
import Data.Natural as Natural
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence

type Sequencer a =
  { duration ∷ Duration, sequence ∷ Sequence a }

valueAt ∷ ∀ a. Sequencer a → Natural → a
valueAt { duration, sequence } step =
  case elements !! index `div` elementLength of
    Just element →
      element
    Nothing →
      -- Should never happen if the index calculation is correct
      -- defaulting to the first element in the sequence for type safety
      ArrayNE.head elements

  where
  index ∷ Int
  index = Natural.natToInt step `mod` effectiveSequenceLength

  effectiveSequenceLength ∷ Int
  effectiveSequenceLength = ArrayNE.length elements * elementLength

  elementLength ∷ Int
  elementLength = Duration.toInt duration

  elements ∷ NonEmptyArray a
  elements = Sequence.toArray sequence
