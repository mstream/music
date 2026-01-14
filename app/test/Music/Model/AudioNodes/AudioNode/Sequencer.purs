module Test.Music.Model.AudioNodes.AudioNode.Sequencer (spec) where

import Prelude

import Test.Music.Model.AudioNodes.AudioNode.Sequencer.Duration (spec) as Duration
import Test.Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (spec) as Sequence
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  Duration.spec
  Sequence.spec

