module Test.Music.Model.AudioNodes.AudioNode.Oscillator (spec) where

import Prelude

import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (spec) as Frequency
import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Gain (spec) as Gain
import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  Frequency.spec
  Gain.spec
  Wave.spec

