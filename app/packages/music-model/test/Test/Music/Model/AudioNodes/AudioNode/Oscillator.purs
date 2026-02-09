module Test.Music.Model.AudioNodes.AudioNode.Oscillator (spec) where

import Prelude

import Test.Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  Wave.spec

