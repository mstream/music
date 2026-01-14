module Test.Music.Model (spec) where

import Prelude

import Test.Music.Model.AudioNodes as AudioNodes
import Test.Music.Model.Perspective as Perspective
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  AudioNodes.spec
  Perspective.spec
