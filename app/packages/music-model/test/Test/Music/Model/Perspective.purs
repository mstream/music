module Test.Music.Model.Perspective (spec) where

import Prelude

import Test.Music.Model.Perspective.PerspectiveName as PerspectiveName
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  PerspectiveName.spec
