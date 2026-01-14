module Test.Music (spec) where

import Prelude

import Test.Music.Model as Model
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  Model.spec
