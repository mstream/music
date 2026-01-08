module Music.Model.PerspectiveName (PerspectiveName(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data PerspectiveName = Code | Controls | Diagram

derive instance Eq PerspectiveName

derive instance Generic PerspectiveName _

instance Arbitrary PerspectiveName where
  arbitrary = genericArbitrary

instance Ord PerspectiveName where
  compare Code other = case other of
    Code →
      EQ
    _ →
      LT
  compare Diagram other = case other of
    Code →
      GT
    Diagram →
      EQ
    Controls →
      LT
  compare Controls other = case other of
    Controls →
      EQ
    _ →
      GT

instance Show PerspectiveName where
  show = case _ of
    Code →
      "Code"
    Controls →
      "Controls"
    Diagram →
      "Diagram"

