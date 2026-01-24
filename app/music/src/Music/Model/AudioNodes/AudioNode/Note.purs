module Music.Model.AudioNodes.AudioNode.Note
  ( Name(..)
  , Note(..)
  , Octave(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Note = Note Name Octave

derive instance Eq Note
derive instance Generic Note _
derive instance Ord Note

instance Arbitrary Note where
  arbitrary = genericArbitrary

data Name = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

derive instance Eq Name
derive instance Generic Name _
derive instance Ord Name

instance Arbitrary Name where
  arbitrary = genericArbitrary

data Octave = O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8

derive instance Eq Octave
derive instance Generic Octave _
derive instance Ord Octave

instance Arbitrary Octave where
  arbitrary = genericArbitrary

