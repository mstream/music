module Music.Model.AudioNodes.AudioNodeName (AudioNodeName(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data AudioNodeName = Oscillator

derive instance Eq AudioNodeName
derive instance Ord AudioNodeName
derive instance Generic AudioNodeName _

instance Arbitrary AudioNodeName where
  arbitrary = genericArbitrary

instance Show AudioNodeName where
  show = case _ of
    Oscillator →
      "Oscillator"

