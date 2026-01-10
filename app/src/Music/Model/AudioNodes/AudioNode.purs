module Music.Model.AudioNodes.AudioNode (AudioNode(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data AudioNode = Oscillator Oscillator

derive instance Eq AudioNode
derive instance Generic AudioNode _
derive instance Ord AudioNode

instance Arbitrary AudioNode where
  arbitrary = genericArbitrary

instance Show AudioNode where
  show = genericShow

