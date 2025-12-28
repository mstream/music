module Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  , OscillatorConf
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Model.AudioNodeId (AudioNodeId)
import Model.AudioNodes.Frequency (Frequency)
import Model.AudioNodes.Gain (Gain)
import Model.AudioNodes.Wave (Wave)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

type AudioNodes = Map AudioNodeId AudioNode

data AudioNode = Oscillator OscillatorConf

derive instance Eq AudioNode
derive instance Generic AudioNode _
derive instance Ord AudioNode

instance Arbitrary AudioNode where
  arbitrary = genericArbitrary

instance Show AudioNode where
  show = genericShow

type OscillatorConf =
  { frequency ∷ Frequency, gain ∷ Gain, wave ∷ Wave }

