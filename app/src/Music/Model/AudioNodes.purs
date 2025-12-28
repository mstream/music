module Music.Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  , OscillatorConf
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Music.Model.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.Frequency (Frequency)
import Music.Model.AudioNodes.Gain (Gain)
import Music.Model.AudioNodes.Wave (Wave)
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

