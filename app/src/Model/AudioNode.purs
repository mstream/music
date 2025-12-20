module Model.AudioNode
  ( AudioComponent(..)
  , AudioNode
  , OscillatorConf
  , Wave(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type AudioNode = { component ∷ AudioComponent, id ∷ String }
data AudioComponent = Oscillator OscillatorConf

derive instance Eq AudioComponent
derive instance Generic AudioComponent _

instance Show AudioComponent where
  show = genericShow

type OscillatorConf = { frequency ∷ Number, gain ∷ Number, wave ∷ Wave }

data Wave = Sine | Square

derive instance Eq Wave
derive instance Generic Wave _

instance Show Wave where
  show = genericShow
