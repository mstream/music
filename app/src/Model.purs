module Model
  ( AudioComponent(..)
  , AudioNode
  , Controls
  , InitializedModel
  , Model(..)
  , OscillatorConf
  , PlaybackModel(..)
  , audioNode
  ) where

import Prelude

import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , GainNode
  , OscillatorNode
  )
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Timer (IntervalId)
import Parsing (Parser, fail)

data Model = Initialized InitializedModel | Uninitialized

type InitializedModel =
  { ctrls ∷ Controls
  , frequency ∷ Number
  , gain ∷ Number
  , playback ∷ PlaybackModel
  }

type Controls =
  { anal ∷ AnalyserNode
  , ctx ∷ AudioContext
  , g ∷ GainNode
  , osc ∷ OscillatorNode
  }

data PlaybackModel
  = Started IntervalId
  | Starting
  | Stopped

type AudioNode = { component ∷ AudioComponent, id ∷ String }
data AudioComponent = Oscillator OscillatorConf

derive instance Eq AudioComponent
derive instance Generic AudioComponent _

instance Show AudioComponent where
  show = genericShow

type OscillatorConf = { frequency ∷ Number, gain ∷ Number }

audioNode ∷ Parser String AudioNode
audioNode = fail "TODO"
