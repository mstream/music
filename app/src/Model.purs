module Model (Controls, InitializedModel, Model(..), PlaybackModel(..)) where

import Prelude

import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext
  ( createGain
  , createOscillator
  , currentTime
  , destination
  , newAudioContext
  , resume
  , state
  , suspend
  )
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator
  ( OscillatorType(..)
  , frequency
  , setOscillatorType
  , startOscillator
  )
import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , AudioContextState(..)
  , GainNode
  , OscillatorNode
  , connect
  )
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Elmish
  ( Dispatch
  , ReactElement
  , Transition
  , fork
  , forkVoid
  , (<|)
  )
import Elmish.Boot as Boot
import Elmish.Dispatch (handleEffect)
import Elmish.HTML.Styled as H
import Web.Event.Event (preventDefault, stopPropagation)

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

