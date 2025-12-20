module Model
  ( Controls
  , InitializedModel
  , Model(..)
  , PlaybackModel(..)
  ) where

import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , GainNode
  , OscillatorNode
  )
import Effect.Timer (IntervalId)

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

