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
import Data.List (List)
import Data.Maybe (Maybe)
import Effect.Timer (IntervalId)
import Model.AudioNode (AudioNode)

data Model = Initialized InitializedModel | Uninitialized

type InitializedModel =
  { ctrls ∷ Controls
  , frequency ∷ Number
  , gain ∷ Number
  , nodes ∷ List AudioNode
  , nodesDiagramSvg ∷ Maybe String
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

