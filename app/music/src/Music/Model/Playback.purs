module Music.Model.Playback (Playback(..), PlaybackControls, Playing) where

import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , GainNode
  , OscillatorNode
  )
import Data.Map (Map)
import Data.Natural (Natural)
import Data.Tuple.Nested (type (/\))
import Effect.Timer (IntervalId)
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)

type PlaybackControls =
  { analyser ∷ AnalyserNode
  , audioContext ∷ AudioContext
  , oscillators ∷ Map AudioNodeId (GainNode /\ OscillatorNode)
  }

type Playing =
  { controls ∷ PlaybackControls
  , step ∷ Natural
  , stepUpdateIntervalId ∷ IntervalId
  }

data Playback
  = Playing Playing
  | PlaybackStarting
  | Stopped

