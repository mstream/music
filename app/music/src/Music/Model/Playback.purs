module Music.Model.Playback (Playback(..), PlaybackControls) where

import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , GainNode
  , OscillatorNode
  )
import Data.Map (Map)
import Data.Tuple.Nested (type (/\))
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)

type PlaybackControls =
  { analyser ∷ AnalyserNode
  , audioContext ∷ AudioContext
  , oscillators ∷ Map AudioNodeId (GainNode /\ OscillatorNode)
  }

data Playback
  = Playing PlaybackControls
  | PlaybackStarting
  | Stopped

