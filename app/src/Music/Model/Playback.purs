module Music.Model.Playback (Playback(..), PlaybackControls) where

import Audio.WebAudio.Types (AnalyserNode, AudioContext)

type PlaybackControls =
  { analyserNode ∷ AnalyserNode, audioContext ∷ AudioContext }

data Playback
  = Playing PlaybackControls
  | PlaybackStarting
  | Stopped

