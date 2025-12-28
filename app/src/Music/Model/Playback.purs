module Model.Playback (Playback(..)) where

import Audio.WebAudio.Types (AudioContext)

data Playback
  = Playing AudioContext
  | PlaybackStarting
  | Stopped

