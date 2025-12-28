module Music.Model.Perspective
  ( ControlsPerspective
  , Perspective(..)
  ) where

import Music.Model.AudioNodes (AudioNodes)
import Music.Model.Playback (Playback)

type ControlsPerspective =
  { audioNodes ∷ AudioNodes
  , playback ∷ Playback
  }

data Perspective
  = Code String
  | Controls ControlsPerspective
  | Diagram String
