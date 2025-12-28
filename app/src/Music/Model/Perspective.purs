module Model.Perspective
  ( ControlsPerspective
  , Perspective(..)
  ) where

import Model.AudioNodes (AudioNodes)
import Model.Playback (Playback)

type ControlsPerspective =
  { audioNodes ∷ AudioNodes
  , playback ∷ Playback
  }

data Perspective
  = Code String
  | Controls ControlsPerspective
  | Diagram String
