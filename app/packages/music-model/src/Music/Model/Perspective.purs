module Music.Model.Perspective
  ( CodePerspective
  , ControlsPerspective
  , DiagramPerspective
  , DiagramState(..)
  , Perspective(..)
  ) where

import Mermaid.DiagramDef (DiagramDef)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.Playback (Playback)

data Perspective
  = Code CodePerspective
  | Controls ControlsPerspective
  | Diagram DiagramPerspective

type CodePerspective =
  { code ∷ String }

type ControlsPerspective =
  { audioNodes ∷ AudioNodes
  , playback ∷ Playback
  }

type DiagramPerspective =
  { audioNodes ∷ AudioNodes
  , state ∷ DiagramState
  }

data DiagramState
  = Generated String
  | Generating DiagramDef
  | Invalid String

