module Music.Init.Perspective.Controls (init) where

import Prelude

import Music.Init.Types (Init)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Playback (Playback(..))

init ∷ ∀ m. AudioNodes → Init m ControlsPerspective
init audioNodes = pure { audioNodes, playback: Stopped }
