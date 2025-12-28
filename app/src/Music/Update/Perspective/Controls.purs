module Update.Perspective.Controls (init, update) where

import Prelude

import Audio as Audio
import Elmish as E
import Message (Message(..))
import Model.AudioNodes (AudioNodes)
import Model.Perspective
  ( ControlsPerspective
  , Perspective(..)
  )
import Model.Playback (Playback(..))
import Update.Types (Update)

init ∷ AudioNodes → ControlsPerspective
init audioNodes = { audioNodes, playback: Stopped }

update ∷ Update ControlsPerspective
update model = case _ of
  PerspectiveChanged perspective →
    pure { perspective }
  PlayRequested → do
    E.fork do
      newAudioContext ← Audio.play model.audioNodes
      pure $ PlaybackStarted newAudioContext
    pure { perspective: Controls model { playback = PlaybackStarting } }
  PlaybackStarted audioContext →
    pure
      { perspective: Controls model { playback = Playing audioContext }
      }
  StopRequested → case model.playback of
    Playing audioContext → do
      E.forkVoid $ Audio.stop audioContext
      pure { perspective: Controls model { playback = Stopped } }
    _ →
      pure { perspective: Controls model }
  _ →
    pure { perspective: Controls model }

