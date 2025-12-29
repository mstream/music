module Music.Update.Perspective.Controls (init, update) where

import Prelude

import Elmish as E
import Music.Audio as Audio
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.Perspective (ControlsPerspective, Perspective(..))
import Music.Model.Playback (Playback(..))
import Music.Update.Types (Update)

init ∷ AudioNodes → ControlsPerspective
init audioNodes = { audioNodes, playback: Stopped }

update ∷ Update ControlsPerspective
update model = case _ of
  PerspectiveChanged perspective →
    pure { perspective }
  PlayRequested → do
    E.fork do
      playbackControls ← Audio.play model.audioNodes
      pure $ PlaybackStarted playbackControls
    pure { perspective: Controls model { playback = PlaybackStarting } }
  PlaybackStarted playbackControls → do
    E.forkVoid $ Audio.initializeAnalyserCanvas
      playbackControls.analyserNode
    pure
      { perspective: Controls model
          { playback = Playing playbackControls }
      }
  StopRequested → case model.playback of
    Playing playbackControls → do
      E.forkVoid $ Audio.stop playbackControls.audioContext
      pure { perspective: Controls model { playback = Stopped } }
    _ →
      pure { perspective: Controls model }
  _ →
    pure { perspective: Controls model }

