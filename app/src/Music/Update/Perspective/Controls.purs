module Music.Update.Perspective.Controls (update) where

import Prelude

import Effect.Aff (Aff)
import Elmish as E
import Music.Audio as Audio
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Perspective.Diagram as Diagram
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Perspective as Perspective
import Music.Model.PerspectiveName (PerspectiveName(..))
import Music.Model.Playback (Playback(..))
import Music.Update.Types (Update)

update ∷ Update ControlsPerspective
update model = case _ of
  PerspectiveChanged { audioNodes, toPerspective } →
    case toPerspective of
      Code → do
        E.forkVoid ensurePlaybackStopped
        codePerspective ← Code.init audioNodes
        pure { perspective: Perspective.Code codePerspective }
      Diagram → do
        E.forkVoid ensurePlaybackStopped
        diagramPerspective ← Diagram.init audioNodes
        pure { perspective: Perspective.Diagram diagramPerspective }
      _ →
        noop
  PlayRequested → do
    E.fork do
      playbackControls ← Audio.play model.audioNodes
      pure $ PlaybackStarted playbackControls
    pure
      { perspective: Perspective.Controls model
          { playback = PlaybackStarting }
      }
  PlaybackStarted playbackControls → do
    E.forkVoid $ Audio.initializeAnalyserCanvas
      playbackControls.analyserNode
    pure
      { perspective: Perspective.Controls model
          { playback = Playing playbackControls }
      }
  StopRequested → case model.playback of
    Playing _ → do
      E.forkVoid ensurePlaybackStopped
      pure
        { perspective: Perspective.Controls model { playback = Stopped }
        }
    _ →
      noop
  _ →
    noop
  where
  noop ∷ Init Model
  noop = pure { perspective: Perspective.Controls model }

  ensurePlaybackStopped ∷ Aff Unit
  ensurePlaybackStopped = case model.playback of
    Playing playbackControls →
      Audio.stop playbackControls.audioContext
    _ →
      pure unit
