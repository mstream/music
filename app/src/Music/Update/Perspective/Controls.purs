module Music.Update.Perspective.Controls (update) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Elmish as E
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Audio as Audio
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Perspective.Diagram as Diagram
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Model.Playback (Playback(..))
import Music.Update.Types (Update)

update ∷ Update ControlsPerspective
update model = case _ of
  ControlsAdjusted nodeId node →
    case AudioNodes.updateAudioNode model.audioNodes nodeId node of
      Left violationsById → do
        E.forkVoid
          $ Console.error
          $ "failed to update audio nodes: " <> show violationsById
        noop
      Right updatedAudioNodes → do
        E.forkVoid $ liftEffect case model.playback of
          Playing playbackControls →
            case node of
              Oscillator oscillatorConf →
                case Map.lookup nodeId playbackControls.oscillators of
                  Just (gain /\ oscillator) →
                    Audio.updateOscillator
                      gain
                      oscillator
                      oscillatorConf
                  Nothing →
                    Console.error $ "no such oscillator: "
                      <> Codec.encoder BlockId.stringCodec unit nodeId
              _ →
                pure unit
          _ →
            pure unit
        pure
          { perspective: Perspective.Controls model
              { audioNodes = updatedAudioNodes }
          }
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
      playbackControls.analyser
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
