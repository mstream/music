module Music.Update.Perspective.Controls (update) where

import Prelude

import Audio.WebAudio.Types (GainNode, OscillatorNode)
import Control.Monad.Logger.Class (class MonadLogger)
import Data.Array.NonEmpty as ArrayNE
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.List (List)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Elmish (Transition')
import Elmish as E
import Music.Audio as Audio
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Perspective.Diagram as Diagram
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.AudioNodes (AudioNodeEntry)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Model.Playback (Playback(..))
import Music.Update.Types (Update)

update ∷ ∀ m. MonadAff m ⇒ MonadLogger m ⇒ Update m ControlsPerspective
update model = case _ of
  ControlsAdjusted nodeId audioNodeEntry →
    handleControlsAdjusted nodeId audioNodeEntry
  PerspectiveChanged { audioNodes, toPerspective } →
    case toPerspective of
      Code → do
        E.forkVoid $ liftAff ensurePlaybackStopped
        codePerspective ← Code.init
          $ Codec.encoder AudioNodes.stringCodec unit audioNodes
        pure { perspective: Perspective.Code codePerspective }
      Diagram → do
        E.forkVoid $ liftAff ensurePlaybackStopped
        diagramPerspective ← Diagram.init audioNodes
        pure { perspective: Perspective.Diagram diagramPerspective }
      _ →
        noop
  PlayRequested → do
    E.fork $ liftAff do
      playbackControls ← Audio.play model.audioNodes
      pure $ PlaybackStarted playbackControls
    pure
      { perspective: Perspective.Controls model
          { playback = PlaybackStarting }
      }
  PlaybackStarted playbackControls → do
    E.forkVoid $ liftAff $ Audio.initializeAnalyserCanvas
      playbackControls.analyser
    pure
      { perspective: Perspective.Controls model
          { playback = Playing playbackControls }
      }
  StopRequested → case model.playback of
    Playing _ → do
      E.forkVoid $ liftAff ensurePlaybackStopped
      pure
        { perspective: Perspective.Controls model { playback = Stopped }
        }
    _ →
      noop
  _ →
    noop
  where
  handleControlsAdjusted
    ∷ AudioNodeId → AudioNodeEntry → Transition' m Message Model
  handleControlsAdjusted nodeId audioNodeEntry =
    case
      AudioNodes.updateAudioNode model.audioNodes nodeId audioNodeEntry
      of
      Left violationsById → do
        E.forkVoid
          $ Console.error
          $ "failed to update audio nodes: " <> show violationsById
        noop
      Right (updatedAudioNodes /\ connectionEnds) → do
        E.forkVoid $ liftEffect case model.playback of
          Playing playbackControls → do
            case audioNodeEntry.audioNode of
              FrequencySequencer frequencySequencerConf →
                traverse_
                  ( \(_ /\ osc) → Audio.updateOscillatorFrequency osc
                      ( ArrayNE.head $ Sequence.toArray
                          frequencySequencerConf.sequence
                      )
                  )
                  (findOscillators $ Set.fromFoldable $ connectionEnds)
              GainSequencer gainSequencerConf →
                traverse_
                  ( \(gain /\ _) → Audio.updateOscillatorGain gain
                      ( ArrayNE.head $ Sequence.toArray
                          gainSequencerConf.sequence
                      )
                  )
                  (findOscillators $ Set.fromFoldable $ connectionEnds)
              Oscillator oscillatorConf →
                traverse_
                  ( \(_ /\ osc) → Audio.updateOscillatorWave osc
                      oscillatorConf.wave
                  )
                  (findOscillators $ Set.singleton nodeId)
            where
            findOscillators
              ∷ Set AudioNodeId → List (GainNode /\ OscillatorNode)
            findOscillators nodeIds = Map.values $ Map.filterKeys
              (\id → Set.member id nodeIds)
              playbackControls.oscillators

          _ →
            pure unit
        pure
          { perspective: Perspective.Controls model
              { audioNodes = updatedAudioNodes }
          }

  noop ∷ Init m Model
  noop = pure { perspective: Perspective.Controls model }

  ensurePlaybackStopped ∷ Aff Unit
  ensurePlaybackStopped = case model.playback of
    Playing playbackControls →
      Audio.stop playbackControls.audioContext
    _ →
      pure unit

