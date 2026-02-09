module Music.Update.Perspective.Controls (update) where

import Prelude

import Audio.WebAudio.Types (GainNode, OscillatorNode)
import Control.Monad.Logger.Class (class MonadLogger)
import Data.Array.NonEmpty as ArrayNE
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.List (List)
import Data.Map as Map
import Data.Natural (Natural)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Timer (clearInterval, setInterval)
import Elmish as E
import Music.Audio as Audio
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Perspective.Diagram as Diagram
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodeEntry, AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Sequencer as Sequencer
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Perspective (ControlsPerspective, Perspective)
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Model.Playback (Playback(..), PlaybackControls)
import Music.Update.Types (Update)

update
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadLogger m
  ⇒ Update m ControlsPerspective Perspective
update controlsPerspective = case _ of
  ControlsAdjusted nodeId audioNodeEntry →
    handleControlsAdjusted nodeId audioNodeEntry
  PerspectiveChanged { audioNodes, toPerspective } →
    case toPerspective of
      Code → do
        E.forkVoid $ liftAff ensurePlaybackStopped
        codePerspective ← Code.init
          $ Codec.encoder AudioNodes.stringCodec unit audioNodes
        pure $ Perspective.Code codePerspective
      Diagram → do
        E.forkVoid $ liftAff ensurePlaybackStopped
        diagramPerspective ← Diagram.init audioNodes
        pure $ Perspective.Diagram diagramPerspective
      _ →
        noop
  PlayRequested → do
    E.forks \{ dispatch } → liftAff do
      controls ← Audio.play controlsPerspective.audioNodes
      stepUpdateIntervalId ← liftEffect
        $ setInterval 250 (dispatch PlaybackTick)
      liftEffect $ dispatch $ PlaybackStarted
        { controls, step: zero, stepUpdateIntervalId }
    pure $ Perspective.Controls controlsPerspective
      { playback = PlaybackStarting }
  PlaybackStarted playing → do
    E.forkVoid $ liftAff $ Audio.initializeAnalyserCanvas
      playing.controls.analyser
    pure $ Perspective.Controls controlsPerspective
      { playback = Playing playing }

  PlaybackTick → case controlsPerspective.playback of
    Playing playing → do
      let
        newStep ∷ Natural
        newStep = playing.step + one
      E.forkVoid $ liftAff
        $ actuateSequencers playing.controls newStep
            controlsPerspective.audioNodes
      pure $ Perspective.Controls controlsPerspective
        { playback = Playing playing { step = newStep } }
    _ →
      noop
  StopRequested → case controlsPerspective.playback of
    Playing _ → do
      E.forkVoid $ liftAff ensurePlaybackStopped
      pure $ Perspective.Controls controlsPerspective
        { playback = Stopped }
    _ →
      noop
  _ →
    noop
  where
  handleControlsAdjusted
    ∷ AudioNodeId → AudioNodeEntry → Init m Perspective
  handleControlsAdjusted nodeId audioNodeEntry =
    case
      AudioNodes.updateAudioNode controlsPerspective.audioNodes nodeId
        audioNodeEntry
      of
      Left violationsById → do
        E.forkVoid
          $ Console.error
          $ "failed to update audio nodes: " <> show violationsById
        noop
      Right (updatedAudioNodes /\ connectionEnds) → do
        E.forkVoid $ liftAff case controlsPerspective.playback of
          Playing { controls } → do
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
              NoteSequencer _ →
                pure unit
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
              controls.oscillators

          _ →
            pure unit
        pure $ Perspective.Controls controlsPerspective
          { audioNodes = updatedAudioNodes }

  actuateSequencers ∷ PlaybackControls → Natural → AudioNodes → Aff Unit
  actuateSequencers controls step = AudioNodes.toMap >>> traverse_ f
    where
    f ∷ AudioNodeEntry /\ Set AudioNodeId → Aff Unit
    f ({ audioNode } /\ connectionEnds) = case audioNode of
      FrequencySequencer sequencer →
        traverse_
          ( \(_ /\ osc) → Audio.updateOscillatorFrequency osc
              (sequencer `Sequencer.valueAt` step)
          )
          (findConnectedOscillatorNodes connectionEnds)
      GainSequencer sequencer →
        traverse_
          ( \(gain /\ _) → Audio.updateOscillatorGain gain
              (sequencer `Sequencer.valueAt` step)
          )
          (findConnectedOscillatorNodes $ connectionEnds)
      _ →
        pure unit
      where
      findConnectedOscillatorNodes
        ∷ Set AudioNodeId → List (GainNode /\ OscillatorNode)
      findConnectedOscillatorNodes nodeIds = Map.values $ Map.filterKeys
        (\id → Set.member id nodeIds)
        controls.oscillators

  noop ∷ Init m Perspective
  noop = pure $ Perspective.Controls controlsPerspective

  ensurePlaybackStopped ∷ Aff Unit
  ensurePlaybackStopped = case controlsPerspective.playback of
    Playing { controls, stepUpdateIntervalId } → do
      Audio.stop controls.audioContext
      liftEffect $ clearInterval stepUpdateIntervalId
    _ →
      pure unit

