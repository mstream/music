module Update (init, update) where

import Prelude

import Audio (adjustControls, play, stop)
import Data.Codec (encoder)
import Data.Diagram as Diagram
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Number (pow, round)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Timer (clearInterval, setInterval)
import Elmish (Transition, fork, forkVoid)
import Mermaid as Mermaid
import Message (Message(..))
import Model (InitializedModel, Model(..), PlaybackModel(..))
import Model.AudioNode
  ( AudioNode(..)
  , AudioNodeId
  , AudioNodes
  , Wave(..)
  , dummyAudioNodeId1
  , dummyAudioNodeId2
  )
import Parsing (runParser)
import Parsing.String.Basic (number)

type Update m = m → Message → Transition Message Model
type UpdateVoid = Message → Transition Message Model

init ∷ Transition Message Model
init = pure Uninitialized

update ∷ Update Model
update = case _ of
  Initialized initializedModel →
    updateInitialized initializedModel
  Uninitialized →
    updateUninitialized

updateInitialized ∷ Update InitializedModel
updateInitialized model msg =
  let
    handleGainInputChanged
      ∷ String → Transition Message Model
    handleGainInputChanged s = do
      let
        newModel ∷ InitializedModel
        newModel = model { gain = parseGain s }
      forkVoid $ liftEffect $ adjustControls
        newModel.ctrls
        newModel.gain
        newModel.frequency
      pure $ Initialized newModel

    handleFrequencyInputChanged
      ∷ String → Transition Message Model
    handleFrequencyInputChanged s = do
      let
        newModel ∷ InitializedModel
        newModel = model { frequency = parseFrequency s }
      forkVoid $ liftEffect $ adjustControls
        newModel.ctrls
        newModel.gain
        newModel.frequency
      pure $ Initialized newModel

    handleDiagramRendered ∷ String → Transition Message Model
    handleDiagramRendered s = pure $ Initialized $ model
      { nodesDiagramSvg = Just s }
  in
    case model.playback of
      Starting → case msg of
        DiagramRendered s →
          handleDiagramRendered s
        GainInputChanged s →
          handleGainInputChanged s
        FrequencyInputChanged s →
          handleFrequencyInputChanged s
        PlaybackStarted intervalId →
          pure $ Initialized $ model { playback = Started intervalId }
        _ →
          pure $ Initialized model

      Started intervalId → case msg of
        DiagramRendered s →
          handleDiagramRendered s
        GainInputChanged s →
          handleGainInputChanged s
        FrequencyInputChanged s →
          handleFrequencyInputChanged s
        StopRequested → do
          forkVoid $ liftEffect do
            stop model.ctrls
            clearInterval intervalId
          pure $ Initialized $ model { playback = Stopped }
        _ →
          pure $ Initialized model

      Stopped → case msg of
        DiagramRendered s →
          handleDiagramRendered s
        GainInputChanged s →
          handleGainInputChanged s
        FrequencyInputChanged s →
          handleFrequencyInputChanged s
        PlayRequested → do
          fork $ liftEffect do
            adjustControls model.ctrls model.gain model.frequency
            play model.ctrls
            intervalId ← setInterval 100 (pure unit)
            pure $ PlaybackStarted intervalId
          pure $ Initialized $ model { playback = Starting }

        _ →
          pure $ Initialized model

updateUninitialized ∷ UpdateVoid
updateUninitialized = case _ of
  ControlsCreated ctrls → do
    let
      nodes ∷ AudioNodes
      nodes = dummyNodes

      renderDiagramDef ∷ AudioNodes → String
      renderDiagramDef = encoder Diagram.codec unit

    fork do
      diagramSvg ← Mermaid.render $ renderDiagramDef nodes

      pure $ DiagramRendered diagramSvg

    pure $ Initialized
      { ctrls
      , gain: parseGain "0.5"
      , frequency: parseFrequency "3.0"
      , nodes
      , nodesDiagramSvg: Nothing
      , playback: Stopped
      }
  _ →
    pure Uninitialized

parseGain ∷ String → Number
parseGain s = case runParser s number of
  Left _ →
    zero
  Right x →
    x

parseFrequency ∷ String → Number
parseFrequency s = case runParser s number of
  Left _ →
    zero
  Right x →
    round $ pow 10.0 x

dummyNodes ∷ Map AudioNodeId AudioNode
dummyNodes =
  fromFoldable
    [ dummyAudioNodeId1 /\ Oscillator
        { frequency: 100.0, gain: 1.0, wave: Sine }
    , dummyAudioNodeId2 /\ Oscillator
        { frequency: 200.0, gain: 0.5, wave: Sine }
    ]
