module Update (init, update) where

import Prelude

import Audio (adjustControls, play, stop)
import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext
  ( createGain
  , createOscillator
  , currentTime
  , destination
  , newAudioContext
  , resume
  , state
  , suspend
  )
import Audio.WebAudio.GainNode (gain)
import Audio.WebAudio.Oscillator
  ( OscillatorType(..)
  , frequency
  , setOscillatorType
  , startOscillator
  )
import Audio.WebAudio.Types
  ( AudioContext
  , AudioContextState(..)
  , GainNode
  , OscillatorNode
  , connect
  )
import Data.Either (Either(..))
import Data.Number (pow, round)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Timer (IntervalId, clearInterval, setInterval)
import Elmish
  ( Dispatch
  , ReactElement
  , Transition
  , fork
  , forkVoid
  , (<|)
  )
import Elmish.Boot as Boot
import Elmish.Dispatch (handleEffect)
import Elmish.HTML.Styled as H
import Message (Message(..))
import Model (Controls, InitializedModel, Model(..), PlaybackModel(..))
import Parsing (runParser)
import Parsing.String.Basic (number)
import Web.Event.Event (preventDefault, stopPropagation)

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
  in
    case model.playback of
      Starting → case msg of
        GainInputChanged s →
          handleGainInputChanged s
        FrequencyInputChanged s →
          handleFrequencyInputChanged s
        PlaybackStarted intervalId →
          pure $ Initialized $ model { playback = Started intervalId }
        _ →
          pure $ Initialized model

      Started intervalId → case msg of
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
  ControlsCreated ctrls →
    pure $ Initialized
      { ctrls
      , gain: parseGain "0.5"
      , frequency: parseFrequency "3.0"
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
