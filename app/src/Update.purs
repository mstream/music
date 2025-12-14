module Update (init, update) where

import Prelude

import Audio (beep)
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
updateInitialized model msg = case model.playback of
  Starting → case msg of
    PlaybackStarted intervalId →
      pure $ Initialized $ model { playback = Started intervalId }
    _ →
      pure $ Initialized model

  Started intervalId → case msg of
    StopRequested → do
      forkVoid $ liftEffect do
        suspend model.ctrls.ctx
        clearInterval intervalId
      pure $ Initialized $ model { playback = Stopped }
    _ →
      pure $ Initialized model

  Stopped → case msg of
    PlayRequested → do
      fork $ liftEffect do
        resume model.ctrls.ctx
        intervalId ← setInterval 100 (beep model.ctrls)
        pure $ PlaybackStarted intervalId
      pure $ Initialized $ model { playback = Starting }

    _ →
      pure $ Initialized model

updateUninitialized ∷ UpdateVoid
updateUninitialized = case _ of
  ControlsCreated ctrls →
    pure $ Initialized { ctrls, playback: Stopped }
  _ →
    pure Uninitialized

