module View (view) where

import Prelude

import Audio (createControls)
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
import Model (Model(..))
import Web.Event.Event (preventDefault, stopPropagation)

type ViewVoid = Dispatch Message → ReactElement
type View m = m → Dispatch Message → ReactElement

view ∷ View Model
view model dispatch = go dispatch
  where
  go ∷ ViewVoid
  go = case model of
    Initialized initializedModel →
      viewInitialized initializedModel
    Uninitialized →
      viewUninitialized

viewUninitialized ∷ ViewVoid
viewUninitialized dispatch =
  H.div "m-4" [ H.button_ "btn m-1" { onClick } "Start" ]
  where
  onClick = handleEffect \e → do
    -- stopPropagation e
    ctrls ← createControls
    dispatch $ ControlsCreated ctrls

viewInitialized ∷ View InitializedModel
viewInitialized model dispatch =
  H.div "m-4"
    [ H.button_ "btn m-1" { onClick: dispatch <| PlayRequested }
        "Play"
    , H.button_ "btn m-1" { onClick: dispatch <| StopRequested } "Stop"
    , H.div "m-1"
        [ H.text case model.playback of
            Starting → "starting"
            Started _ → "started"
            Stopped → "stopped"
        ]
    ]
