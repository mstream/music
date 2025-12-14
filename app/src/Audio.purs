module Audio (adjustControls, createControls, play, stop) where

import Prelude

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
import Model (Controls)
import Web.Event.Event (preventDefault, stopPropagation)

createControls ∷ Effect Controls
createControls = do
  ctx ← newAudioContext
  osc ← createOscillator ctx
  setOscillatorType Square osc
  startOscillator 0.0 osc
  g ← createGain ctx
  setValue 0.0 =<< gain g
  connect osc g
  connect g =<< destination ctx
  suspend ctx
  pure { ctx, g, osc }

adjustControls ∷ Controls → Number → Number → Effect Unit
adjustControls ctrls gainVal freqVal = do
  freqParam ← frequency ctrls.osc
  setValue freqVal freqParam
  -- t ← currentTime ctrls.ctx
  gainParam ← gain ctrls.g
  _ ← setValue gainVal gainParam
  pure unit

play ∷ Controls → Effect Unit
play ctrls = resume ctrls.ctx

stop ∷ Controls → Effect Unit
stop ctrls = suspend ctrls.ctx
