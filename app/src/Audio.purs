module Audio (beep, createControls) where

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

beep ∷ Controls → Effect Unit
beep ctrls = do
  freqParam ← frequency ctrls.osc
  f ← getValue freqParam
  setValue 60.0 freqParam
  -- t ← currentTime ctrls.ctx
  gainParam ← gain ctrls.g
  _ ← setValue 0.1 gainParam
  pure unit
