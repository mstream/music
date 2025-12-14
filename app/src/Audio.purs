module Audio (adjustControls, createControls, play, stop) where

import Prelude

import Audio.WebAudio.AnalyserNode
  ( frequencyBinCount
  , getByteTimeDomainData
  , setFftSize
  )
import Audio.WebAudio.AudioParam (getValue, setValue, setValueAtTime)
import Audio.WebAudio.BaseAudioContext
  ( createAnalyser
  , createGain
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
import Control.Monad.Rec.Class
  ( Step(..)
  , loop2
  , tailRec
  , tailRec2
  , tailRecM
  , tailRecM2
  , untilJust
  , whileJust
  )
import Data.ArrayBuffer.Typed as AB
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.UInt (toInt)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
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
import Graphics.Canvas
  ( beginPath
  , fillPath
  , fillRect
  , getCanvasDimensions
  , getCanvasElementById
  , getContext2D
  , lineTo
  , moveTo
  , setFillStyle
  , setLineWidth
  , setStrokeStyle
  , stroke
  )
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
  anal ← createAnalyser ctx
  setFftSize 2048 anal
  connect osc g
  connect g anal
  connect anal =<< destination ctx
  suspend ctx
  pure { anal, ctx, g, osc }

adjustControls ∷ Controls → Number → Number → Effect Unit
adjustControls ctrls gainVal freqVal = do
  freqParam ← frequency ctrls.osc
  setValue freqVal freqParam
  -- t ← currentTime ctrls.ctx
  gainParam ← gain ctrls.g
  _ ← setValue gainVal gainParam

  mbCanvasEl ← getCanvasElementById "analyzer"

  case mbCanvasEl of
    Just canvasEl → do
      bufferLength ← frequencyBinCount ctrls.anal
      buffer ← AB.empty bufferLength
      getByteTimeDomainData ctrls.anal buffer

      dim ← getCanvasDimensions canvasEl
      ctx ← getContext2D canvasEl

      let
        sliceWidth ∷ Number
        sliceWidth = dim.width / toNumber bufferLength

      setFillStyle ctx "#111"
      fillRect ctx
        { height: dim.height, width: dim.width, x: zero, y: zero }
      setStrokeStyle ctx "#EEE"
      setLineWidth ctx 2.0

      beginPath ctx

      tailRecM
        ( \i →
            if i < bufferLength then do
              mbByte ← AB.at buffer i

              v ← case mbByte of
                Just byte →
                  pure $ toNumber (toInt byte) / 128.0
                Nothing →
                  throw $ "buffer index out of bound: " <> show i

              let
                x ∷ Number
                x = toNumber i * sliceWidth

                y ∷ Number
                y = v * dim.height / 2.0

              if i == 0 then moveTo ctx x y
              else lineTo ctx x y

              pure (Loop (i + 1))
            else
              pure (Done unit)
        )
        0

      lineTo ctx dim.width (dim.height / 2.0)
      stroke ctx

      pure unit
    Nothing →
      throw "no analyser canvas found"

play ∷ Controls → Effect Unit
play ctrls = resume ctrls.ctx

stop ∷ Controls → Effect Unit
stop ctrls = suspend ctrls.ctx
