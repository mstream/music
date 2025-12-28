module Music.Audio (play, stop, updateAnalyserCanvas) where

import Prelude

import Audio.WebAudio.AnalyserNode
  ( frequencyBinCount
  , getByteTimeDomainData
  , setFftSize
  ) as WebAudio
import Audio.WebAudio.BaseAudioContext
  ( close
  , createAnalyser
  , createGain
  , createOscillator
  , destination
  , newAudioContext
  , resume
  , suspend
  ) as WebAudio
import Audio.WebAudio.GainNode (setGain) as WebAudio
import Audio.WebAudio.Oscillator (OscillatorType(..))
import Audio.WebAudio.Oscillator
  ( setFrequency
  , setOscillatorType
  , startOscillator
  ) as WebAudio
import Audio.WebAudio.Types (AnalyserNode, AudioContext, GainNode)
import Audio.WebAudio.Types (connect) as WebAudio
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Graphics.Canvas
  ( beginPath
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
import Music.Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  , OscillatorConf
  )
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.Playback (PlaybackControls)

play ∷ AudioNodes → Aff PlaybackControls
play audioNodes = liftEffect do
  audioContext ← createAudioContext
  gainNodes ← traverse (createNode audioContext) audioNodes
  analyserNode ← WebAudio.createAnalyser audioContext
  WebAudio.setFftSize 2048 analyserNode
  traverse_
    (\gainNode → WebAudio.connect gainNode analyserNode)
    gainNodes
  destination ← WebAudio.destination audioContext
  WebAudio.connect analyserNode destination
  WebAudio.resume audioContext
  pure { analyserNode, audioContext }
  where
  createAudioContext ∷ Effect AudioContext
  createAudioContext = do
    audioContext ← WebAudio.newAudioContext
    WebAudio.suspend audioContext
    pure audioContext

  createNode ∷ AudioContext → AudioNode → Effect GainNode
  createNode audioContext = case _ of
    Oscillator conf →
      createOscillator audioContext conf

createOscillator ∷ AudioContext → OscillatorConf → Effect GainNode
createOscillator audioContext conf = do
  oscillator ← WebAudio.createOscillator audioContext
  WebAudio.setOscillatorType Square oscillator
  WebAudio.setFrequency (Frequency.toNumber conf.frequency) oscillator
  WebAudio.startOscillator zero oscillator
  gain ← WebAudio.createGain audioContext
  WebAudio.setGain (Gain.toNumber conf.gain) gain
  WebAudio.connect oscillator gain
  pure gain

stop ∷ AudioContext → Aff Unit
stop = liftEffect <<< WebAudio.close

updateAnalyserCanvas ∷ AnalyserNode → Aff Unit
updateAnalyserCanvas analyserNode = liftEffect do
  mbCanvasEl ← getCanvasElementById "analyser"
  case mbCanvasEl of
    Just canvasEl → do
      bufferLength ← WebAudio.frequencyBinCount analyserNode
      buffer ← ArrayBuffer.empty bufferLength
      WebAudio.getByteTimeDomainData analyserNode buffer
      dim ← getCanvasDimensions canvasEl
      ctx ← getContext2D canvasEl
      let
        sliceWidth ∷ Number
        sliceWidth = dim.width / Int.toNumber bufferLength
      setFillStyle ctx "#111"
      fillRect ctx
        { height: dim.height, width: dim.width, x: zero, y: zero }
      setStrokeStyle ctx "#EEE"
      setLineWidth ctx 2.0
      beginPath ctx
      tailRecM
        ( \i →
            if i < bufferLength then do
              mbByte ← ArrayBuffer.at buffer i
              v ← case mbByte of
                Just byte →
                  pure $ Int.toNumber (UInt.toInt byte) / 128.0
                Nothing →
                  throw $ "buffer index out of bound: " <> show i
              let
                x ∷ Number
                x = Int.toNumber i * sliceWidth

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
