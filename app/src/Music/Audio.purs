module Music.Audio
  ( initializeAnalyserCanvas
  , play
  , stop
  , updateOscillator
  ) where

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
import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , GainNode
  , OscillatorNode
  )
import Audio.WebAudio.Types (connect) as WebAudio
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable (any)
import Data.Graph as Graph
import Data.Int as Int
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Gesso (launchIn) as Gesso
import Gesso.Application (WindowMode(..), defaultBehavior) as Gesso
import Gesso.Geometry (Scalers, null) as Gesso
import Gesso.State (States) as Gesso
import Gesso.Time (Delta) as Gesso
import Graphics.Canvas as Canvas
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.Playback (PlaybackControls)

play ∷ AudioNodes → Aff PlaybackControls
play audioNodes = liftEffect do
  audioContext ← createAudioContext
  analyser ← createOutput audioContext
  oscillators ← traverse
    (createNode audioContext analyser)
    (Graph.toMap $ AudioNodes.toGraph audioNodes)
  WebAudio.resume audioContext
  pure { analyser, audioContext, oscillators }
  where
  createAudioContext ∷ Effect AudioContext
  createAudioContext = do
    audioContext ← WebAudio.newAudioContext
    WebAudio.suspend audioContext
    pure audioContext

  createNode
    ∷ AudioContext
    → AnalyserNode
    → AudioNode /\ List (AudioNodeId)
    → Effect (GainNode /\ OscillatorNode)
  createNode audioContext analyser (node /\ connectionEnds) =
    case node of
      Oscillator conf →
        createOscillator audioContext analyser conf connectionEnds

createOutput ∷ AudioContext → Effect AnalyserNode
createOutput audioContext = do
  destination ← WebAudio.destination audioContext
  analyserNode ← WebAudio.createAnalyser audioContext
  WebAudio.setFftSize 2048 analyserNode
  WebAudio.connect analyserNode destination
  pure analyserNode

createOscillator
  ∷ AudioContext
  → AnalyserNode
  → Oscillator
  → List AudioNodeId
  → Effect (GainNode /\ OscillatorNode)
createOscillator audioContext analyser conf connectionEnds = do
  oscillator ← WebAudio.createOscillator audioContext
  gain ← WebAudio.createGain audioContext
  updateOscillator gain oscillator conf
  WebAudio.startOscillator zero oscillator
  WebAudio.connect oscillator gain
  when
    (any (eq AudioNodeId.output) connectionEnds)
    (WebAudio.connect gain analyser)
  pure $ gain /\ oscillator

updateOscillator
  ∷ GainNode → OscillatorNode → Oscillator → Effect Unit
updateOscillator gain oscillator conf = do
  WebAudio.setOscillatorType oscillatorType oscillator
  WebAudio.setFrequency (Frequency.toNumber conf.frequency) oscillator
  WebAudio.setGain (Gain.toNumber conf.gain) gain
  where
  oscillatorType ∷ OscillatorType
  oscillatorType = case conf.wave of
    Wave.Sine →
      Sine
    Wave.Square →
      Square

stop ∷ AudioContext → Aff Unit
stop = liftEffect <<< WebAudio.close

initializeAnalyserCanvas ∷ AnalyserNode → Aff Unit
initializeAnalyserCanvas analyserNode = liftEffect do
  bufferLengthInBytes ← WebAudio.frequencyBinCount analyserNode
  buffer ← ArrayBuffer.empty bufferLengthInBytes
  Gesso.launchIn "#analyser"
    { name: "analyser-spectrum"
    , initialState: buffer
    , viewBox: Gesso.null
    , window: Gesso.Stretch
    , behavior: Gesso.defaultBehavior
        { render = render, update = update analyserNode }
    }

update
  ∷ AnalyserNode
  → Gesso.Delta
  → Gesso.Scalers
  → Uint8Array
  → Effect (Maybe Uint8Array)
update analyserNode _ _ buffer = do
  WebAudio.getByteTimeDomainData analyserNode buffer
  pure $ Just buffer

render
  ∷ Canvas.Context2D
  → Gesso.Delta
  → Gesso.Scalers
  → Gesso.States Uint8Array
  → Effect Unit
render ctx _ { canvas } { current: buffer } = do
  let
    bufferLengthInBytes ∷ Int
    bufferLengthInBytes = ArrayBuffer.length buffer

    sliceWidth ∷ Number
    sliceWidth = canvas.rect.width / Int.toNumber bufferLengthInBytes
  Canvas.setFillStyle ctx "#111"
  Canvas.fillRect ctx
    { height: canvas.rect.height
    , width: canvas.rect.width
    , x: zero
    , y: zero
    }
  Canvas.setStrokeStyle ctx "#EEE"
  Canvas.setLineWidth ctx 2.0
  Canvas.beginPath ctx
  tailRecM
    ( \i →
        if i < bufferLengthInBytes then do
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
            y = v * canvas.rect.height / 2.0
          if i == 0 then Canvas.moveTo ctx x y
          else Canvas.lineTo ctx x y
          pure (Loop (i + 1))
        else
          pure (Done unit)
    )
    0
  Canvas.lineTo ctx canvas.rect.width (canvas.rect.height / 2.0)
  Canvas.stroke ctx
  pure unit
