module Music.Audio
  ( initializeAnalyserCanvas
  , play
  , stop
  , updateOscillatorFrequency
  , updateOscillatorGain
  , updateOscillatorWave
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
import Audio.WebAudio.Oscillator
  ( OscillatorType(..)
  , setFrequency
  , setOscillatorType
  , startOscillator
  ) as WebAudio
import Audio.WebAudio.Types
  ( AnalyserNode
  , AudioContext
  , GainNode
  , OscillatorNode
  , connect
  ) as WebAudio
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array.NonEmpty as ArrayNE
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph as Graph
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Data.Value (codecConf)
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
import Music.Model.AudioNodes (AudioNodeEntry, AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Playback (PlaybackControls)

type ActiveOscillatorConfig =
  { frequency ∷ Frequency
  , gain ∷ Gain
  , isConnectedToOutput ∷ Boolean
  , oscillator ∷ Oscillator
  }

play ∷ AudioNodes → Aff PlaybackControls
play audioNodes = do
  audioContext ← createAudioContext
  analyser ← createOutput audioContext
  oscillators ← traverse
    ( \({ frequency, gain, isConnectedToOutput, oscillator }) →
        createOscillator audioContext analyser
          frequency
          gain
          oscillator
          isConnectedToOutput
    )
    (activeOscillatorConfigs audioNodes)
  liftEffect $ WebAudio.resume audioContext
  pure { analyser, audioContext, oscillators: oscillators }
  where
  createAudioContext ∷ Aff WebAudio.AudioContext
  createAudioContext = liftEffect do
    audioContext ← WebAudio.newAudioContext
    WebAudio.suspend audioContext
    pure audioContext

activeOscillatorConfigs
  ∷ AudioNodes → Map AudioNodeId ActiveOscillatorConfig
activeOscillatorConfigs audioNodes = foldlWithIndex
  f
  Map.empty
  (Graph.toMap $ AudioNodes.toGraph audioNodes)
  where
  f
    ∷ AudioNodeId
    → Map AudioNodeId ActiveOscillatorConfig
    → (AudioNodeEntry /\ Set AudioNodeId)
    → Map AudioNodeId ActiveOscillatorConfig
  f nodeId acc ({ audioNode, isConnectedToOutput } /\ _) =
    case audioNode of
      Oscillator oscillator → do
        case connectedFrequencyAndGainSequencers of
          Just (frequencySequencer /\ gainSequencer) →
            Map.insert nodeId
              { frequency: ArrayNE.head $ Sequence.toArray
                  frequencySequencer.sequence
              , gain: ArrayNE.head $ Sequence.toArray
                  gainSequencer.sequence
              , isConnectedToOutput
              , oscillator
              }
              acc
          Nothing →
            acc
        where
        connectedFrequencyAndGainSequencers
          ∷ Maybe (Sequencer Frequency /\ Sequencer Gain)
        connectedFrequencyAndGainSequencers = do
          frequencySequencer ← AudioNodes.connectedFrequencySequencer
            nodeId
            audioNodes
          gainSequencer ← AudioNodes.connectedGainSequencer
            nodeId
            audioNodes
          pure $ frequencySequencer /\ gainSequencer
      _ →
        acc

createOutput ∷ WebAudio.AudioContext → Aff WebAudio.AnalyserNode
createOutput audioContext = liftEffect do
  destination ← WebAudio.destination audioContext
  analyserNode ← WebAudio.createAnalyser audioContext
  WebAudio.setFftSize 2048 analyserNode
  WebAudio.connect analyserNode destination
  pure analyserNode

createOscillator
  ∷ WebAudio.AudioContext
  → WebAudio.AnalyserNode
  → Frequency
  → Gain
  → Oscillator
  → Boolean
  → Aff (WebAudio.GainNode /\ WebAudio.OscillatorNode)
createOscillator
  audioContext
  analyserNode
  frequency
  gain
  conf
  isConnectedToOutput = do
  oscillatorNode ← liftEffect $ WebAudio.createOscillator audioContext
  gainNode ← liftEffect $ WebAudio.createGain audioContext
  updateOscillatorWave oscillatorNode conf.wave
  liftEffect do
    WebAudio.setFrequency (codecConf.unwrap frequency) oscillatorNode
    WebAudio.setGain (codecConf.unwrap gain) gainNode
    WebAudio.startOscillator zero oscillatorNode
    WebAudio.connect oscillatorNode gainNode
    when
      isConnectedToOutput
      (WebAudio.connect gainNode analyserNode)
  pure $ gainNode /\ oscillatorNode

updateOscillatorFrequency
  ∷ WebAudio.OscillatorNode → Frequency → Aff Unit
updateOscillatorFrequency oscillatorNode frequency = liftEffect
  $ WebAudio.setFrequency (codecConf.unwrap frequency) oscillatorNode

updateOscillatorGain
  ∷ WebAudio.GainNode → Gain → Aff Unit
updateOscillatorGain gainNode gain = liftEffect
  $ WebAudio.setGain (codecConf.unwrap gain) gainNode

updateOscillatorWave
  ∷ WebAudio.OscillatorNode → Wave → Aff Unit
updateOscillatorWave oscillatorNode wave = liftEffect do
  WebAudio.setOscillatorType oscillatorType oscillatorNode
  where
  oscillatorType ∷ WebAudio.OscillatorType
  oscillatorType = case wave of
    Sine →
      WebAudio.Sine
    Square →
      WebAudio.Square

stop ∷ WebAudio.AudioContext → Aff Unit
stop = liftEffect <<< WebAudio.close

initializeAnalyserCanvas ∷ WebAudio.AnalyserNode → Aff Unit
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
  ∷ WebAudio.AnalyserNode
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
