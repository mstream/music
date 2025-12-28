module Audio (play, stop) where

import Prelude

import Audio.WebAudio.BaseAudioContext
  ( close
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
import Audio.WebAudio.Types (AudioContext, GainNode)
import Audio.WebAudio.Types (connect) as WebAudio
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  , OscillatorConf
  )
import Model.AudioNodes.Frequency as Frequency
import Model.AudioNodes.Gain as Gain

play ∷ AudioNodes → Aff AudioContext
play audioNodes = liftEffect do
  audioContext ← createAudioContext
  destination ← WebAudio.destination audioContext
  gainNodes ← traverse (createNode audioContext) audioNodes
  traverse_
    (\gainNode → WebAudio.connect gainNode destination)
    gainNodes
  WebAudio.resume audioContext
  pure audioContext
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
