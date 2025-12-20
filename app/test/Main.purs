module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Model.AudioNode
  ( AudioComponent(..)
  , AudioNode
  , Wave(..)
  )
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import View.Code as Code
import View.Diagram as Diagram

main ∷ Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

codeCodecTestCase ∷ String → AudioNode → String → Spec Unit
codeCodecTestCase title parsedAudioNodeExample renderedAudioNodeExample =
  it title do
    case runParser renderedAudioNodeExample Code.audioNode of
      Left error →
        fail $ show error

      Right parsedAudioNode → do
        shouldEqual parsedAudioNodeExample parsedAudioNode
        let
          renderedAudioNode ∷ String
          renderedAudioNode = Code.render parsedAudioNode
        shouldEqual renderedAudioNodeExample renderedAudioNode

diagramCodecTestCase ∷ String → AudioNode → String → Spec Unit
diagramCodecTestCase
  title
  parsedAudioNodeExample
  renderedAudioNodeExample =
  it title do
    case runParser renderedAudioNodeExample Diagram.audioNode of
      Left error →
        fail $ show error

      Right parsedAudioNode → do
        shouldEqual parsedAudioNodeExample parsedAudioNode
        let
          renderedAudioNode ∷ String
          renderedAudioNode = Diagram.render parsedAudioNode
        shouldEqual renderedAudioNodeExample renderedAudioNode

parsedOscillatorExample ∷ AudioNode
parsedOscillatorExample =
  { id: "osc1"
  , component: Oscillator
      { frequency: 200.0, gain: 0.5, wave: Sine }
  }

spec ∷ Spec Unit
spec = do
  describe "code codec" do
    codeCodecTestCase "oscillator"
      parsedOscillatorExample
      "osc osc1 {f=200.0,g=0.5,w=sine}"
  describe "diagram codec" do
    diagramCodecTestCase "oscillator"
      parsedOscillatorExample
      "osc1[\"osc{f=200.0,g=0.5,w=sine}\"]"
