module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Model.AudioNode
  ( AudioComponent(..)
  , AudioNode
  , Wave(..)
  , audioNode
  , render
  )
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

codeCodecTestCase ∷ String → AudioNode → String → Spec Unit
codeCodecTestCase title parsedAudioNodeExample renderedAudioNodeExample =
  it title do
    case runParser renderedAudioNodeExample audioNode of
      Left error →
        fail $ show error

      Right parsedAudioNode → do
        shouldEqual parsedAudioNodeExample parsedAudioNode
        let
          renderedAudioNode ∷ String
          renderedAudioNode = render parsedAudioNode
        shouldEqual renderedAudioNodeExample renderedAudioNode

spec ∷ Spec Unit
spec = do
  describe "parsing" do
    codeCodecTestCase "oscillator"
      { id: "osc1"
      , component: Oscillator
          { frequency: 200.0, gain: 0.5, wave: Sine }
      }
      "osc osc1 {f=200.0,g=0.5,w=sine}"

