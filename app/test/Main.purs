module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Model (AudioComponent(..), AudioNode, audioNode)
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

testCase ∷ String → AudioNode → String → Spec Unit
testCase title audioNodeExample audioNodeText = it title do
  case runParser audioNodeText audioNode of
    Left error →
      fail $ show error

    Right parsedAudioNode →
      shouldEqual audioNodeExample parsedAudioNode

spec ∷ Spec Unit
spec = describe "parsing" do
  testCase "oscillator"
    { id: "osc1"
    , component: Oscillator { frequency: 200.0, gain: 0.5 }
    }
    "osc osc1 {f=200,g=0.5}"

