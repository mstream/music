module Test.Main (main) where

import Prelude

import Data.Code as Code
import Data.Codec (decoder, encoder)
import Data.Codec.AudioNodes (AudioNodesCodec)
import Data.Diagram as Diagram
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map, fromFoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Model.AudioNode
  ( AudioNode(..)
  , AudioNodeId
  , AudioNodes
  , Wave(..)
  , dummyAudioNodeId1
  , dummyAudioNodeId2
  )
import Parsing (ParseError, runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Code.codec
    , examples:
        fromFoldable
          [ parsedAudioNodesExample /\
              "dummy1 osc{f=200.0,g=0.5,w=sine}\ndummy2 osc{f=200.0,g=0.5,w=sine}"
          ]
    , name: "code"
    }
  codecTestSuite
    { codec: Diagram.codec
    , examples:
        fromFoldable
          [ parsedAudioNodesExample /\
              "block\n  dummy1[\"osc{f=200.0,g=0.5,w=sine}\"]\n  dummy2[\"osc{f=200.0,g=0.5,w=sine}\"]"
          ]
    , name: "diagram"
    }

type CodeTestSuiteConf =
  { codec ∷ AudioNodesCodec Unit
  , examples ∷ Map AudioNodes String
  , name ∷ String
  }

codecTestSuite ∷ CodeTestSuiteConf → Spec Unit
codecTestSuite { codec, examples, name } = describe
  (name <> " codec")
  (traverseWithIndex_ testCase examples)
  where
  parse ∷ String → ParseError \/ AudioNodes
  parse s = runParser s (decoder codec)

  render ∷ AudioNodes → String
  render = (encoder codec) unit

  testCase ∷ AudioNodes → String → Spec Unit
  testCase parsedExample renderedExample = it "roundtrips"
    ( case parse renderedExample of
        Left parseError →
          fail $ show parseError
        Right parsed → do
          shouldEqual parsedExample parsed
          shouldEqual renderedExample (render parsed)
    )

parsedAudioNodesExample ∷ AudioNodes
parsedAudioNodesExample = fromFoldable
  [ parsedOscillatorExample1
  , parsedOscillatorExample2
  ]

parsedOscillatorExample1 ∷ AudioNodeId /\ AudioNode
parsedOscillatorExample1 = dummyAudioNodeId1 /\ Oscillator
  { frequency: 200.0, gain: 0.5, wave: Sine }

parsedOscillatorExample2 ∷ AudioNodeId /\ AudioNode
parsedOscillatorExample2 = dummyAudioNodeId2 /\ Oscillator
  { frequency: 200.0, gain: 0.5, wave: Sine }
