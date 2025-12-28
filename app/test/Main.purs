module Test.Main (main) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map, fromFoldable)
import Data.Traversable (sequence_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Music.Gen (arbitraryMap) as Gen
import Mermaid as Mermaid
import Music.Model.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  )
import Music.Model.AudioNodes.Codec (AudioNodesCodec)
import Music.Model.AudioNodes.Codec.Code as Code
import Music.Model.AudioNodes.Codec.Diagram as Diagram
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.AudioNodes.Wave (Wave(..))
import Parsing (ParseError, runParser)
import Partial.Unsafe (unsafePartial)
import Random.LCG (mkSeed)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

foreign import mockBrowserImpl ∷ Effect Unit

main ∷ Effect Unit
main = do
  mockBrowserImpl
  runSpecAndExitProcess [ consoleReporter ] spec

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
              ( unsafePartial $ Diagram.fromString
                  "block\n  dummy1[\"osc{f=200.0,g=0.5,w=sine}\"]\n  dummy2[\"osc{f=200.0,g=0.5,w=sine}\"]"
              )
          ]
    , name: "diagram"
    }
  mermaidTestSuite 10

type CodeTestSuiteConf e =
  { codec ∷ AudioNodesCodec e Unit
  , examples ∷ Map AudioNodes e
  , name ∷ String
  }

codecTestSuite ∷ ∀ e. Eq e ⇒ Show e ⇒ CodeTestSuiteConf e → Spec Unit
codecTestSuite { codec, examples, name } = describe
  (name <> " codec")
  (traverseWithIndex_ testCase examples)
  where
  parse ∷ e → ParseError \/ AudioNodes
  parse s = runParser s (Codec.decoder codec)

  render ∷ AudioNodes → e
  render = (Codec.encoder codec) unit

  testCase ∷ AudioNodes → e → Spec Unit
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
parsedOscillatorExample1 = unsafePartial $
  AudioNodeId.fromString "dummy1" /\ Oscillator
    { frequency: Frequency.fromNumber 200.0
    , gain: Gain.fromNumber 0.5
    , wave: Sine
    }

parsedOscillatorExample2 ∷ AudioNodeId /\ AudioNode
parsedOscillatorExample2 = unsafePartial $
  AudioNodeId.fromString "dummy2" /\ Oscillator
    { frequency: Frequency.fromNumber 200.0
    , gain: Gain.fromNumber 0.5
    , wave: Sine
    }

mermaidTestSuite ∷ Int → Spec Unit
mermaidTestSuite quantity = describe
  "mermaid diagram rendering"
  (sequence_ testCases)
  where
  testCases ∷ Array (Spec Unit)
  testCases = Array.mapWithIndex testCase testInputs

  testInputs ∷ Array AudioNodes
  testInputs = Gen.evalGen
    (Gen.vectorOf quantity Gen.arbitraryMap)
    { newSeed: mkSeed 123, size: 100 }

  testCase ∷ Int → AudioNodes → Spec Unit
  testCase index audioNodes = it
    ("diagram " <> show index)
    ( void
        $ Mermaid.render
        $ Codec.encoder Diagram.codec unit audioNodes
    )

