module Test.Main (main) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Traversable (sequence_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Mermaid as Mermaid
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Model.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes (AudioNode(..), AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.Codec.Code as Code
import Music.Model.AudioNodes.Codec.Diagram as Diagram
import Music.Model.AudioNodes.Frequency (Frequency)
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain (Gain)
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.AudioNodes.Wave (Wave(..))
import Music.Model.PerspectiveName (PerspectiveName)
import Partial.Unsafe (unsafePartial)
import Random.LCG (mkSeed)
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.Laws (lawsTestSuite)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.QuickCheck.Laws.Data (checkEq, checkOrd, checkSemigroup)
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Proxy (Proxy(..))

foreign import mockBrowserImpl ∷ Effect Unit

main ∷ Effect Unit
main = do
  mockBrowserImpl
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: DiagramDef.codec
    , encoderOpts: unit
    , examples: diagramDefCodecExamples
    , name: "diagramDef/string"
    }
  codecTestSuite
    { codec: Code.codec
    , encoderOpts: unit
    , examples: codeCodecExamples
    , name: "audioNode/string"
    }
  codecTestSuite
    { codec: Diagram.codec
    , encoderOpts: unit
    , examples: diagramCodecExamples
    , name: "audioNode/diagramDef"
    }
  mermaidTestSuite 10
  lawsTestSuite "AudioNode" do
    let
      proxy ∷ Proxy AudioNode
      proxy = Proxy
    checkEq proxy
    checkOrd proxy
  lawsTestSuite "BlockId" do
    let
      proxy ∷ Proxy BlockId
      proxy = Proxy
    checkEq proxy
    checkOrd proxy
    checkSemigroup proxy
  lawsTestSuite "DiagramDef" do
    let
      proxy ∷ Proxy DiagramDef
      proxy = Proxy
    checkEq proxy
    checkOrd proxy
  lawsTestSuite "Frequency" do
    let
      proxy ∷ Proxy Frequency
      proxy = Proxy
    checkEq proxy
    checkOrd proxy
  lawsTestSuite "Gain" do
    let
      proxy ∷ Proxy Gain
      proxy = Proxy
    checkEq proxy
    checkOrd proxy
  lawsTestSuite "PerspectiveName" do
    let
      proxy ∷ Proxy PerspectiveName
      proxy = Proxy
    checkEq proxy
    checkOrd proxy
  lawsTestSuite "Wave" do
    let
      proxy ∷ Proxy Wave
      proxy = Proxy
    checkEq proxy
    checkOrd proxy

diagramDefCodecExamples ∷ Map DiagramDef String
diagramDefCodecExamples = Map.fromFoldable
  [ parsedBlocksDiagramDefExample /\ code
      [ "block"
      , "  osc1[\"osc{f=100.0,g=0.25,w=sine}\"]"
      , "  osc2[\"osc{f=200.0,g=0.75,w=square}\"]"
      , "  osc1 --> output"
      ]
  ]

codeCodecExamples ∷ Map AudioNodes String
codeCodecExamples = Map.fromFoldable
  [ parsedAudioNodesExample /\ code
      [ "osc1 osc{f=100.0,g=0.25,w=sine}"
      , "osc2 osc{f=200.0,g=0.75,w=square}"
      , "osc1->output"
      ]
  ]

diagramCodecExamples ∷ Map AudioNodes DiagramDef
diagramCodecExamples = Map.fromFoldable
  [ parsedAudioNodesExample /\ parsedBlocksDiagramDefExample ]

parsedBlocksDiagramDefExample ∷ DiagramDef
parsedBlocksDiagramDefExample = DiagramDef.Blocks
  $ Blocks.Def
  $ Graph.fromMap
  $
    Map.fromFoldable
      [ parsedBlockExample1
      , parsedBlockExample2
      ]

parsedBlockExample1 ∷ BlockId /\ (String /\ List BlockId)
parsedBlockExample1 =
  unsafeBlockId "osc1" /\
    ("osc{f=100.0,g=0.25,w=sine}" /\ AudioNodeId.output : Nil)

parsedBlockExample2 ∷ BlockId /\ (String /\ List BlockId)
parsedBlockExample2 =
  unsafeBlockId "osc2" /\ ("osc{f=200.0,g=0.75,w=square}" /\ Nil)

parsedAudioNodesExample ∷ AudioNodes
parsedAudioNodesExample = unsafeAudioNodes $ Graph.fromMap $
  Map.fromFoldable
    [ parsedOscillatorExample1
    , parsedOscillatorExample2
    ]

parsedOscillatorExample1
  ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
parsedOscillatorExample1 =
  unsafeAudioNodeId "osc1" /\
    ( Oscillator
        { frequency: unsafeFrequency "100.0"
        , gain: unsafeGain "0.25"
        , wave: Sine
        } /\ Cons AudioNodeId.output Nil
    )

parsedOscillatorExample2
  ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
parsedOscillatorExample2 =
  unsafeAudioNodeId "osc2" /\
    ( Oscillator
        { frequency: unsafeFrequency "200.0"
        , gain: unsafeGain "0.75"
        , wave: Square
        } /\ Nil
    )

mermaidTestSuite ∷ Int → Spec Unit
mermaidTestSuite quantity = describe
  "mermaid diagram rendering"
  (sequence_ testCases)
  where
  testCases ∷ Array (Spec Unit)
  testCases = Array.mapWithIndex testCase testInputs

  testInputs ∷ Array AudioNodes
  testInputs = Gen.evalGen
    (Gen.vectorOf quantity arbitrary)
    { newSeed: mkSeed 123, size: 100 }

  testCase ∷ Int → AudioNodes → Spec Unit
  testCase index audioNodes = it
    ("diagram " <> show index)
    ( void
        $ Mermaid.render
        $ Codec.encoder Diagram.codec unit audioNodes
    )

unsafeAudioNodes ∷ Graph AudioNodeId AudioNode → AudioNodes
unsafeAudioNodes graph = unsafePartial $
  case AudioNodes.fromGraph graph of
    Right audioNodes →
      audioNodes

unsafeAudioNodeId ∷ String → AudioNodeId
unsafeAudioNodeId = unsafeDecoded AudioNodeId.codec

unsafeBlockId ∷ String → BlockId
unsafeBlockId = unsafeDecoded BlockId.codec

unsafeFrequency ∷ String → Frequency
unsafeFrequency = unsafeDecoded Frequency.codec

unsafeGain ∷ String → Gain
unsafeGain = unsafeDecoded Gain.codec

code ∷ Array String → String
code = String.joinWith "\n"
