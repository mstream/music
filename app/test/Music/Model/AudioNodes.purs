module Test.Music.Model.AudioNodes (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes.Codec.Code as Code
import Music.Model.AudioNodes.Codec.Diagram as Diagram
import Partial.Unsafe (unsafePartial)
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.Laws (lawsTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockId (unsafeBlockId)
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec)
import Test.Utils (lines)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Code.codec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ parsedEmptyAudioNodesExample /\ lines [ "" ]
        , parsedAudioNodesExample /\ lines
            [ "osc1 osc{f=100.0,g=0.25,w=sine}"
            , "osc2 osc{f=200.0,g=0.75,w=square}"
            , "osc3 osc{f=400.0,g=0.5,w=sine}"
            , "osc1->output"
            , "osc3->output"
            ]
        ]
    , name: "audioNode/string"
    }
  codecTestSuite
    { codec: Diagram.groupBlockCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ parsedEmptyAudioNodesExample /\
            parsedEmptyBlocksDiagramDefExample
        , parsedAudioNodesExample /\
            parsedBlocksDiagramDefExample
        ]
    , name: "audioNode/groupBlock"
    }
  lawsTestSuite "AudioNode" do
    let
      proxy ∷ Proxy AudioNode
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
  lawsTestSuite "Wave" do
    let
      proxy ∷ Proxy Wave
      proxy = Proxy
    checkEq proxy
    checkOrd proxy

parsedEmptyBlocksDiagramDefExample ∷ GroupBlock
parsedEmptyBlocksDiagramDefExample =
  { children: Graph.fromMap $ Map.fromFoldable
      [ outputNodeId /\ outputNode
      ]
  , properties: { columns: Just C1 }
  , spacedOut: false
  }
  where

  outputNode ∷ BlockDef /\ List BlockId
  outputNode = Node "output" /\ Nil

  outputNodeId ∷ BlockId
  outputNodeId = unsafeBlockId "output"

parsedBlocksDiagramDefExample ∷ GroupBlock
parsedBlocksDiagramDefExample =
  { children: Graph.fromMap $ Map.fromFoldable
      [ oscillatorsGroupId /\ oscillatorsGroup
      , outputNodeId /\ outputNode
      ]
  , properties: { columns: Just C1 }
  , spacedOut: true
  }
  where
  oscillatorsGroup ∷ BlockDef /\ List BlockId
  oscillatorsGroup =
    Group
      { children: Graph.fromMap $ Map.fromFoldable
          [ oscId1 /\ oscNode1
          , oscId2 /\ oscNode2
          , oscId3 /\ oscNode3
          ]
      , properties: { columns: Nothing }
      , spacedOut: false
      }
      /\ Nil

  oscNode1 ∷ BlockDef /\ List BlockId
  oscNode1 = Node "f=100.0 g=0.25 w=sine" /\ outputNodeId : Nil

  oscNode2 ∷ BlockDef /\ List BlockId
  oscNode2 = Node "f=200.0 g=0.75 w=square" /\ Nil

  oscNode3 ∷ BlockDef /\ List BlockId
  oscNode3 = Node "f=400.0 g=0.5 w=sine" /\ outputNodeId : Nil

  outputNode ∷ BlockDef /\ List BlockId
  outputNode = Node "output" /\ Nil

  oscillatorsGroupId ∷ BlockId
  oscillatorsGroupId = unsafeBlockId "oscillators"

  outputNodeId ∷ BlockId
  outputNodeId = unsafeBlockId "output"

  oscId1 ∷ BlockId
  oscId1 = unsafeBlockId "osc1"

  oscId2 ∷ BlockId
  oscId2 = unsafeBlockId "osc2"

  oscId3 ∷ BlockId
  oscId3 = unsafeBlockId "osc3"

parsedEmptyAudioNodesExample ∷ AudioNodes
parsedEmptyAudioNodesExample = unsafeAudioNodes Graph.empty

parsedAudioNodesExample ∷ AudioNodes
parsedAudioNodesExample = unsafeAudioNodes $ Graph.fromMap $
  Map.fromFoldable
    [ parsedOscillatorExample1
    , parsedOscillatorExample2
    , parsedOscillatorExample3
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

parsedOscillatorExample3
  ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
parsedOscillatorExample3 =
  unsafeAudioNodeId "osc3" /\
    ( Oscillator
        { frequency: unsafeFrequency "400"
        , gain: unsafeGain "0.5"
        , wave: Sine
        } /\ Nil
    )

unsafeAudioNodes ∷ Graph AudioNodeId AudioNode → AudioNodes
unsafeAudioNodes graph = unsafePartial $
  case AudioNodes.fromGraph graph of
    Right audioNodes →
      audioNodes

unsafeAudioNodeId ∷ String → AudioNodeId
unsafeAudioNodeId = unsafeDecoded AudioNodeId.codec

unsafeFrequency ∷ String → Frequency
unsafeFrequency = unsafeDecoded Frequency.stringCodec

unsafeGain ∷ String → Gain
unsafeGain = unsafeDecoded Gain.stringCodec

