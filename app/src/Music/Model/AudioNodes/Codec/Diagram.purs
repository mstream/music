module Music.Model.AudioNodes.Codec.Diagram (groupBlockCodec) where

import Prelude

import Control.Alt ((<|>))
import Data.Codec as Codec
import Data.Either (Either(..), note)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (fst)
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
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId, output)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Parsing (ParseState(..), parseErrorMessage, runParser)
import Parsing as P
import Parsing.Combinators (optional) as PC
import Parsing.String (string) as PS

groupBlockCodec ∷ AudioNodesCodec GroupBlock Unit
groupBlockCodec = Codec.codec groupBlockDecoder groupBlockEncoder

groupBlockDecoder ∷ AudioNodesDecoder GroupBlock
groupBlockDecoder = do
  ParseState input _ _ ← P.getParserT
  case toAudioNodes input of
    Left err →
      P.fail err
    Right audioNodes →
      pure audioNodes

groupBlockEncoder ∷ AudioNodesEncoder GroupBlock Unit
groupBlockEncoder _ audioNodes =
  { children: Graph.fromMap topChildren
  , properties: { columns: Just C1 }
  , spacedOut
  }
  where
  graphMap ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  graphMap = Graph.toMap (AudioNodes.toGraph audioNodes)

  oscillatorChildren
    ∷ Map BlockId (BlockDef /\ List BlockId)
  oscillatorChildren = Map.fromFoldable
    ( renderNode
        <$>
          ( Map.toUnfoldable graphMap
              ∷ Array
                  (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
          )
    )

  hasOscillators ∷ Boolean
  hasOscillators = not $ Map.isEmpty oscillatorChildren

  oscillatorGroup ∷ GroupBlock
  oscillatorGroup =
    { children: Graph.fromMap oscillatorChildren
    , properties: { columns: Nothing }
    , spacedOut: false
    }

  topChildren ∷ Map BlockId (BlockDef /\ List BlockId)
  topChildren = Map.fromFoldable
    (oscillatorsEntry <> [ outputEntry ])

  oscillatorsEntry
    ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
  oscillatorsEntry =
    if hasOscillators then
      [ AudioNodeId.oscillators /\ (Group oscillatorGroup /\ Nil) ]
    else
      []

  outputEntry ∷ BlockId /\ (BlockDef /\ List BlockId)
  outputEntry = output /\ (Node "output" /\ Nil)

  spacedOut ∷ Boolean
  spacedOut = Map.size topChildren > 1

  renderNode
    ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
    → BlockId /\ (BlockDef /\ List BlockId)
  renderNode (nodeId /\ (node /\ ends)) = case node of
    Oscillator conf →
      nodeId /\ (Node (renderOscillator conf) /\ ends)

renderOscillator
  ∷ { frequency ∷ Frequency.Frequency
    , gain ∷ Gain.Gain
    , wave ∷ Wave.Wave
    }
  → String
renderOscillator { frequency, gain, wave } = String.joinWith " "
  [ Codec.encoder Frequency.stringCodec unit frequency
  , Codec.encoder Gain.stringCodec unit gain
  , Codec.encoder Wave.stringCodec unit wave
  ]

toAudioNodes ∷ GroupBlock → Either String AudioNodes
toAudioNodes groupBlock = do
  outputPresent ← note "Missing output node"
    (Map.lookup AudioNodeId.output topChildren)
  case fst outputPresent of
    Node _ →
      pure unit
    _ →
      Left "Output must be a node"
  oscChildren ← extractOscillators
  parsed ← traverse parseEntry
    ( Map.toUnfoldable oscChildren
        ∷ Array (AudioNodeId /\ (BlockDef /\ List BlockId))
    )
  let
    nodes = Map.fromFoldable parsed
    graph = Graph.fromMap nodes
  case AudioNodes.fromGraph graph of
    Left err →
      Left err
    Right audioNodes →
      Right audioNodes
  where
  topChildren ∷ Map BlockId (BlockDef /\ List BlockId)
  topChildren = Graph.toMap groupBlock.children

  extractOscillators
    ∷ Either String
        (Map AudioNodeId (BlockDef /\ List BlockId))
  extractOscillators =
    case
      Map.lookup AudioNodeId.oscillators topChildren
      of
      Just (Group oscGroup /\ _) →
        Right $ Graph.toMap oscGroup.children
      Just _ →
        Left "Oscillators entry must be a group"
      Nothing →
        Right $ Map.filterWithKey
          (\key _ → key /= AudioNodeId.output)
          (Map.filter isNode topChildren)

  isNode ∷ BlockDef /\ List BlockId → Boolean
  isNode = case _ of
    Node _ /\ _ →
      true
    _ →
      false

  parseEntry
    ∷ AudioNodeId /\ (BlockDef /\ List BlockId)
    → Either String (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  parseEntry (nodeId /\ (blockDef /\ ends)) = case blockDef of
    Node contents →
      parseOscillator contents
        >>= \node →
          Right $ nodeId /\ (node /\ ends)
    Group _ →
      Left "Oscillator must be a node"

parseOscillator ∷ String → Either String AudioNode
parseOscillator contents = case runParser contents oscillatorParser of
  Left err →
    Left $ parseErrorMessage err
  Right node →
    Right node

oscillatorParser ∷ P.Parser String AudioNode
oscillatorParser = do
  frequency ← Codec.decoder Frequency.stringCodec
  _ ← PS.string " "
  gain ← Codec.decoder Gain.stringCodec
  _ ← PS.string " "
  wave ← Codec.decoder Wave.stringCodec
  pure $ Oscillator { frequency, gain, wave }

waveValue ∷ P.Parser String Wave.Wave
waveValue =
  PS.string "sine" $> Wave.Sine
    <|> PS.string "square" $> Wave.Square
