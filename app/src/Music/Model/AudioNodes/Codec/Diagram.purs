module Music.Model.AudioNodes.Codec.Diagram (groupBlockCodec) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..), note)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Music.Model.AudioNodes (AudioNode(..), AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId, output)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.AudioNodes.Wave as Wave
import Parsing (ParseState(..), parseErrorMessage, runParser)
import Parsing as P

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
        <$> ( Map.toUnfoldable graphMap
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
    ( oscillatorsEntry <> [ outputEntry ] )

  oscillatorsEntry
    ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
  oscillatorsEntry = if hasOscillators then
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

renderOscillator ∷
  { frequency ∷ Frequency.Frequency
  , gain ∷ Gain.Gain
  , wave ∷ Wave.Wave
  }
  → String
renderOscillator { frequency, gain, wave } =
  "f="
    <> Codec.encoder Frequency.codec unit frequency
    <> " g="
    <> Codec.encoder Gain.codec unit gain
    <> " s="
    <> Codec.encoder Wave.codec unit wave

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
  extractOscillators = case
    Map.lookup AudioNodeId.oscillators topChildren of
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
  parseEntry (nodeId /\ (blockDef /\ ends)) = do
    oscillator ← case blockDef of
      Node label →
        parseOscillator label
      Group _ →
        Left "Nested groups are not audio nodes"
    Right (nodeId /\ (oscillator /\ ends))

parseOscillator ∷ String → Either String AudioNode
parseOscillator text = do
  parts ← splitParts text
  frequency ← decode Frequency.codec =<< expectPrefix "f=" parts.f
  gain ← decode Gain.codec =<< expectPrefix "g=" parts.g
  wave ← decode Wave.codec =<< expectPrefix "s=" parts.s
  Right $ Oscillator { frequency, gain, wave }
  where
  decode
    ∷ ∀ a
    . Codec.Codec a String Unit
    → String
    → Either String a
  decode codec value = case runParser value (Codec.decoder codec) of
    Left parseError →
      Left $ parseErrorMessage parseError
    Right parsed →
      Right parsed

  expectPrefix ∷ String → String → Either String String
  expectPrefix prefix value = case
    String.stripPrefix (Pattern prefix) value of
    Just remainder →
      Right remainder
    Nothing →
      Left $ "Missing " <> prefix

  splitParts
    ∷ String
    → Either String { f ∷ String, g ∷ String, s ∷ String }
  splitParts raw = case Array.filter
    (not <<< String.null)
    tokens of
    [ fPart, gPart, sPart ] →
      Right { f: fPart, g: gPart, s: sPart }
    _ →
      Left "Oscillator line malformed"
    where
    tokens ∷ Array String
    tokens = String.split (Pattern " ") raw
