module Music.Model.AudioNodes
  ( AudioNodes
  , connections
  , empty
  , fromGraph
  , groupBlockCodec
  , nodesById
  , stringCodec
  , toGraph
  , updateAudioNode
  ) where

import Prelude

import Data.Array as Array
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Edge, Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (all, traverse)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Gen as Gen
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Model.AudioNodes.AudioNode (AudioNode)
import Music.Model.AudioNodes.AudioNode as AudioNode
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Parsing (ParseState(..), fail, getParserT) as P
import Parsing (parseErrorMessage, runParser)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype AudioNodes = AudioNodes (Graph AudioNodeId AudioNode)

derive newtype instance Eq AudioNodes
derive newtype instance Ord AudioNodes
derive newtype instance Show AudioNodes

instance Arbitrary AudioNodes where
  arbitrary = do
    nodes ← Gen.arbitraryMap
    let
      normalizedNodes ∷ Map AudioNodeId AudioNode
      normalizedNodes = Map.fromFoldable $ canonicalizeEntry
        <$> (Map.toUnfoldable nodes ∷ Array (AudioNodeId /\ AudioNode))

      graph ∷ Graph AudioNodeId AudioNode
      graph = Graph.fromMap (map withNoConnections normalizedNodes)
    pure $ case fromGraph graph of
      Right audioNodes →
        audioNodes
      Left _ →
        AudioNodes graph
    where
    canonicalizeEntry
      ∷ AudioNodeId /\ AudioNode
      → AudioNodeId /\ AudioNode
    canonicalizeEntry (nodeId /\ node) =
      canonicalizeId nodeId /\ node

    canonicalizeId ∷ AudioNodeId → AudioNodeId
    canonicalizeId nodeId = case prefixInfo of
      Nothing /\ true →
        case
          runParser ("def-" <> idString)
            (Codec.decoder BlockId.stringCodec)
          of
          Right prefixed →
            prefixed
          Left _ →
            nodeId
      _ →
        nodeId
      where
      prefixInfo
        ∷ Maybe String
            /\ Boolean
      prefixInfo = String.stripPrefix (Pattern "def-") idString
        /\ usesDefaultPrefix idString

      idString ∷ String
      idString = Codec.encoder BlockId.stringCodec unit nodeId

    withNoConnections
      ∷ AudioNode
      → (AudioNode /\ List AudioNodeId)
    withNoConnections node = node /\ Nil

empty ∷ AudioNodes
empty = AudioNodes $ Graph.fromMap Map.empty

updateAudioNode
  ∷ AudioNodes → AudioNodeId → AudioNode → String \/ AudioNodes
updateAudioNode (AudioNodes graph) nodeId node =
  if nodeId `Map.member` nodes then fromGraph $ Graph.fromMap $
    Map.insertWith
      (\(_ /\ connectionEnds) _ → node /\ connectionEnds)
      nodeId
      (node /\ Nil)
      nodes
  else Left $ "no such node: "
    <> Codec.encoder BlockId.stringCodec unit nodeId
  where
  nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  nodes = Graph.toMap graph

fromGraph ∷ Graph AudioNodeId AudioNode → String \/ AudioNodes
fromGraph graph =
  if allConnectionsAreValid then Right $ AudioNodes normalizedGraph
  else Left "invalid connections"
  where
  normalizedGraph ∷ Graph AudioNodeId AudioNode
  normalizedGraph = Graph.fromMap normalizedNodes

  allConnectionsAreValid ∷ Boolean
  allConnectionsAreValid = foldlWithIndex
    areNodeConnectionsValid
    true
    normalizedNodes

  areNodeConnectionsValid
    ∷ AudioNodeId → Boolean → AudioNode /\ List AudioNodeId → Boolean
  areNodeConnectionsValid nodeId acc (_ /\ connectionEnds) =
    acc && isNotConnectedWithItself && isConnectedWithExistingNodes
    where
    isNotConnectedWithItself ∷ Boolean
    isNotConnectedWithItself = all (not <<< eq nodeId) connectionEnds

    isConnectedWithExistingNodes ∷ Boolean
    isConnectedWithExistingNodes = all
      ( \connectionEnd → connectionEnd == AudioNodeId.output ||
          Map.member connectionEnd nodes
      )
      connectionEnds

  nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  nodes = Graph.toMap graph

  normalizedNodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  normalizedNodes = normalizeNode <$> nodes

  normalizeNode
    ∷ AudioNode /\ List AudioNodeId
    → AudioNode /\ List AudioNodeId
  normalizeNode entry@(audioNode /\ connectionEnds) = case audioNode of
    AudioNode.Oscillator { wave }
      | connectionEnds == Nil && wave == Wave.Sine →
          audioNode /\ Cons AudioNodeId.output Nil
    _ →
      entry

toGraph ∷ AudioNodes → (Graph AudioNodeId AudioNode)
toGraph (AudioNodes graph) = graph

connections ∷ AudioNodes → List (Edge AudioNodeId)
connections (AudioNodes graph) = Graph.edges graph

nodesById ∷ AudioNodes → Map AudioNodeId AudioNode
nodesById (AudioNodes graph) = Tuple.fst <$> Graph.toMap graph

stringCodec ∷ Codec AudioNodes String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder AudioNodes String
stringDecoder = do
  P.ParseState input _ _ ← P.getParserT
  case decodeString input of
    Left err →
      P.fail err
    Right audioNodes →
      pure audioNodes

stringEncoder ∷ Encoder AudioNodes String Unit
stringEncoder _ = encodeString

decodeString
  ∷ String
  → String \/ AudioNodes
decodeString input = do
  state ← foldl parseFold (Right initialDecodeState) nonEmptyLines
  let
    nodeEntries ∷ Array (AudioNodeId /\ AudioNode)
    nodeEntries = Map.toUnfoldable state.nodes

    graphEntries ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
    graphEntries = Map.fromFoldable
      (toGraphEntry state.connections <$> nodeEntries)
  fromGraph $ Graph.fromMap graphEntries
  where
  nonEmptyLines ∷ Array String
  nonEmptyLines = Array.filter (not <<< String.null)
    (String.split (Pattern "\n") input)

  parseFold
    ∷ String \/ DecodeState
    → String
    → String \/ DecodeState
  parseFold acc line = acc >>= flip parseLine line

encodeString
  ∷ AudioNodes
  → String
encodeString (AudioNodes graph) = String.joinWith "\n"
  (nodeLines <> connectionLines)
  where
  nodeLines ∷ Array String
  nodeLines = renderNodeLine <$> sortedEntries

  renderNodeLine
    ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
    → String
  renderNodeLine (nodeId /\ (node /\ _)) =
    renderId nodeId <> " " <> renderNodeConf node

  renderNodeConf ∷ AudioNode → String
  renderNodeConf node =
    replaceSeqPrefix "gseq" "gsec"
      $ replaceSeqPrefix "fseq" "fsec"
      $ Codec.encoder AudioNode.stringCodec unit node

  connectionLines ∷ Array String
  connectionLines = Array.sort renderedConnections

  renderedConnections ∷ Array String
  renderedConnections =
    sortedEntries >>= \(nodeId /\ (_ /\ ends)) →
      renderConnection nodeId <$> Array.fromFoldable ends

  renderConnection ∷ AudioNodeId → AudioNodeId → String
  renderConnection from to =
    renderId from <> "->" <> renderId to

  sortedEntries
    ∷ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  sortedEntries = Array.sortWith (renderId <<< Tuple.fst)
    (Map.toUnfoldable $ Graph.toMap graph)

type DecodeState =
  { connections ∷ Map AudioNodeId (Array AudioNodeId)
  , nodes ∷ Map AudioNodeId AudioNode
  }

initialDecodeState ∷ DecodeState
initialDecodeState =
  { connections: Map.empty
  , nodes: Map.empty
  }

parseLine
  ∷ DecodeState
  → String
  → String \/ DecodeState
parseLine state line = case String.trim line of
  trimmed | String.null trimmed →
    Right state
  trimmed | String.contains (Pattern "->") trimmed →
    parseConnectionLine state trimmed
  trimmed →
    parseNodeLine state trimmed

parseConnectionLine
  ∷ DecodeState
  → String
  → String \/ DecodeState
parseConnectionLine state line =
  case String.split (Pattern "->") line of
    [ fromPart, toPart ] → do
      fromId ← decodeNodeId (String.trim fromPart)
      toId ← decodeNodeId (String.trim toPart)
      if Map.member fromId state.nodes then
        Right state
          { connections = Map.alter (appendConnection toId) fromId
              state.connections
          }
      else
        Left $ "unknown node: "
          <> Codec.encoder BlockId.stringCodec unit fromId
    _ →
      Left "invalid connection definition"
  where
  appendConnection
    ∷ AudioNodeId
    → Maybe (Array AudioNodeId)
    → Maybe (Array AudioNodeId)
  appendConnection connectionEnd maybeExisting = Just
    case maybeExisting of
      Nothing →
        [ connectionEnd ]
      Just existing →
        Array.snoc existing connectionEnd

parseNodeLine
  ∷ DecodeState
  → String
  → String \/ DecodeState
parseNodeLine state line = case String.indexOf (Pattern " ") line of
  Nothing →
    Left "expected audio node definition"
  Just spaceIndex →
    let
      nodeIdPart ∷ String
      nodeIdPart = String.take spaceIndex line

      nodeConf ∷ String
      nodeConf = String.drop (spaceIndex + 1) line
    in
      do
        nodeId ← decodeNodeId nodeIdPart
        if Map.member nodeId state.nodes then
          Left $
            "duplicate audio node id: "
              <> Codec.encoder BlockId.stringCodec unit nodeId
        else do
          node ← decodeAudioNode nodeConf
          Right $ state
            { nodes = Map.insert nodeId node state.nodes
            }

decodeNodeId
  ∷ String
  → String \/ AudioNodeId
decodeNodeId raw = case runParser normalized decoder of
  Left parseError →
    Left $ parseErrorMessage parseError
  Right nodeId →
    Right nodeId
  where
  normalized ∷ String
  normalized = normalizeIdString (String.trim raw)

  decoder ∷ Decoder AudioNodeId String
  decoder = Codec.decoder BlockId.stringCodec

decodeAudioNode
  ∷ String
  → String \/ AudioNode
decodeAudioNode raw = case runParser normalized decoder of
  Left parseError →
    Left $ parseErrorMessage parseError
  Right node →
    Right node
  where
  normalized ∷ String
  normalized = normalizeNodeCode (String.trim raw)

  decoder ∷ Decoder AudioNode String
  decoder = Codec.decoder AudioNode.stringCodec

normalizeIdString ∷ String → String
normalizeIdString raw = case String.stripPrefix (Pattern "def-") raw of
  Just _ →
    raw
  Nothing | usesDefaultPrefix raw →
    "def-" <> raw
  _ →
    raw

normalizeNodeCode ∷ String → String
normalizeNodeCode raw = case String.stripPrefix (Pattern "fsec") raw of
  Just rest →
    "fseq" <> rest
  Nothing → case String.stripPrefix (Pattern "gsec") raw of
    Just rest →
      "gseq" <> rest
    Nothing →
      raw

renderId ∷ AudioNodeId → String
renderId nodeId = case stripped of
  Just rest | usesDefaultPrefix rest →
    rest
  _ →
    raw
  where
  raw ∷ String
  raw = Codec.encoder BlockId.stringCodec unit nodeId

  stripped ∷ Maybe String
  stripped = String.stripPrefix (Pattern "def-") raw

replaceSeqPrefix
  ∷ String
  → String
  → String
  → String
replaceSeqPrefix oldPrefix newPrefix value =
  case String.stripPrefix (Pattern oldPrefix) value of
    Just rest →
      newPrefix <> rest
    Nothing →
      value

toGraphEntry
  ∷ Map AudioNodeId (Array AudioNodeId)
  → (AudioNodeId /\ AudioNode)
  → AudioNodeId /\ (AudioNode /\ List AudioNodeId)
toGraphEntry connectionsMap (nodeId /\ node) =
  nodeId /\ (node /\ List.fromFoldable connectionsForNode)
  where
  connectionsForNode ∷ Array AudioNodeId
  connectionsForNode =
    fromMaybe [] (Map.lookup nodeId connectionsMap)

usesDefaultPrefix ∷ String → Boolean
usesDefaultPrefix s =
  isJust (String.stripPrefix (Pattern "osc-") s)
    || isJust (String.stripPrefix (Pattern "seq-") s)

groupBlockCodec ∷ Codec AudioNodes GroupBlock Unit
groupBlockCodec = Codec.codec groupBlockDecoder groupBlockEncoder

groupBlockDecoder ∷ Decoder AudioNodes GroupBlock
groupBlockDecoder = do
  P.ParseState input _ _ ← P.getParserT
  case decodeGroupBlock input of
    Left err →
      P.fail err
    Right audioNodes →
      pure audioNodes
  where
  decodeGroupBlock ∷ GroupBlock → String \/ AudioNodes
  decodeGroupBlock groupBlock = do
    oscillatorNodes ← decodeChildGroup AudioNodeId.oscillators
      groupBlock
    sequencerNodes ← decodeChildGroup AudioNodeId.sequencers groupBlock
    let
      allNodes ∷ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
      allNodes = oscillatorNodes <> sequencerNodes
    fromGraph $ Graph.fromMap $ Map.fromFoldable allNodes

  decodeChildGroup
    ∷ AudioNodeId
    → GroupBlock
    → String \/ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  decodeChildGroup groupId groupBlock =
    case Map.lookup groupId childrenMap of
      Nothing →
        Right []
      Just (Group nested /\ _) →
        traverse decodeNode
          (Map.toUnfoldable $ Graph.toMap nested.children)
      Just _ →
        Left "expected group definition"
    where
    childrenMap ∷ Map AudioNodeId (BlockDef /\ List AudioNodeId)
    childrenMap = Graph.toMap groupBlock.children

    decodeNode
      ∷ AudioNodeId
          /\ (BlockDef /\ List AudioNodeId)
      → String \/ (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
    decodeNode (nodeId /\ (Group nested /\ connectionEnds)) =
      case runParser nested (Codec.decoder AudioNode.groupBlockCodec) of
        Left parseError →
          Left $ parseErrorMessage parseError
        Right node →
          Right $ nodeId /\ (node /\ connectionEnds)
    decodeNode (nodeId /\ _) =
      Left $ "expected group node for "
        <> Codec.encoder BlockId.stringCodec unit nodeId

groupBlockEncoder ∷ Encoder AudioNodes GroupBlock Unit
groupBlockEncoder _ (AudioNodes graph) =
  { children: Graph.fromMap $ Map.fromFoldable topChildren
  , properties: { columns: Just C1 }
  , spacedOut: Array.length topChildren > 1
  }
  where
  nodes
    ∷ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  nodes = Map.toUnfoldable (Graph.toMap graph)

  oscillatorChildren
    ∷ Array (AudioNodeId /\ (BlockDef /\ List AudioNodeId))
  oscillatorChildren = map mkGroupEntry $
    Array.filter (\(_ /\ (node /\ _)) → isOscillator node) nodes

  sequencerChildren
    ∷ Array (AudioNodeId /\ (BlockDef /\ List AudioNodeId))
  sequencerChildren = map mkGroupEntry $
    Array.filter (\(_ /\ (node /\ _)) → isSequencer node) nodes

  mkGroupEntry
    ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
    → AudioNodeId /\ (BlockDef /\ List AudioNodeId)
  mkGroupEntry (nodeId /\ (node /\ ends)) =
    nodeId /\
      ( Group (Codec.encoder AudioNode.groupBlockCodec nodeId node)
          /\ ends
      )

  isOscillator ∷ AudioNode → Boolean
  isOscillator = case _ of
    AudioNode.Oscillator _ →
      true
    _ →
      false

  isSequencer ∷ AudioNode → Boolean
  isSequencer = case _ of
    AudioNode.FrequencySequencer _ →
      true
    AudioNode.GainSequencer _ →
      true
    _ →
      false

  topChildren
    ∷ Array (AudioNodeId /\ (BlockDef /\ List AudioNodeId))
  topChildren =
    oscillatorGroupEntry <> sequencerGroupEntry <> outputEntry

  oscillatorGroupEntry
    ∷ Array (AudioNodeId /\ (BlockDef /\ List AudioNodeId))
  oscillatorGroupEntry = case Array.null oscillatorChildren of
    true →
      []
    false →
      [ AudioNodeId.oscillators /\ (oscillatorGroup /\ Nil) ]
    where
    oscillatorGroup ∷ BlockDef
    oscillatorGroup = Group
      { children: Graph.fromMap $ Map.fromFoldable oscillatorChildren
      , properties: { columns: Nothing }
      , spacedOut: false
      }

  sequencerGroupEntry
    ∷ Array (AudioNodeId /\ (BlockDef /\ List AudioNodeId))
  sequencerGroupEntry = case Array.null sequencerChildren of
    true →
      []
    false →
      [ AudioNodeId.sequencers /\ (sequencerGroup /\ Nil) ]
    where
    sequencerGroup ∷ BlockDef
    sequencerGroup = Group
      { children: Graph.fromMap $ Map.fromFoldable sequencerChildren
      , properties: { columns: Nothing }
      , spacedOut: false
      }

  outputEntry
    ∷ Array (AudioNodeId /\ (BlockDef /\ List AudioNodeId))
  outputEntry =
    [ AudioNodeId.output /\ (Node "output" /\ Nil) ]
