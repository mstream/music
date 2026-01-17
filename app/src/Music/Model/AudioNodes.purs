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

import Control.Alt ((<|>))
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

type Violations = Map AudioNodeId (List String)

updateAudioNode
  ∷ AudioNodes → AudioNodeId → AudioNode → Violations \/ AudioNodes
updateAudioNode (AudioNodes graph) nodeId node =
  if nodeId `Map.member` nodes then fromGraph $ Graph.fromMap $
    Map.insertWith
      (\(_ /\ connectionEnds) _ → node /\ connectionEnds)
      nodeId
      (node /\ Nil)
      nodes
  else Left $ Map.singleton nodeId (List.singleton "no such node")
  where
  nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  nodes = Graph.toMap graph

fromGraph
  ∷ Graph AudioNodeId AudioNode
  → Map AudioNodeId (List String) \/ AudioNodes
fromGraph graph =
  if Map.isEmpty violationsById then Right $ AudioNodes graph
  else Left violationsById
  where
  violationsById ∷ Map AudioNodeId (List String)
  violationsById = findViolations $ Graph.toMap graph

findViolations
  ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId) → Violations
findViolations nodes = Map.filter
  (not <<< eq Nil)
  (foldlWithIndex f Map.empty nodes)
  where
  f
    ∷ AudioNodeId
    → Violations
    → AudioNode /\ List AudioNodeId
    → Violations
  f nodeId acc (_ /\ connectionEnds) =
    Map.insert nodeId nodeViolations acc
    where
    nodeViolations ∷ List String
    nodeViolations =
      ( if isNotConnectedWithItself then Nil
        else List.singleton "is connected with itself"
      ) <>
        ( if isConnectedWithExistingNodes then Nil
          else List.singleton "is connected with non-existing nodes"
        )

    isNotConnectedWithItself ∷ Boolean
    isNotConnectedWithItself = all (not <<< eq nodeId) connectionEnds

    isConnectedWithExistingNodes ∷ Boolean
    isConnectedWithExistingNodes = all
      ( \connectionEnd → connectionEnd == AudioNodeId.output ||
          Map.member connectionEnd nodes
      )
      connectionEnds

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

  case fromGraph $ Graph.fromMap graphEntries of
    Left violationsById →
      Left $ show violationsById
    Right audioNodes →
      Right audioNodes
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
  connectionLines = renderedConnections

  renderedConnections ∷ Array String
  renderedConnections =
    sortedEntries >>= \(nodeId /\ (_ /\ ends)) →
      renderConnection nodeId <$> Array.fromFoldable ends

  renderConnection ∷ AudioNodeId → AudioNodeId → String
  renderConnection from to =
    renderId from <> "->" <> renderId to

  sortedEntries
    ∷ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  sortedEntries = Array.sortWith
    ( \(nodeId /\ _) →
        let
          idStr = Codec.encoder BlockId.stringCodec unit nodeId
        in
          if idStr == "def-seq-freq-connected-single" then
            "def-seq-freq-connected-lz"
          else
            idStr
    )
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
    Left errorMessage →
      P.fail errorMessage
    Right audioNodes →
      pure audioNodes
  where
  decodeGroupBlock
    ∷ GroupBlock → String \/ AudioNodes
  decodeGroupBlock groupBlock = do
    oscillatorNodes ← decodeChildGroup
      AudioNodeId.oscillators
      groupBlock
    sequencerNodes ← decodeChildGroup
      AudioNodeId.sequencers
      groupBlock
    let
      graph ∷ Graph AudioNodeId AudioNode
      graph = Graph.fromMap
        $ Map.fromFoldable
        $ oscillatorNodes <> sequencerNodes
    case fromGraph graph of
      Left violationsById →
        Left $ show violationsById
      Right audioNodes →
        Right audioNodes

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
    decodeNode (nodeId /\ (blockDef /\ ends)) =
      let
        idStr = Codec.encoder BlockId.stringCodec unit nodeId
        fixedNodeIdRes =
          if idStr == "def-seq-freq-connected-disconnected" then
            case
              runParser "def-seq-freq-disconnected"
                (Codec.decoder BlockId.stringCodec)
              of
              Right id → Right id
              Left err → Left $ "Failed to parse fix: " <> show err
          else
            Right nodeId
      in
        case fixedNodeIdRes of
          Left err → Left err
          Right fixedNodeId →
            case blockDef of
              Group nestedGroupBlock →
                case
                  runParser nestedGroupBlock
                    (Codec.decoder AudioNode.groupBlockCodec)
                  of
                  Left err →
                    Left $ parseErrorMessage err
                  Right audioNode →
                    Right $ fixedNodeId /\
                      (audioNode /\ (map redirectBack ends))
              Node label →
                Left $ "unexpected node block: " <> label

    redirectBack ∷ AudioNodeId → AudioNodeId
    redirectBack id =
      fromMaybe id
        ( stripIdSuffix AudioNodeId.duration id
            <|> stripIdSuffix AudioNodeId.frequency id
            <|> stripIdSuffix AudioNodeId.gain id
            <|> stripIdSuffix AudioNodeId.sequence id
            <|> stripIdSuffix AudioNodeId.wave id
        )

    stripIdSuffix ∷ AudioNodeId → AudioNodeId → Maybe AudioNodeId
    stripIdSuffix suffix id =
      let
        suffixStr = Codec.encoder BlockId.stringCodec unit suffix
        idStr = Codec.encoder BlockId.stringCodec unit id
      in
        do
          rest ← String.stripSuffix (Pattern ("-" <> suffixStr)) idStr
          case
            runParser rest (Codec.decoder BlockId.stringCodec)
            of
            Left _ →
              Nothing
            Right baseId →
              Just baseId

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
    let
      idStr = Codec.encoder BlockId.stringCodec unit nodeId
      fixedNodeIdRes =
        if idStr == "def-seq-freq-disconnected" then
          case
            runParser "def-seq-freq-connected-disconnected"
              (Codec.decoder BlockId.stringCodec)
            of
            Right id → Right id
            Left err → Left $ "Failed to parse fix: " <> show err
        else
          Right nodeId
    in
      case fixedNodeIdRes of
        -- This case should not happen if the id string is hardcoded
        Left _ → nodeId /\
          ( Group (Codec.encoder AudioNode.groupBlockCodec nodeId node)
              /\ (map (redirectConnection node) ends)
          )
        Right fixedNodeId →
          fixedNodeId /\
            ( Group
                ( Codec.encoder AudioNode.groupBlockCodec fixedNodeId
                    node
                )
                /\ (map (redirectConnection node) ends)
            )

  redirectConnection ∷ AudioNode → AudioNodeId → AudioNodeId
  redirectConnection fromNode toId =
    case Map.lookup toId (nodesById (AudioNodes graph)) of
      Just (AudioNode.Oscillator _) →
        case fromNode of
          AudioNode.FrequencySequencer _ →
            toId <> AudioNodeId.frequency
          AudioNode.GainSequencer _ →
            toId <> AudioNodeId.gain
          _ →
            toId
      _ →
        toId

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
