module Music.Model.AudioNodes.Codec.Code (codec) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Graph (Edge)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Music.Model.AudioNodes (AudioNode(..), AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.AudioNodes.Wave as Wave
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS

data Entry
  = NodeEntry (AudioNodeId /\ AudioNode)
  | ConnectionEntry (Edge AudioNodeId)

codec ∷ AudioNodesCodec String Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder String
decoder = do
  entries ← PC.many entryLine
  case entries of
    Nil →
      pure AudioNodes.empty
    _ → case buildAudioNodes (Array.fromFoldable entries) of
      Left err →
        P.fail err
      Right audioNodes →
        pure audioNodes

entryLine ∷ P.Parser String Entry
entryLine = do
  entry ← PC.try nodeLine <|> connectionLine
  _ ← PC.optional (PS.char '\n')
  pure entry

nodeLine ∷ P.Parser String Entry
nodeLine = do
  nodeId ← audioNodeId
  _ ← PS.char ' '
  _ ← PS.string "osc{f="
  frequency ← Codec.decoder Frequency.codec
  _ ← PS.string ",g="
  gain ← Codec.decoder Gain.codec
  _ ← PS.string ",w="
  wave ← Codec.decoder Wave.codec
  _ ← PS.char '}'
  pure $ NodeEntry
    ( nodeId
        /\ Oscillator
          { frequency
          , gain
          , wave
          }
    )

connectionLine ∷ P.Parser String Entry
connectionLine = do
  start ← audioNodeId
  _ ← PS.string "->"
  end ← audioNodeId
  pure $ ConnectionEntry { start, end }

buildAudioNodes ∷ Array Entry → Either String AudioNodes
buildAudioNodes entries = do
  collected ← foldEither collect initState entries
  graphMap ← foldEither addConnection
    (map addEmptyConnections collected.nodes)
    collected.connections
  case
    AudioNodes.fromGraph
      (Graph.fromMap graphMap)
    of
    Left err →
      Left err
    Right audioNodes →
      Right audioNodes
  where
  initState
    ∷ { nodes ∷ Map AudioNodeId AudioNode
      , connections ∷ List (Edge AudioNodeId)
      }
  initState = { nodes: Map.empty, connections: Nil }

  collect
    ∷ { nodes ∷ Map AudioNodeId AudioNode
      , connections ∷ List (Edge AudioNodeId)
      }
    → Entry
    → Either String
        { nodes ∷ Map AudioNodeId AudioNode
        , connections ∷ List (Edge AudioNodeId)
        }
  collect acc = case _ of
    NodeEntry (nodeId /\ node) →
      if Map.member nodeId acc.nodes then
        Left $ "Duplicate node id: " <> show nodeId
      else
        Right $ acc
          { nodes = Map.insert nodeId node acc.nodes }
    ConnectionEntry edge →
      Right $ acc
        { connections = Cons edge acc.connections }

  addEmptyConnections
    ∷ AudioNode → (AudioNode /\ List AudioNodeId)
  addEmptyConnections node = node /\ Nil

  addConnection
    ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
    → Edge AudioNodeId
    → Either String (Map AudioNodeId (AudioNode /\ List AudioNodeId))
  addConnection acc { start, end } = case Map.lookup start acc of
    Nothing →
      Left $ "Connection start not defined: " <> show start
    Just (node /\ ends) →
      Right
        $ Map.insert start (node /\ Cons end ends) acc

  foldEither
    ∷ ∀ f a b
    . Foldable f
    ⇒ (b → a → Either String b)
    → b
    → f a
    → Either String b
  foldEither step initVal =
    foldl step' (Right initVal)
    where
    step' ∷ Either String b → a → Either String b
    step' acc item = acc >>= \state → step state item

encoder ∷ AudioNodesEncoder String Unit
encoder _ audioNodes = joinWith "\n" (nodeLines <> connectionLines)
  where
  graphMap ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  graphMap = Graph.toMap (AudioNodes.toGraph audioNodes)

  nodesInOrder
    ∷ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  nodesInOrder = Map.toUnfoldable graphMap

  nodeLines ∷ Array String
  nodeLines =
    renderEntry <$> Map.toUnfoldable (AudioNodes.nodesById audioNodes)

  connectionLines ∷ Array String
  connectionLines = renderConnection
    <$> Array.sortWith edgeKey filteredConnections

  filteredConnections ∷ Array (Edge AudioNodeId)
  filteredConnections = Array.concatMap renderEdges nodesInOrder
    where
    renderEdges
      ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
      → Array (Edge AudioNodeId)
    renderEdges (nodeId /\ (_ /\ ends)) =
      let
        endsArray ∷ Array AudioNodeId
        endsArray = Array.fromFoldable ends

        keepDefault ∷ Boolean
        keepDefault = eqDefault endsArray &&
          Maybe.fromMaybe false (eq nodeId <$> firstDefaultOutput)

        filteredEnds ∷ Array AudioNodeId
        filteredEnds =
          if eqDefault endsArray && not keepDefault then []
          else endsArray
      in
        (\end → { start: nodeId, end }) <$> filteredEnds

    eqDefault ∷ Array AudioNodeId → Boolean
    eqDefault ends =
      ends == [ AudioNodeId.output ]

    firstDefaultOutput ∷ Maybe AudioNodeId
    firstDefaultOutput = Array.head
      (Array.mapMaybe defaultOnly nodesInOrder)

    defaultOnly
      ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
      → Maybe AudioNodeId
    defaultOnly (nodeId /\ (_ /\ ends)) =
      let
        endsArray ∷ Array AudioNodeId
        endsArray = Array.fromFoldable ends
      in
        if endsArray == [ AudioNodeId.output ] then Just nodeId
        else Nothing

  renderConnection ∷ Edge AudioNodeId → String
  renderConnection { end, start } =
    renderAudioNodeId start <> "->" <> renderAudioNodeId end

  edgeKey ∷ Edge AudioNodeId → String
  edgeKey { end, start } =
    renderAudioNodeId start <> "->" <> renderAudioNodeId end

renderEntry ∷ AudioNodeId /\ AudioNode → String
renderEntry (nodeId /\ node) = case node of
  Oscillator { frequency, gain, wave } →
    Codec.encoder AudioNodeId.codec unit nodeId <> " osc"
      <> "{f="
      <> Codec.encoder Frequency.codec unit frequency
      <> ",g="
      <> Codec.encoder Gain.codec unit gain
      <> ",w="
      <> Codec.encoder Wave.codec unit wave
      <> "}"

audioNodeId ∷ P.Parser String AudioNodeId
audioNodeId =
  (PS.string "output" $> AudioNodeId.output)
    <|> Codec.decoder AudioNodeId.codec

renderAudioNodeId ∷ AudioNodeId → String
renderAudioNodeId nodeId =
  if nodeId == AudioNodeId.output then "output"
  else Codec.encoder AudioNodeId.codec unit nodeId
