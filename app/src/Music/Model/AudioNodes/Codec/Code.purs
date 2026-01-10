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
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Parsing (Parser, fail) as P
import Parsing.Combinators (many, optional, try) as P
import Parsing.String (char, string) as P

data Entry
  = NodeEntry (AudioNodeId /\ AudioNode)
  | ConnectionEntry (Edge AudioNodeId)

codec ∷ AudioNodesCodec String Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder String
decoder = do
  entries ← P.many entryLine
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
  entry ← P.try nodeLine <|> connectionLine
  _ ← P.optional (P.char '\n')
  pure entry

nodeLine ∷ P.Parser String Entry
nodeLine = do
  nodeId ← audioNodeId
  _ ← P.char ' '
  _ ← P.string "osc{"
  frequency ← Codec.decoder Frequency.stringCodec
  _ ← P.char ','
  gain ← Codec.decoder Gain.stringCodec
  _ ← P.char ','
  wave ← Codec.decoder Wave.stringCodec
  _ ← P.char '}'
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
  _ ← P.string "->"
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
    <$> Array.sortWith edgeKey connections

  connections ∷ Array (Edge AudioNodeId)
  connections = Array.concatMap renderEdges nodesInOrder
    where
    renderEdges
      ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
      → Array (Edge AudioNodeId)
    renderEdges (nodeId /\ (_ /\ ends)) =
      (\end → { start: nodeId, end }) <$> Array.fromFoldable ends

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
      <> "{"
      <> Codec.encoder Frequency.stringCodec unit frequency
      <> ","
      <> Codec.encoder Gain.stringCodec unit gain
      <> ","
      <> Codec.encoder Wave.stringCodec unit wave
      <> "}"

audioNodeId ∷ P.Parser String AudioNodeId
audioNodeId =
  (P.string "output" $> AudioNodeId.output)
    <|> Codec.decoder AudioNodeId.codec

renderAudioNodeId ∷ AudioNodeId → String
renderAudioNodeId nodeId =
  if nodeId == AudioNodeId.output then "output"
  else Codec.encoder AudioNodeId.codec unit nodeId
