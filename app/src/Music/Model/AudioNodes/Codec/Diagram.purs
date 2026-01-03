module Music.Model.AudioNodes.Codec.Diagram (codec) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef (DiagramDef(..))
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Model.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes (AudioNode(..))
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Music.Model.AudioNodes.Codec.Code as Code
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.AudioNodes.Wave as Wave
import Parsing
  ( ParseState(..)
  , consume
  , fail
  , getParserT
  , parseErrorMessage
  , runParser
  )

codec ∷ AudioNodesCodec DiagramDef Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder DiagramDef
decoder = do
  ParseState diagram _ _ ← getParserT
  consume
  case diagram of
    Blocks (Blocks.Def graph) →
      decodeBlocks blocks
      where
      blocks ∷ Map BlockId.BlockId (String /\ List BlockId)
      blocks = Graph.toMap graph

encoder ∷ AudioNodesEncoder DiagramDef Unit
encoder _ audioNodes = Blocks $ Blocks.Def renderedNodes
  where
  renderedNodes ∷ Graph BlockId String
  renderedNodes = Graph.fromMap (map renderNode filteredGraph)

  filteredGraph ∷ Map BlockId (AudioNode /\ List BlockId)
  filteredGraph = Map.fromFoldable (map keepConnections nodesInOrder)
    where
    keepConnections
      ∷ BlockId /\ (AudioNode /\ List BlockId)
      → BlockId /\ (AudioNode /\ List BlockId)
    keepConnections (nodeId /\ (node /\ ends)) =
      let
        endsArray ∷ Array BlockId
        endsArray = Array.fromFoldable ends

        keepDefault ∷ Boolean
        keepDefault = endsArray == defaultConnection &&
          Maybe.fromMaybe false (eq nodeId <$> firstDefault)

        filteredEnds ∷ List BlockId
        filteredEnds =
          if endsArray == defaultConnection && not keepDefault then Nil
          else List.fromFoldable endsArray
      in
        nodeId /\ (node /\ filteredEnds)

    nodesInOrder
      ∷ Array (BlockId /\ (AudioNode /\ List BlockId))
    nodesInOrder = Map.toUnfoldable graphMap

    graphMap ∷ Map BlockId (AudioNode /\ List BlockId)
    graphMap = Graph.toMap (AudioNodes.toGraph audioNodes)

    firstDefault ∷ Maybe BlockId
    firstDefault = Array.head
      (Array.mapMaybe defaultOnly nodesInOrder)

    defaultOnly
      ∷ BlockId /\ (AudioNode /\ List BlockId)
      → Maybe BlockId
    defaultOnly (nodeId /\ (_ /\ ends)) =
      let
        endsArray ∷ Array BlockId
        endsArray = Array.fromFoldable ends
      in
        if endsArray == defaultConnection then Just nodeId
        else Nothing

    defaultConnection ∷ Array BlockId
    defaultConnection = [ AudioNodeId.output ]

  renderNode
    ∷ AudioNode /\ List BlockId
    → (String /\ List BlockId)
  renderNode (node /\ ends) = renderNodeLabel node /\ ends

  renderNodeLabel ∷ AudioNode → String
  renderNodeLabel (Oscillator { frequency, gain, wave }) =
    "osc{f="
      <> Codec.encoder Frequency.codec unit frequency
      <> ",g="
      <> Codec.encoder Gain.codec unit gain
      <> ",w="
      <> Codec.encoder Wave.codec unit wave
      <> "}"

decodeBlocks
  ∷ Map BlockId.BlockId (String /\ List BlockId)
  → AudioNodesDecoder DiagramDef
decodeBlocks blocks =
  case runParser codeText (Codec.decoder Code.codec) of
    Right audioNodes →
      pure audioNodes
    Left err →
      fail (parseErrorMessage err)
  where
  codeText ∷ String
  codeText = joinWith "\n" (nodeLines <> connectionLines)

  nodeLines ∷ Array String
  nodeLines = map renderBlock (Map.toUnfoldable blocks)

  connectionLines ∷ Array String
  connectionLines = map renderConnection connections

  connections ∷ Array (BlockId /\ BlockId)
  connections = Array.concatMap renderEdges (Map.toUnfoldable blocks)

  renderEdges
    ∷ BlockId /\ (String /\ List BlockId)
    → Array (BlockId /\ BlockId)
  renderEdges (start /\ (_ /\ ends)) =
    (\end → start /\ end) <$> Array.fromFoldable ends

renderBlock
  ∷ BlockId.BlockId /\ (String /\ List BlockId)
  → String
renderBlock (blockId /\ (contents /\ _)) =
  renderAudioNodeId blockId
    <> " "
    <> contents

renderConnection ∷ BlockId /\ BlockId → String
renderConnection (start /\ end) =
  renderAudioNodeId start <> "->" <> renderAudioNodeId end

renderAudioNodeId ∷ BlockId → String
renderAudioNodeId blockId =
  if blockId == BlockId.reserved then "output"
  else Codec.encoder BlockId.codec unit blockId
