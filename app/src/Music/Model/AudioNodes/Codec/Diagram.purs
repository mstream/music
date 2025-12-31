module Music.Model.AudioNodes.Codec.Diagram (codec) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.BlockDiagram.BlockId (BlockId)
import Mermaid.DiagramDef.BlockDiagram.BlockId as BlockId
import Music.Model.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes (AudioNode(..))
import Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Music.Model.AudioNodes.Codec.Code as Code
import Music.Model.AudioNodes.Frequency as Frequency
import Music.Model.AudioNodes.Gain as Gain
import Music.Model.AudioNodes.Wave as Wave
import Parsing as P

codec ∷ AudioNodesCodec DiagramDef Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder DiagramDef
decoder = do
  P.ParseState diagram _ _ ← P.getParserT
  case diagram of
    DiagramDef.BlockDiagram blocks →
      decodeBlocks blocks

encoder ∷ AudioNodesEncoder DiagramDef Unit
encoder _ audioNodes =
  DiagramDef.BlockDiagram (Map.fromFoldable (toBlock <$> nodesArray))
  where
  nodesArray =
    Map.toUnfoldable audioNodes
      ∷ Array (AudioNodeId.AudioNodeId /\ AudioNode)

  toBlock ∷ AudioNodeId /\ AudioNode → BlockId /\ String
  toBlock (audioId /\ node) =
    audioId /\ renderContent node

renderContent ∷ AudioNode → String
renderContent (Oscillator { frequency, gain, wave }) =
  "osc{f="
    <> Codec.encoder Frequency.codec unit frequency
    <> ",g="
    <> Codec.encoder Gain.codec unit gain
    <> ",w="
    <> Codec.encoder Wave.codec unit wave
    <> "}"

decodeBlocks ∷ Map BlockId.BlockId String → AudioNodesDecoder DiagramDef
decodeBlocks blocks =
  case P.runParser codeText (Codec.decoder Code.codec) of
    Right audioNodes →
      pure audioNodes
    Left err →
      P.fail (P.parseErrorMessage err)
  where
  codeText =
    joinWith "\n"
      (map renderBlock (Map.toUnfoldable blocks))

renderBlock ∷ BlockId.BlockId /\ String → String
renderBlock (blockId /\ contents) =
  Codec.encoder BlockId.codec unit blockId
    <> " "
    <> contents
