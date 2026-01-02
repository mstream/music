module Music.Model.AudioNodes.Codec.Diagram (codec) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.String.Common (joinWith)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef (DiagramDef(..))
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
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
      blocks ∷ Map BlockId.BlockId String
      blocks = map fst (Graph.toMap graph)

encoder ∷ AudioNodesEncoder DiagramDef Unit
encoder _ audioNodes = Blocks $
  Blocks.Def $ Graph.fromMap renderedNodes
  where
  renderedNodes ∷ Map BlockId.BlockId (String /\ List BlockId.BlockId)
  renderedNodes = map renderNode audioNodes

  renderNode ∷ AudioNode → String /\ List BlockId.BlockId
  renderNode node = renderContent node /\ Nil

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
  case runParser codeText (Codec.decoder Code.codec) of
    Right audioNodes →
      pure audioNodes
    Left err →
      fail (parseErrorMessage err)
  where
  codeText =
    joinWith "\n"
      (map renderBlock (Map.toUnfoldable blocks))

renderBlock ∷ BlockId.BlockId /\ String → String
renderBlock (blockId /\ contents) =
  Codec.encoder BlockId.codec unit blockId
    <> " "
    <> contents
