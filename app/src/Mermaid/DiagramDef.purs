module Mermaid.DiagramDef (DiagramDef(..), codec) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Graph as Graph
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Parsing (Parser, fail)
import Parsing.Combinators as PC
import Parsing.String as PS
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

data DiagramDef = Blocks Blocks.Def

derive instance Eq DiagramDef
derive instance Generic DiagramDef _
derive instance Ord DiagramDef

instance Arbitrary DiagramDef where
  arbitrary = Blocks <$> arbitrary

instance Show DiagramDef where
  show = genericShow

codec ∷ Codec DiagramDef String Unit
codec = Codec.codec decoder encoder

data DiagramType = Block

decoder ∷ Decoder DiagramDef String
decoder = blockDecoder

encoder ∷ Encoder DiagramDef String Unit
encoder _ = case _ of
  Blocks def → renderBlockDiagram def

data BlockEntry
  = NodeEntry BlockId String
  | ConnectionEntry BlockId BlockId

blockDecoder ∷ Decoder DiagramDef String
blockDecoder = do
  _ ← PS.string "block"
  _ ← blockLineEnd
  entries ← PC.many blockEntry
  let
    parsedEntries ∷ Array BlockEntry
    parsedEntries = Array.fromFoldable entries
  case foldEntries parsedEntries of
    Left err →
      fail err
    Right graphMap →
      pure $ Blocks $ Blocks.Def $ Graph.fromMap graphMap
  where
  blockEntry ∷ Parser String BlockEntry
  blockEntry =
    PC.try nodeEntry <|> connectionEntry

  nodeEntry ∷ Parser String BlockEntry
  nodeEntry = do
    _ ← PC.many (PS.char ' ')
    blockId ← Codec.decoder BlockId.codec
    _ ← PS.char '['
    _ ← PS.char '"'
    contents ← fromCharArray <<< Array.fromFoldable
      <$> PC.manyTill PS.anyChar (PS.string "\"]")
    _ ← PC.many (PS.char ' ')
    _ ← blockLineEnd
    pure $ NodeEntry blockId contents

  connectionEntry ∷ Parser String BlockEntry
  connectionEntry = do
    _ ← PC.many (PS.char ' ')
    fromId ← Codec.decoder BlockId.codec
    _ ← PC.many (PS.char ' ')
    _ ← PS.string "-->"
    _ ← PC.many (PS.char ' ')
    toId ← Codec.decoder BlockId.codec
    _ ← PC.many (PS.char ' ')
    _ ← blockLineEnd
    pure $ ConnectionEntry fromId toId

  blockLineEnd ∷ Parser String Unit
  blockLineEnd = (PS.char '\n' $> unit) <|> pure unit

type ParseState =
  { edges ∷ Array (BlockId /\ BlockId)
  , nodes ∷ Map.Map BlockId String
  , order ∷ Array BlockId
  }

foldEntries
  ∷ Array BlockEntry
  → Either String (Map.Map BlockId (String /\ List BlockId))
foldEntries entries = do
  state ← Array.foldl step (Right initState) entries
  graphEntries ← traverse (toGraphEntry state) state.order
  pure $ Map.fromFoldable graphEntries
  where
  initState ∷ ParseState
  initState =
    { edges: []
    , nodes: Map.empty
    , order: []
    }

  step ∷ Either String ParseState → BlockEntry → Either String ParseState
  step acc entry = acc >>= \state → case entry of
    NodeEntry blockId contents →
      if Map.member blockId state.nodes then
        Left $ "Duplicate block id: " <> renderBlockId blockId
      else
        Right
          $ state
              { nodes = Map.insert blockId contents state.nodes
              , order = Array.snoc state.order blockId
              }
    ConnectionEntry fromId toId →
      Right $ state
        { edges = Array.snoc state.edges (fromId /\ toId) }

  toGraphEntry
    ∷ ParseState
    → BlockId
    → Either String (BlockId /\ (String /\ List BlockId))
  toGraphEntry state blockId =
    case Map.lookup blockId state.nodes of
      Nothing →
        Left $ "Undefined block id: " <> renderBlockId blockId
      Just contents →
        let
          rawTargets ∷ Array BlockId
          rawTargets = map snd
            (Array.filter
              (\(fromId /\ _) → fromId == blockId)
              state.edges)

          connections ∷ List BlockId
          connections = List.fromFoldable
            (map (normalizeTarget blockId state.order) rawTargets)
        in
          Right $ blockId /\ (contents /\ connections)

renderBlockDiagram ∷ Blocks.Def → String
renderBlockDiagram (Blocks.Def graph) = joinWith "\n" diagramLines
  where
  diagramLines ∷ Array String
  diagramLines = Array.cons "block" (nodeLines <> connectionLines)

  nodeLines ∷ Array String
  nodeLines = map renderNode orderedNodes

  connectionLines ∷ Array String
  connectionLines = map renderConnection graphEdges

  orderedNodes ∷ Array (BlockId /\ (String /\ List BlockId))
  orderedNodes = Map.toUnfoldable (Graph.toMap graph)

  graphEdges ∷ Array (BlockId /\ BlockId)
  graphEdges =
    Array.concatMap toEdges orderedNodes
    where
    toEdges
      ∷ BlockId /\ (String /\ List BlockId)
      → Array (BlockId /\ BlockId)
    toEdges (blockId /\ (_ /\ targets)) =
      (\dest → blockId /\ dest) <$> Array.fromFoldable targets

  renderNode ∷ BlockId /\ (String /\ List BlockId) → String
  renderNode (blockId /\ (contents /\ _)) =
    "  "
      <> renderBlockId blockId
      <> "[\""
      <> contents
      <> "\"]"

  renderConnection ∷ BlockId /\ BlockId → String
  renderConnection (fromId /\ dest) =
    "  "
      <> renderBlockId fromId
      <> " --> "
      <> renderDestination fromId dest

  renderDestination ∷ BlockId → BlockId → String
  renderDestination _ dest =
    if dest == BlockId.reserved then "output"
    else renderBlockId dest

normalizeTarget ∷ BlockId → Array BlockId → BlockId → BlockId
normalizeTarget _ _ target =
  if isReserved target then BlockId.reserved
  else target
  where
  isReserved ∷ BlockId → Boolean
  isReserved blockId =
    blockId == BlockId.reserved ||
      renderBlockId blockId == "output"

renderBlockId ∷ BlockId → String
renderBlockId = Codec.encoder BlockId.codec unit
