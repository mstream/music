module Mermaid.DiagramDef (DiagramDef(..), codec) where

import Prelude

import Control.Alt ((<|>))
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Generic.Rep (class Generic)
import Data.Array (cons, fromFoldable)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Parsing (Parser)
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

blockDecoder ∷ Decoder DiagramDef String
blockDecoder = do
  _ ← PS.string "block"
  _ ← blockLineEnd
  blocks ← PC.many blockLine
  pure $ Blocks $ Blocks.Def $ Graph.fromMap
    (Map.fromFoldable blocks)
  where
  blockLine ∷ Parser String (BlockId /\ (String /\ List BlockId))
  blockLine = do
    _ ← PC.many (PS.char ' ')
    blockId ← Codec.decoder BlockId.codec
    _ ← PS.char '['
    _ ← PS.char '"'
    contents ← fromCharArray <<< fromFoldable
      <$> PC.manyTill PS.anyChar (PS.string "\"]")
    _ ← PC.many (PS.char ' ')
    _ ← blockLineEnd
    pure $ blockId /\ (contents /\ Nil)

  blockLineEnd ∷ Parser String Unit
  blockLineEnd = (PS.char '\n' $> unit) <|> pure unit

renderBlockDiagram ∷ Blocks.Def → String
renderBlockDiagram (Blocks.Def graph) = joinWith "\n" lines
  where
  lines ∷ Array String
  lines =
    cons "block"
      (map renderLine (Map.toUnfoldable (Graph.toMap graph)))

  renderLine ∷ BlockId /\ (String /\ List BlockId) → String
  renderLine (blockId /\ (contents /\ _)) =
    "  "
      <> Codec.encoder BlockId.codec unit blockId
      <> "[\""
      <> contents
      <> "\"]"
