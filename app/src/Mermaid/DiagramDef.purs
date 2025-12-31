module Mermaid.DiagramDef (Blocks, DiagramDef(..), codec) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Gen (arbitraryMap) as Gen
import Mermaid.DiagramDef.BlockDiagram.BlockId (BlockId)
import Mermaid.DiagramDef.BlockDiagram.BlockId as BlockId
import Parsing (Parser)
import Parsing.String (anyTill, char, string) as P
import Parsing.String.Basic (skipSpaces) as P
import Test.QuickCheck.Arbitrary (class Arbitrary)

data DiagramDef = BlockDiagram Blocks

derive instance Eq DiagramDef
derive instance Generic DiagramDef _
derive instance Ord DiagramDef

instance Arbitrary DiagramDef where
  arbitrary = BlockDiagram <$> Gen.arbitraryMap

instance Show DiagramDef where
  show = genericShow

codec ∷ Codec DiagramDef String Unit
codec = Codec.codec decoder encoder

decoder ∷ Decoder DiagramDef String
decoder = BlockDiagram <$> do
  _ ← P.string "block"
  blocks ← manyBlocks
  pure $ Map.fromFoldable blocks
  where
  manyBlocks =
    ( do
        block ← blockLine
        more ← manyBlocks
        pure $ Array.cons block more
    ) <|> pure []
  blockLine = P.char '\n' *> P.skipSpaces *> blockEntry

blockEntry ∷ Parser String (BlockId /\ String)
blockEntry = do
  id ← Codec.decoder BlockId.codec
  P.skipSpaces
  _ ← P.char '['
  _ ← P.char '\"'
  contents /\ _ ← P.anyTill $ P.char '\"'
  _ ← P.char ']'
  pure $ id /\ contents

encoder ∷ Encoder DiagramDef String Unit
encoder _ = case _ of
  BlockDiagram blocks →
    encodeBlockDiagram blocks

encodeBlockDiagram ∷ Map BlockId String → String
encodeBlockDiagram = foldlWithIndex
  (\id acc block → acc <> "\n  " <> renderBlock id block)
  "block"

renderBlock ∷ BlockId → String → String
renderBlock id contents =
  Codec.encoder BlockId.codec unit id
    <> "[\""
    <> contents
    <> "\"]"

type Blocks = Map BlockId String
