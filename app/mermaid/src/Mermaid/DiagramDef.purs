module Mermaid.DiagramDef (DiagramDef(..), stringCodec) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Mermaid.DiagramDef.Blocks as Blocks
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

stringCodec ∷ Codec DiagramDef String Boolean
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder DiagramDef String
stringDecoder = do
  _ ← PS.string "block"
  _ ← PC.optional (PS.char '\n')
  Blocks <$> Blocks.defStringDecoder

stringEncoder ∷ Encoder DiagramDef String Boolean
stringEncoder useIndent (Blocks def) =
  "block\n" <> Codec.encoder Blocks.defStringCodec useIndent def
