module Music.Model.AudioNodes.AudioNodeId
  ( AudioNodeId
  , fromBlockId
  , stringCodec
  , toBlockId
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..))
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype AudioNodeId = AudioNodeId BlockId

derive newtype instance Arbitrary AudioNodeId
derive newtype instance Eq AudioNodeId
derive newtype instance Ord AudioNodeId
derive newtype instance Show AudioNodeId

toBlockId ∷ AudioNodeId → BlockId
toBlockId (AudioNodeId blockId) = prefix <> blockId
  where
  prefix ∷ BlockId
  prefix = BlockId.make D [ E, F ] []

fromBlockId ∷ BlockId → AudioNodeId
fromBlockId = AudioNodeId

stringCodec ∷ Codec AudioNodeId String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder AudioNodeId String
stringDecoder = AudioNodeId <$> Codec.decoder BlockId.stringCodec

stringEncoder ∷ Encoder AudioNodeId String Unit
stringEncoder _ (AudioNodeId blockId) = Codec.encoder
  BlockId.stringCodec
  unit
  blockId
