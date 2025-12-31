module Mermaid.DiagramDef.BlockDiagram.BlockId (BlockId, codec) where

import Prelude

import Data.CodePoint.Unicode (isAlphaNum)
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.String.Gen (genAlphaString) as Gen
import Parsing.String.Basic (takeWhile1) as P
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype BlockId = BlockId String

derive newtype instance Eq BlockId
derive newtype instance Ord BlockId

instance Arbitrary BlockId where
  arbitrary = BlockId <$> Gen.genAlphaString

instance Show BlockId where
  show (BlockId s) = s

codec ∷ Codec BlockId String Unit
codec = Codec.codec decoder encoder

decoder ∷ Decoder BlockId String
decoder = BlockId <$> P.takeWhile1 isAlphaNum

encoder ∷ Encoder BlockId String Unit
encoder _ (BlockId s) = s
