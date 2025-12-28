module Music.Model.AudioNodeId
  ( AudioNodeId
  , codec
  , fromString
  ) where

import Prelude

import Data.CodePoint.Unicode (isAlphaNum)
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.String.Gen (genAlphaString) as Gen
import Parsing (runParser)
import Parsing.String.Basic (takeWhile1)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype AudioNodeId = AudioNodeId String

derive instance Eq AudioNodeId
derive instance Ord AudioNodeId

instance Arbitrary AudioNodeId where
  arbitrary = AudioNodeId <$> Gen.genAlphaString

instance Show AudioNodeId where
  show (AudioNodeId s) = s

fromString ∷ Partial ⇒ String → AudioNodeId
fromString s = case runParser s decoder of
  Right nodeId →
    nodeId

codec ∷ Codec AudioNodeId String Unit
codec = Codec.codec decoder encoder

decoder ∷ Decoder AudioNodeId String
decoder = AudioNodeId <$> takeWhile1 isAlphaNum

encoder ∷ Encoder AudioNodeId String Unit
encoder _ (AudioNodeId s) = s

