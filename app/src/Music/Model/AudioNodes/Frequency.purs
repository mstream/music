module Music.Model.AudioNodes.Frequency
  ( Frequency
  , stringCodec
  , toNumber
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Number as Number
import Parsing (fail) as P
import Parsing.String.Basic (number) as P
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen

newtype Frequency = Frequency Number

derive instance Eq Frequency
derive instance Ord Frequency
derive newtype instance Show Frequency

instance Arbitrary Frequency where
  arbitrary = Frequency <$> Gen.choose minValue maxValue

instance Bounded Frequency where
  bottom ∷ Frequency
  bottom = Frequency 10.0
  top ∷ Frequency
  top = Frequency 10000.0

stringCodec ∷ Codec Frequency String Boolean
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder Frequency String
stringDecoder = Frequency <$> do
  x ← P.number
  if x >= minValue && x <= maxValue then pure x
  else P.fail $ show x
    <> " is out of bound: "
    <> show minValue
    <> " - "
    <> show maxValue

stringEncoder ∷ Encoder Frequency String Boolean
stringEncoder round (Frequency x) = show
  if round then Number.round x else x

maxValue ∷ Number
maxValue = toNumber top

minValue ∷ Number
minValue = toNumber bottom

toNumber ∷ Frequency → Number
toNumber (Frequency x) = x
