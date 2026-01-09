module Music.Model.AudioNodes.Gain
  ( Gain
  , stringCodec
  , toNumber
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Parsing (fail) as P
import Parsing.String.Basic (number) as P
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen

newtype Gain = Gain Number

derive instance Eq Gain
derive instance Ord Gain
derive newtype instance Show Gain

instance Arbitrary Gain where
  arbitrary = Gain <$> Gen.choose minValue maxValue

instance Bounded Gain where
  bottom ∷ Gain
  bottom = Gain zero
  top ∷ Gain
  top = Gain one

stringCodec ∷ Codec Gain String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder Gain String
stringDecoder = Gain <$> do
  x ← P.number
  if x >= minValue && x <= maxValue then pure x
  else P.fail $ show x
    <> " is out of bound: "
    <> show minValue
    <> " - "
    <> show maxValue

stringEncoder ∷ Encoder Gain String Unit
stringEncoder _ (Gain x) = show x

maxValue ∷ Number
maxValue = toNumber top

minValue ∷ Number
minValue = toNumber bottom

toNumber ∷ Gain → Number
toNumber (Gain x) = x

