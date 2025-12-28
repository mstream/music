module Music.Model.AudioNodes.Gain
  ( Gain
  , codec
  , fromNumber
  , htmlInputCodec
  , htmlInputMaxVal
  , htmlInputMinVal
  , toNumber
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Parsing (fail)
import Parsing.String.Basic (number)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Gain = Gain Number

derive instance Eq Gain
derive instance Ord Gain
derive newtype instance Arbitrary Gain
derive newtype instance Show Gain

fromNumber ∷ Partial ⇒ Number → Gain
fromNumber = Gain

toNumber ∷ Gain → Number
toNumber (Gain x) = x

codec ∷ Codec Gain String Unit
codec = Codec.codec decoder encoder

decoder ∷ Decoder Gain String
decoder = Gain <$> number

encoder ∷ Encoder Gain String Unit
encoder _ (Gain x) = show x

htmlInputMaxVal ∷ Number
htmlInputMaxVal = 0.0

htmlInputMinVal ∷ Number
htmlInputMinVal = 1.0

htmlInputCodec ∷ Codec Gain String Unit
htmlInputCodec = Codec.codec htmlInputDecoder htmlInputEncoder

htmlInputDecoder ∷ Decoder Gain String
htmlInputDecoder = do
  x ← number
  if isWithinRange x then pure $ Gain x
  else fail outOfRangeErrorMessage
  where
  isWithinRange ∷ Number → Boolean
  isWithinRange x = x >= htmlInputMinVal && x <= htmlInputMaxVal

  outOfRangeErrorMessage ∷ String
  outOfRangeErrorMessage =
    "HTML input form of gain should be a number between "
      <> show htmlInputMinVal
      <> " and "
      <> show htmlInputMaxVal

htmlInputEncoder ∷ Encoder Gain String Unit
htmlInputEncoder _ (Gain x) = show x
