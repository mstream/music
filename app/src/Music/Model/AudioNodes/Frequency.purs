module Music.Model.AudioNodes.Frequency
  ( Frequency
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
import Data.Number (exp, log)
import Parsing (fail)
import Parsing.String.Basic (number)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype Frequency = Frequency Number

derive instance Eq Frequency
derive instance Ord Frequency
derive newtype instance Arbitrary Frequency
derive newtype instance Show Frequency

fromNumber ∷ Partial ⇒ Number → Frequency
fromNumber = Frequency

toNumber ∷ Frequency → Number
toNumber (Frequency x) = x

codec ∷ Codec Frequency String Unit
codec = Codec.codec decoder encoder

decoder ∷ Decoder Frequency String
decoder = Frequency <$> number

encoder ∷ Encoder Frequency String Unit
encoder _ (Frequency x) = show x

htmlInputMaxVal ∷ Number
htmlInputMaxVal = 10.0

htmlInputMinVal ∷ Number
htmlInputMinVal = 3.0

htmlInputCodec ∷ Codec Frequency String Unit
htmlInputCodec = Codec.codec htmlInputDecoder htmlInputEncoder

htmlInputDecoder ∷ Decoder Frequency String
htmlInputDecoder = do
  x ← number
  if isWithinRange x then pure $ Frequency $ exp x
  else fail outOfRangeErrorMessage
  where
  isWithinRange ∷ Number → Boolean
  isWithinRange x = x >= htmlInputMinVal && x <= htmlInputMaxVal

  outOfRangeErrorMessage ∷ String
  outOfRangeErrorMessage =
    "HTML input form of frequency should be a number between "
      <> show htmlInputMinVal
      <> " and "
      <> show htmlInputMaxVal

htmlInputEncoder ∷ Encoder Frequency String Unit
htmlInputEncoder _ (Frequency x) = show $ log x
