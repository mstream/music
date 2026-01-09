module Music.Model.AudioNodes.Wave (Wave(..), stringCodec) where

import Prelude

import Data.CodePoint.Unicode (isAlphaNum)
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Parsing (fail)
import Parsing.String.Basic (takeWhile1)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Wave = Sine | Square

derive instance Eq Wave
derive instance Generic Wave _
derive instance Ord Wave

instance Arbitrary Wave where
  arbitrary = genericArbitrary

instance Show Wave where
  show = genericShow

stringCodec ∷ Codec Wave String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder Wave String
stringDecoder = do
  waveText ← takeWhile1 isAlphaNum
  case waveText of
    "sine" →
      pure Sine
    "square" →
      pure Square
    _ →
      fail $ "Unrecognized wave type: " <> waveText

stringEncoder ∷ Encoder Wave String Unit
stringEncoder _ = case _ of
  Sine →
    "sine"
  Square →
    "square"

