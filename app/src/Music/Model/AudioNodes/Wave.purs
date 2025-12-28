module Music.Model.AudioNodes.Wave (Wave(..), codec) where

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

codec ∷ Codec Wave String Unit
codec = Codec.codec decoder encoder

decoder ∷ Decoder Wave String
decoder = do
  waveText ← takeWhile1 isAlphaNum
  case waveText of
    "sine" →
      pure Sine
    "square" →
      pure Square
    _ →
      fail $ "Unrecognized wave type: " <> waveText

encoder ∷ Encoder Wave String Unit
encoder _ = case _ of
  Sine →
    "sine"
  Square →
    "square"

