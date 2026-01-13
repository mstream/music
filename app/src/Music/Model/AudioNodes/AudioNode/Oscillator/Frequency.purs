module Music.Model.AudioNodes.AudioNode.Oscillator.Frequency
  ( Frequency
  , fromNote
  , stringCodec
  , toNumber
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Number as Number
import Data.Set as Set
import Music.Model.AudioNodes.AudioNode.Note
  ( Name(..)
  , Note(..)
  , Octave(..)
  )
import Music.Model.AudioNodes.Codec.Code.Parameter
  ( class Codeable
  , class Documentable
  , CodecConf
  , Documentation
  )
import Music.Model.AudioNodes.Codec.Code.Parameter as Parameter
import Parsing.String.Basic (number) as P
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen
import Type.Proxy (Proxy(..))

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

instance Codeable Frequency Number Unit where
  codecConf ∷ CodecConf Frequency Number Unit
  codecConf =
    { name: "f"
    , parser: P.number
    , render: const show
    , unwrap: toNumber
    , wrap: \x →
        if x >= minValue && x <= maxValue then Right $ Frequency x
        else Left $ show x
          <> " is out of bound: "
          <> show minValue
          <> " - "
          <> show maxValue
    }

instance Documentable Frequency Number where
  documentation ∷ Documentation Number
  documentation =
    { description: "frequency"
    , examples: Set.fromFoldable [ minValue, maxValue / 2.0, maxValue ]
    , valueConstraints: Set.empty

    }

fromNote ∷ Note → Frequency
fromNote (Note name octave) = Frequency $ 440.0 *
  (2.0 `Number.pow` ((Int.toNumber $ midiNoteIndex - 69) / 12.0))
  where
  midiNoteIndex ∷ Int
  midiNoteIndex = midiOctaveIndex + midiNameIndex

  midiNameIndex ∷ Int
  midiNameIndex = case name of
    C →
      0
    Cs →
      1
    D →
      2
    Ds →
      3
    E →
      4
    F →
      5
    Fs →
      6
    G →
      7
    Gs →
      8
    A →
      9
    As →
      10
    B →
      11

  midiOctaveIndex ∷ Int
  midiOctaveIndex = 12 * case octave of
    O1 →
      2
    O2 →
      3
    O3 →
      4
    O4 →
      5
    O5 →
      6
    O6 →
      7
    O7 →
      8
    O8 →
      9

stringCodec ∷ Codec Frequency String Unit
stringCodec = Parameter.stringCodec (Proxy ∷ Proxy Frequency)

maxValue ∷ Number
maxValue = toNumber top

minValue ∷ Number
minValue = toNumber bottom

toNumber ∷ Frequency → Number
toNumber (Frequency x) = x
