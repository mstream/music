module Music.Model.AudioNodes.AudioNode.Oscillator.Frequency
  ( Frequency
  , stringCodec
  , toNumber
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Set as Set
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

stringCodec ∷ Codec Frequency String Unit
stringCodec = Parameter.stringCodec (Proxy ∷ Proxy Frequency)

maxValue ∷ Number
maxValue = toNumber top

minValue ∷ Number
minValue = toNumber bottom

toNumber ∷ Frequency → Number
toNumber (Frequency x) = x
