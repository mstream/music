module Music.Model.AudioNodes.AudioNode.Oscillator.Gain
  ( Gain
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

instance Codeable Gain Number Unit where
  codecConf ∷ CodecConf Gain Number Unit
  codecConf =
    { name: "g"
    , parser: P.number
    , render: const show
    , unwrap: toNumber
    , wrap: \x →
        if x >= minValue && x <= maxValue then Right $ Gain x
        else Left $ show x
          <> " is out of bound: "
          <> show minValue
          <> " - "
          <> show maxValue
    }

instance Documentable Gain Number where
  documentation ∷ Documentation Number
  documentation =
    { description: "gain"
    , examples: Set.fromFoldable [ minValue, maxValue / 2.0, maxValue ]
    , valueConstraints: Set.empty

    }

stringCodec ∷ Codec Gain String Unit
stringCodec = Parameter.stringCodec (Proxy ∷ Proxy Gain)

maxValue ∷ Number
maxValue = toNumber top

minValue ∷ Number
minValue = toNumber bottom

toNumber ∷ Gain → Number
toNumber (Gain x) = x

