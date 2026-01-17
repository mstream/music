module Music.Model.AudioNodes.AudioNode.Oscillator.Gain
  ( Gain(..)
  , stringCodec
  , toNumber
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Set as Set
import Music.Model.AudioNodes.AudioNode.Code.Documentable
  ( class Documentable
  , Documentation
  )
import Music.Model.AudioNodes.AudioNode.Code.Value
  ( class Codeable
  , CodecConf
  )
import Music.Model.AudioNodes.AudioNode.Code.Value as Value
import Parsing.String.Basic as P
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
  bottom = Gain minValue
  top ∷ Gain
  top = Gain maxValue

instance Codeable Gain Number Unit where
  codecConf ∷ CodecConf Gain Number Unit
  codecConf =
    { internalValueParser: P.number
    , renderInternalValue: const show
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
    }

stringCodec ∷ Codec Gain String Unit
stringCodec = Value.stringCodec (Proxy ∷ Proxy Gain)

maxValue ∷ Number
maxValue = one

minValue ∷ Number
minValue = zero

toNumber ∷ Gain → Number
toNumber (Gain x) = x
