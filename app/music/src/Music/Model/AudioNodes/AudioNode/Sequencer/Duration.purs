module Music.Model.AudioNodes.AudioNode.Sequencer.Duration
  ( Duration
  , d1
  , d2
  , d3
  , d4
  , parameterStringCodec
  , toInt
  , valueStringCodec
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Set as Set
import Data.Value as Value
import Music.Model.AudioNodes.AudioNode.Code.Documentable
  ( class Documentable
  , Documentation
  )
import Music.Model.AudioNodes.AudioNode.Code.Parameter as Parameter
import Parsing.String (eof)
import Parsing.String.Basic as P
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen

newtype Duration = Duration Int

derive newtype instance Eq Duration
derive newtype instance Ord Duration
derive newtype instance Show Duration

instance Arbitrary Duration where
  arbitrary = Duration <$> Gen.chooseInt minValue maxValue

instance Bounded Duration where
  bottom ∷ Duration
  bottom = Duration minValue
  top ∷ Duration
  top = Duration maxValue

instance Parameter.Codeable Duration Int Unit where
  name ∷ String
  name = "d"

instance Value.Codeable Duration Int Unit where
  codecConf ∷ Value.CodecConf Duration Int Unit
  codecConf =
    { internalValueParser: P.intDecimal
    , renderInternalValue: const show
    , unwrap: toInt
    , wrap: \x →
        if x >= minValue && x <= maxValue then Right $ Duration x
        else Left $ show x
          <> " is out of bound: "
          <> show minValue
          <> " - "
          <> show maxValue
    }

instance Documentable Duration Int where
  documentation ∷ Documentation Int
  documentation =
    { description: "duration"
    , examples: Set.fromFoldable [ minValue, maxValue / 2, maxValue ]
    }

parameterStringCodec ∷ Codec Duration String Unit
parameterStringCodec = Parameter.stringCodec

valueStringCodec ∷ Codec Duration String Unit
valueStringCodec = Codec.codec
  (Codec.decoder Value.stringCodec <* eof)
  (Codec.encoder Value.stringCodec)

maxValue ∷ Int
maxValue = 4

minValue ∷ Int
minValue = 1

d1 ∷ Duration
d1 = Duration 1

d2 ∷ Duration
d2 = Duration 2

d3 ∷ Duration
d3 = Duration 3

d4 ∷ Duration
d4 = Duration 4

toInt ∷ Duration → Int
toInt (Duration i) = i
