module Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( Sequence(..)
  , frequencyParameterStringCodec
  , frequencyValueStringCodec
  , gainParameterStringCodec
  , gainValueStringCodec
  , toArray
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Set as Set
import Data.String as String
import Data.Value as Value
import Music.Model.AudioNodes.AudioNode.Code.Documentable
  ( class Documentable
  , Documentation
  )
import Music.Model.AudioNodes.AudioNode.Code.Parameter as Parameter
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Parsing (Parser)
import Parsing.Combinators (between, sepBy1) as P
import Parsing.String (char) as P
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as Gen

newtype Sequence a = Sequence (NonEmptyArray a)

instance Arbitrary a ⇒ Arbitrary (Sequence a) where
  arbitrary = Sequence <$> Gen.arrayOf1 arbitrary

instance
  Parameter.Codeable (Sequence Frequency) (NonEmptyArray Frequency) Unit where
  name ∷ String
  name = "s"

instance
  Value.Codeable (Sequence Frequency) (NonEmptyArray Frequency) Unit where
  codecConf
    ∷ Value.CodecConf (Sequence Frequency) (NonEmptyArray Frequency)
        Unit
  codecConf =
    { internalValueParser: parser Frequency.valueStringCodec
    , renderInternalValue: render Frequency.valueStringCodec
    , unwrap: \(Sequence xs) → xs
    , wrap: Right <<< Sequence
    }

instance Parameter.Codeable (Sequence Gain) (NonEmptyArray Gain) Unit where
  name ∷ String
  name = "s"

instance Value.Codeable (Sequence Gain) (NonEmptyArray Gain) Unit where
  codecConf
    ∷ Value.CodecConf (Sequence Gain) (NonEmptyArray Gain) Unit
  codecConf =
    { internalValueParser: parser Gain.valueStringCodec
    , renderInternalValue: render Gain.valueStringCodec
    , unwrap: \(Sequence xs) → xs
    , wrap: Right <<< Sequence
    }

parser ∷ ∀ a. Codec a String Unit → Parser String (NonEmptyArray a)
parser elementStringCodec = ArrayNE.fromFoldable1
  <$> P.between (P.char '[') (P.char ']')
    (elementParser `P.sepBy1` P.char ' ')
  where
  elementParser ∷ Parser String a
  elementParser = Codec.decoder elementStringCodec

render ∷ ∀ a. Codec a String Unit → Unit → NonEmptyArray a → String
render elementStringCodec _ xs = "["
  <> String.joinWith " " (renderElement <$> ArrayNE.toArray xs)
  <> "]"
  where
  renderElement ∷ a → String
  renderElement = Codec.encoder elementStringCodec unit

instance Documentable (Sequence Frequency) (NonEmptyArray Frequency) where
  documentation ∷ Documentation (NonEmptyArray Frequency)
  documentation =
    { description: "frequency sequence"
    , examples: Set.fromFoldable [ example1, example2 ]
    }
    where
    example1 ∷ NonEmptyArray Frequency
    example1 = ArrayNE.cons' bottom []

    example2 ∷ NonEmptyArray Frequency
    example2 = ArrayNE.cons' bottom [ top ]

instance Documentable (Sequence Gain) (NonEmptyArray Gain) where
  documentation ∷ Documentation (NonEmptyArray Gain)
  documentation =
    { description: "gain sequence"
    , examples: Set.fromFoldable [ example1, example2 ]
    }
    where
    example1 ∷ NonEmptyArray Gain
    example1 = ArrayNE.cons' bottom []

    example2 ∷ NonEmptyArray Gain
    example2 = ArrayNE.cons' bottom [ top ]

instance Eq a ⇒ Eq (Sequence a) where
  eq (Sequence xs1) (Sequence xs2) = eq
    (ArrayNE.toArray xs1)
    (ArrayNE.toArray xs2)

instance Ord a ⇒ Ord (Sequence a) where
  compare (Sequence xs1) (Sequence xs2) = compare
    (ArrayNE.toArray xs1)
    (ArrayNE.toArray xs2)

instance Show a ⇒ Show (Sequence a) where
  show (Sequence xs) = show $ ArrayNE.toArray xs

frequencyParameterStringCodec ∷ Codec (Sequence Frequency) String Unit
frequencyParameterStringCodec = Parameter.stringCodec

frequencyValueStringCodec ∷ Codec (Sequence Frequency) String Unit
frequencyValueStringCodec = Value.stringCodec

gainParameterStringCodec ∷ Codec (Sequence Gain) String Unit
gainParameterStringCodec = Parameter.stringCodec

gainValueStringCodec ∷ Codec (Sequence Gain) String Unit
gainValueStringCodec = Value.stringCodec

toArray ∷ ∀ a. Sequence a → NonEmptyArray a
toArray (Sequence xs) = xs
