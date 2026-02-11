module Music.Model.AudioNodes.AudioNode.Oscillator.Wave
  ( Wave(..)
  , parameterStringCodec
  , valueStringCodec
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Value as Value
import Music.Model.AudioNodes.AudioNode.Code.Documentable
  ( class Documentable
  , Documentation
  )
import Music.Model.AudioNodes.AudioNode.Code.Parameter as Parameter
import Parsing.Combinators (choice) as P
import Parsing.String (string) as P
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Wave = Sine | Square

derive instance Eq Wave
derive instance Generic Wave _
derive instance Ord Wave

instance Arbitrary Wave where
  arbitrary = genericArbitrary

instance Parameter.Codeable Wave String Unit where
  name ∷ String
  name = "w"

instance Value.Codeable Wave String Unit where
  codecConf ∷ Value.CodecConf Wave String Unit
  codecConf =
    { internalValueParser: P.choice
        [ P.string "sine", P.string "square" ]
    , renderInternalValue: const identity
    , unwrap: case _ of
        Sine →
          "sine"
        Square →
          "square"
    , wrap: case _ of
        "sine" →
          Right Sine
        "square" →
          Right Square
        s →
          Left $ "Unknown value: " <> s
    }

instance Documentable Wave String where
  documentation ∷ Documentation String
  documentation =
    { description: "wave shape"
    , examples: Set.fromFoldable [ "sine", "square" ]
    }

instance Show Wave where
  show = genericShow

parameterStringCodec ∷ Codec Wave String Unit
parameterStringCodec = Parameter.stringCodec

valueStringCodec ∷ Codec Wave String Unit
valueStringCodec = Value.stringCodec
