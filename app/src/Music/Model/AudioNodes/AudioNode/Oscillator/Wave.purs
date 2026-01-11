module Music.Model.AudioNodes.AudioNode.Oscillator.Wave
  ( Wave(..)
  , stringCodec
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Music.Model.AudioNodes.Codec.Code.Parameter
  ( class Codeable
  , class Documentable
  , CodecConf
  , Documentation
  )
import Music.Model.AudioNodes.Codec.Code.Parameter as Parameter
import Parsing.Combinators (choice) as P
import Parsing.String (string) as P
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)
import Type.Proxy (Proxy(..))

data Wave = Sine | Square

derive instance Eq Wave
derive instance Generic Wave _
derive instance Ord Wave

instance Arbitrary Wave where
  arbitrary = genericArbitrary

instance Codeable Wave String Unit where
  codecConf ∷ CodecConf Wave String Unit
  codecConf =
    { name: "w"
    , parser: P.choice [ P.string "sine", P.string "square" ]
    , render: const identity
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
    , valueConstraints: Set.empty
    }

instance Show Wave where
  show = genericShow

stringCodec ∷ Codec Wave String Unit
stringCodec = Parameter.stringCodec (Proxy ∷ Proxy Wave)
