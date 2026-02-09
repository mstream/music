module Music.Model.AudioNodes.AudioNode.Note
  ( Config
  , Name(..)
  , Note(..)
  , Octave(..)
  , parameterStringCodec
  , valueStringCodec
  ) where

import Prelude

import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Value (CodecConf)
import Data.Value as Value
import Music.Model.AudioNodes.AudioNode.Code.Parameter as Parameter
import Parsing (Parser)
import Parsing (fail) as P
import Parsing.Combinators (optionMaybe) as P
import Parsing.String (anyChar, char) as P
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

type Config = { name ∷ Name, octave ∷ Octave }

newtype Note = Note Config

derive instance Eq Note
derive instance Generic Note _

instance Ord Note where
  compare (Note a) (Note b) =
    case compare a.octave b.octave of
      EQ →
        compare a.name b.name
      res →
        res

derive newtype instance Bounded Note

instance Arbitrary Note where
  arbitrary = genericArbitrary

instance Parameter.Codeable Note Config Unit where
  name ∷ String
  name = "n"

instance Value.Codeable Note Config Unit where
  codecConf ∷ CodecConf Note Config Unit
  codecConf =
    { internalValueParser: configParser
    , renderInternalValue: const \{ name, octave } →
        renderName name <> renderOctave octave
    , unwrap: \(Note conf) → conf
    , wrap: Right <<< Note
    }

instance Show Note where
  show = genericShow

parameterStringCodec ∷ Codec Note String Unit
parameterStringCodec = Parameter.stringCodec

valueStringCodec ∷ Codec Note String Unit
valueStringCodec = Value.stringCodec

data Name = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B

derive instance Eq Name
derive instance Generic Name _
derive instance Ord Name

instance Arbitrary Name where
  arbitrary = genericArbitrary

instance Bounded Name where
  bottom ∷ Name
  bottom = C
  top ∷ Name
  top = B

instance Show Name where
  show = genericShow

data Octave = O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8

derive instance Eq Octave
derive instance Generic Octave _
derive instance Ord Octave

instance Arbitrary Octave where
  arbitrary = genericArbitrary

instance Bounded Octave where
  bottom ∷ Octave
  bottom = O1
  top ∷ Octave
  top = O8

instance Show Octave where
  show = genericShow

configParser ∷ Parser String Config
configParser = do
  name ← nameParser
  octave ← octaveParser
  pure { name, octave }

nameParser ∷ Parser String Name
nameParser = do
  c1 ← P.anyChar
  c2 ← P.optionMaybe $ P.char '#'
  case c2 of
    Just '#' →
      case c1 of
        'C' →
          pure Cs
        'D' →
          pure Ds
        'F' →
          pure Fs
        'G' →
          pure Gs
        'A' →
          pure As
        _ →
          P.fail ""
    Just _ →
      fail
    Nothing →
      case c1 of
        'C' →
          pure C
        'D' →
          pure D
        'E' →
          pure E
        'F' →
          pure F
        'G' →
          pure G
        'A' →
          pure A
        'B' →
          pure B
        _ →
          fail

  where
  fail ∷ Parser String Name
  fail = P.fail "unrecognized note name"

renderName ∷ Name → String
renderName = case _ of
  C →
    "C"
  Cs →
    "C#"
  D →
    "D"
  Ds →
    "D#"
  E →
    "E"
  F →
    "F"
  Fs →
    "F#"
  G →
    "G"
  Gs →
    "G#"
  A →
    "A"
  As →
    "A#"
  B →
    "B"

octaveParser ∷ Parser String Octave
octaveParser = do
  c ← P.anyChar
  case c of
    '1' →
      pure O1
    '2' →
      pure O2
    '3' →
      pure O3
    '4' →
      pure O4
    '5' →
      pure O5
    '6' →
      pure O6
    '7' →
      pure O7
    '8' →
      pure O8
    _ →
      fail
  where
  fail ∷ Parser String Octave
  fail = P.fail "unrecognized note octave"

renderOctave ∷ Octave → String
renderOctave = case _ of
  O1 →
    "1"
  O2 →
    "2"
  O3 →
    "3"
  O4 →
    "4"
  O5 →
    "5"
  O6 →
    "6"
  O7 →
    "7"
  O8 →
    "8"
