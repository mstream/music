module Music.Model.AudioNodes.Codec.Code.Parameter
  ( class Codeable
  , class Documentable
  , class Typed
  , CodecConf
  , Documentation
  , ValueConstraint(..)
  , ValueType(..)
  , codecConf
  , documentation
  , stringCodec
  , valueType
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either.Nested (type (\/))
import Data.Set (Set)
import Parsing (Parser, liftEither)
import Parsing.Combinators (optional)
import Parsing.String (string)
import Type.Proxy (Proxy)

class Codeable a i o | a → i o where
  codecConf ∷ CodecConf a i o

type CodecConf a i o =
  { name ∷ String
  , parser ∷ Parser String i
  , render ∷ o → i → String
  , unwrap ∷ a → i
  , wrap ∷ i → String \/ a
  }

class Documentable ∷ ∀ k. k → Type → Constraint
class Documentable a i | a → i where
  documentation ∷ Documentation i

type Documentation a =
  { description ∷ String
  , examples ∷ Set a
  , valueConstraints ∷ Set (ValueConstraint a)
  }

class Typed ∷ ∀ k. k → Constraint
class Typed a where
  valueType ∷ Proxy a → ValueType

instance Typed Number where
  valueType ∷ Proxy Number → ValueType
  valueType _ = Number

instance Typed String where
  valueType ∷ Proxy String → ValueType
  valueType _ = String

data ValueConstraint a = Bounded a a | Enum (Set a)

derive instance Eq a ⇒ Eq (ValueConstraint a)
derive instance Ord a ⇒ Ord (ValueConstraint a)

data ValueType = Number | String

stringCodec
  ∷ ∀ a i o. Codeable a i o ⇒ Proxy a → Codec a String o
stringCodec _ = Codec.codec stringDecoder stringEncoder
  where
  stringDecoder ∷ Decoder a String
  stringDecoder = do
    _ ← optional $ string (conf.name <> "=")
    internalValue ← conf.parser
    liftEither $ conf.wrap internalValue

  stringEncoder ∷ Encoder a String o
  stringEncoder opt =
    (\renderedValue → conf.name <> "=" <> renderedValue)
      <<< conf.render opt
      <<< conf.unwrap

  conf ∷ CodecConf a i o
  conf = codecConf
