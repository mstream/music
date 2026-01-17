module Music.Model.AudioNodes.AudioNode.Code.Value
  ( class Codeable
  , CodecConf
  , codecConf
  , stringCodec
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either.Nested (type (\/))
import Parsing (Parser, liftEither)
import Type.Proxy (Proxy)

class Codeable a i o | a → i o where
  codecConf ∷ CodecConf a i o

type CodecConf a i o =
  { internalValueParser ∷ Parser String i
  , renderInternalValue ∷ o → i → String
  , unwrap ∷ a → i
  , wrap ∷ i → String \/ a
  }

stringCodec
  ∷ ∀ a i o. Codeable a i o ⇒ Proxy a → Codec a String o
stringCodec _ = Codec.codec stringDecoder stringEncoder
  where
  stringDecoder ∷ Decoder a String
  stringDecoder = do
    internalValue ← conf.internalValueParser
    liftEither $ conf.wrap internalValue

  stringEncoder ∷ Encoder a String o
  stringEncoder opt = conf.renderInternalValue opt <<< conf.unwrap

  conf ∷ CodecConf a i o
  conf = codecConf

