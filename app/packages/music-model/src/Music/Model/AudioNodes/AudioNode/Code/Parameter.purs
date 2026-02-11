module Music.Model.AudioNodes.AudioNode.Code.Parameter
  ( class Codeable
  , name
  , stringCodec
  ) where

import Prelude

import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Value
  ( CodecConf
  )
import Data.Value as Value
import Parsing (liftEither)
import Parsing.String (string)

class Value.Codeable a i o ⇐ Codeable a i o | a → i o where
  name ∷ String

stringCodec
  ∷ ∀ a i o
  . Codeable a i o
  ⇒ Value.Codeable a i o
  ⇒ Codec a String o
stringCodec = Codec.codec stringDecoder stringEncoder
  where
  stringDecoder ∷ Decoder a String
  stringDecoder = do
    void $ string (parameterName <> "=")
    internalValue ← valueCodecConf.internalValueParser
    liftEither $ valueCodecConf.wrap internalValue

  stringEncoder ∷ Encoder a String o
  stringEncoder opt =
    (\renderedValue → parameterName <> "=" <> renderedValue)
      <<< valueCodecConf.renderInternalValue opt
      <<< valueCodecConf.unwrap

  parameterName ∷ String
  parameterName = name @a @i @o

  valueCodecConf ∷ CodecConf a i o
  valueCodecConf = Value.codecConf

