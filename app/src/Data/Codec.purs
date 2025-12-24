module Data.Codec (Codec, Decoder, Encoder, codec, decoder, encoder) where

import Parsing (Parser)

data Codec a o = Codec (Decoder a) (Encoder a o)
type Decoder a = Parser String a
type Encoder a o = o → a → String

codec ∷ ∀ a o. Decoder a → Encoder a o → Codec a o
codec = Codec

decoder ∷ ∀ a o. Codec a o → Decoder a
decoder (Codec dec _) = dec

encoder ∷ ∀ a o. Codec a o → Encoder a o
encoder (Codec _ enc) = enc

