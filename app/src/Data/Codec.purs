module Data.Codec (Codec, Decoder, Encoder, codec, decoder, encoder) where

import Parsing (Parser)

data Codec d e o = Codec (Decoder d e) (Encoder d e o)
type Decoder d e = Parser e d
type Encoder d e o = o → d → e

codec ∷ ∀ d e o. Decoder d e → Encoder d e o → Codec d e o
codec = Codec

decoder ∷ ∀ d e o. Codec d e o → Decoder d e
decoder (Codec dec _) = dec

encoder ∷ ∀ d e o. Codec d e o → Encoder d e o
encoder (Codec _ enc) = enc

