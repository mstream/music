module Mermaid.DiagramDef.Blocks.BlockId
  ( class IsAlphaNumeric
  , class CharLike
  , BlockId
  , stringCodec
  , make
  , toChar
  , AlphaChar(..)
  , NumericChar(..)
  ) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Foldable (class Foldable, foldr)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Gen (genAlphaString) as Gen
import Parsing.String.Basic (takeWhile1) as P
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype BlockId = BlockId String

derive newtype instance Eq BlockId
derive newtype instance Ord BlockId

instance Arbitrary BlockId where
  arbitrary = BlockId <$> Gen.genAlphaString

instance Semigroup BlockId where
  append (BlockId s1) (BlockId s2) = BlockId $ s1 <> "-" <> s2

instance Show BlockId where
  show (BlockId s) = s

stringCodec ∷ Codec BlockId String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder BlockId String
stringDecoder = BlockId <$> P.takeWhile1 isAlphaNum

stringEncoder ∷ Encoder BlockId String Unit
stringEncoder _ (BlockId s) = s

class CharLike a where
  toChar ∷ a → Char

class CharLike a ⇐ IsAlphaNumeric a

data AlphaChar
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z

instance CharLike AlphaChar where
  toChar A = 'a'
  toChar B = 'b'
  toChar C = 'c'
  toChar D = 'd'
  toChar E = 'e'
  toChar F = 'f'
  toChar G = 'g'
  toChar H = 'h'
  toChar I = 'i'
  toChar J = 'j'
  toChar K = 'k'
  toChar L = 'l'
  toChar M = 'm'
  toChar N = 'n'
  toChar O = 'o'
  toChar P = 'p'
  toChar Q = 'q'
  toChar R = 'r'
  toChar S = 's'
  toChar T = 't'
  toChar U = 'u'
  toChar V = 'v'
  toChar W = 'w'
  toChar X = 'x'
  toChar Y = 'y'
  toChar Z = 'z'

instance IsAlphaNumeric AlphaChar

data NumericChar = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9

instance CharLike NumericChar where
  toChar N0 = '0'
  toChar N1 = '1'
  toChar N2 = '2'
  toChar N3 = '3'
  toChar N4 = '4'
  toChar N5 = '5'
  toChar N6 = '6'
  toChar N7 = '7'
  toChar N8 = '8'
  toChar N9 = '9'

instance IsAlphaNumeric NumericChar

make ∷ ∀ c f. Foldable f ⇒ IsAlphaNumeric c ⇒ AlphaChar → f c → BlockId
make firstChar otherChars = BlockId identifier
  where
  identifier ∷ String
  identifier = fromCharArray (Array.cons headChar tailChars)

  headChar ∷ Char
  headChar = toChar firstChar

  tailChars ∷ Array Char
  tailChars = foldr (Array.cons <<< toChar) [] otherChars
