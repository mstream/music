module Mermaid.DiagramDef.Blocks.BlockId
  ( BlockId
  , stringCodec
  , make
  ) where

import Prelude

import Data.Array as Array
import Data.Codec (Codec)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (foldl1)
import Data.String.CodeUnits as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (join1With, nes, toString) as StringNE
import Data.String.NonEmpty.CodeUnits (cons) as StringNE
import Data.Value (class Codeable)
import Data.Value as Value
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar)
import Mermaid.DiagramDef.Blocks.BlockId.Class (fromChar, toChar)
import Mermaid.DiagramDef.Blocks.BlockId.NumChar (NumChar)
import Parsing (Parser)
import Parsing (fail) as P
import Parsing.Combinators (many, sepBy1, try) as P
import Parsing.String (anyChar, char) as P
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Type.Proxy (Proxy(..))

newtype BlockId = BlockId NonEmptyString

derive newtype instance Eq BlockId
derive newtype instance Ord BlockId
derive newtype instance Show BlockId

instance Arbitrary BlockId where
  arbitrary = foldl1 append <$> Gen.arrayOf1 genSegment
    where
    genSegment ∷ Gen BlockId
    genSegment = do
      firstCharacter ← QC.arbitrary
      otherAlphaCharacters ← Gen.arrayOf $ QC.arbitrary @AlphaChar
      numCharacters ← Gen.arrayOf $ QC.arbitrary @NumChar
      pure $ make firstCharacter otherAlphaCharacters numCharacters

instance Codeable BlockId NonEmptyString Unit where
  codecConf ∷ Value.CodecConf BlockId NonEmptyString Unit
  codecConf =
    { internalValueParser:
        let
          parseSegment ∷ Parser String NonEmptyString
          parseSegment = do
            firstChar ← alphaCharParser
            otherAlphaChars ← P.many alphaCharParser
            numChars ← P.many numCharParser
            pure $ nonEmptyString firstChar otherAlphaChars numChars
        in
          StringNE.join1With "_" <$> P.sepBy1 parseSegment (P.char '_')
    , renderInternalValue: const StringNE.toString
    , unwrap: \(BlockId s) → s
    , wrap: \s → Right $ BlockId s
    }

instance Semigroup BlockId where
  append (BlockId s1) (BlockId s2) = BlockId
    $ s1 <> StringNE.nes (Proxy ∷ Proxy "_") <> s2

numCharParser ∷ Parser String Char
numCharParser = P.try $ toChar <$> parser
  where
  parser ∷ Parser String NumChar
  parser = do
    c ← P.anyChar
    case fromChar c of
      Just numChar →
        pure numChar
      Nothing →
        P.fail $ "not a numeric character: " <> show c

alphaCharParser ∷ Parser String Char
alphaCharParser = P.try $ toChar <$> parser
  where
  parser ∷ Parser String AlphaChar
  parser = do
    c ← P.anyChar
    case fromChar c of
      Just alphaChar →
        pure alphaChar
      Nothing →
        P.fail $ "not an alpha character: " <> show c

stringCodec ∷ Codec BlockId String Unit
stringCodec = Value.stringCodec

make
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ Semigroup (f Char)
  ⇒ AlphaChar
  → f AlphaChar
  → f NumChar
  → BlockId
make firstChar otherAlphaCharacters numericChars = BlockId $
  nonEmptyString
    (toChar firstChar)
    (toChar <$> otherAlphaCharacters)
    (toChar <$> numericChars)

nonEmptyString
  ∷ ∀ f
  . Foldable f
  ⇒ Semigroup (f Char)
  ⇒ Char
  → f Char
  → f Char
  → NonEmptyString
nonEmptyString firstChar middleChars endChars = StringNE.cons
  firstChar
  (String.fromCharArray $ Array.fromFoldable $ middleChars <> endChars)
