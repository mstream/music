module Data.LzString
  ( decode
  , encode
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List.NonEmpty (NonEmptyList)
import Data.String as String
import Foreign (Foreign, ForeignError)
import Foreign as F

decode ∷ String → String \/ String
decode = stringFromForeign "_decompressFromEncodedURIComponent"
  <<< _decompressFromEncodedURIComponent

encode ∷ String → String \/ String
encode s = do
  encoded ← stringFromForeign
    "_compressToEncodedURIComponent"
    (_compressToEncodedURIComponent s)

  if String.length encoded <= 1024 then Right encoded
  else Left "Too long to encode."

stringFromForeign ∷ String → Foreign → String \/ String
stringFromForeign name v = case result of
  Left foreignErrors →
    Left $ errorMessage foreignErrors
  Right s →
    Right s
  where
  result ∷ NonEmptyList ForeignError \/ String
  result = runExcept $ F.readString v

  errorMessage ∷ NonEmptyList ForeignError → String
  errorMessage errors = "Unexpected foreign value \""
    <> name
    <> "\": "
    <> String.joinWith ","
      (Array.fromFoldable $ F.renderForeignError <$> errors)

foreign import _compressToEncodedURIComponent ∷ String → Foreign
foreign import _decompressFromEncodedURIComponent ∷ String → Foreign
