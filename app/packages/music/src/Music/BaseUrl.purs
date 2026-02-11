module Music.BaseUrl
  ( BaseUrl
  , fromSegments
  , segments
  , toString
  ) where

import Prelude

import Data.Array as Array
import Data.Char.Gen (genAlphaLowercase) as Gen
import Data.Foldable (class Foldable)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (toString) as StringNE
import Data.String.NonEmpty.CodeUnits (fromFoldable1) as StringNE
import Data.Unfoldable (class Unfoldable)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (arrayOf, arrayOf1) as Gen

newtype BaseUrl = BaseUrl (Array NonEmptyString)

instance Arbitrary BaseUrl where
  arbitrary = fromSegments <$> Gen.arrayOf genSegment
    where
    genSegment ∷ Gen NonEmptyString
    genSegment = StringNE.fromFoldable1 <$> Gen.arrayOf1
      Gen.genAlphaLowercase

fromSegments ∷ ∀ f. Foldable f ⇒ f NonEmptyString → BaseUrl
fromSegments = BaseUrl <<< Array.fromFoldable

segments ∷ ∀ f. Unfoldable f ⇒ BaseUrl → f String
segments (BaseUrl ss) = Array.toUnfoldable $ StringNE.toString <$> ss

toString ∷ BaseUrl → String
toString = String.joinWith "/" <<< segments
