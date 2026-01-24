module Gen (arbitraryMap, arbitrarySet, genMap, genSet, genUnique) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

arbitrarySet ∷ ∀ v. Arbitrary v ⇒ Ord v ⇒ Gen (Set v)
arbitrarySet = genSet arbitrary

arbitraryMap ∷ ∀ k v. Arbitrary k ⇒ Arbitrary v ⇒ Ord k ⇒ Gen (Map k v)
arbitraryMap = genMap arbitrary arbitrary

genSet ∷ ∀ m v. MonadGen m ⇒ MonadRec m ⇒ Ord v ⇒ m v → m (Set v)
genSet genValue = Set.fromMap <$> genMap genValue (pure unit)

genMap
  ∷ ∀ k m v. MonadGen m ⇒ MonadRec m ⇒ Ord k ⇒ m k → m v → m (Map k v)
genMap genKey genValue = do
  keys ← Gen.unfoldable genKey
  values ← Gen.unfoldable genValue
  pure $ Map.fromFoldable $ Array.zip keys values

genUnique ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Set a → Gen a
genUnique existingValues = arbitrary `Gen.suchThat` isUnique
  where
  isUnique ∷ a → Boolean
  isUnique value = not $ value `Set.member` existingValues
