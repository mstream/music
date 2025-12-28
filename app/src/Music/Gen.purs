module Music.Gen (arbitraryMap, genMap) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

arbitraryMap ∷ ∀ k v. Arbitrary k ⇒ Arbitrary v ⇒ Ord k ⇒ Gen (Map k v)
arbitraryMap = genMap arbitrary arbitrary

genMap
  ∷ ∀ k m v. MonadGen m ⇒ MonadRec m ⇒ Ord k ⇒ m k → m v → m (Map k v)
genMap genKey genValue = do
  keys ← Gen.unfoldable genKey
  values ← Gen.unfoldable genValue
  pure $ Map.fromFoldable $ Array.zip keys values

