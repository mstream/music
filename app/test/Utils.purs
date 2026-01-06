module Test.Utils (lines, orderTestSuite, words) where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

lines ∷ Array String → String
lines = String.joinWith "\n"

words ∷ Array String → String
words = String.joinWith " "

type OrderTestSuiteConf a =
  { examples ∷ Set (Array a)
  , name ∷ String
  }

orderTestSuite
  ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Show a ⇒ OrderTestSuiteConf a → Spec Unit
orderTestSuite { examples, name } = describe (name <> " ordered value")
  do
    traverse_ exampleTestCase examples
    generatedTestCase
  where
  exampleTestCase ∷ Array a → Spec Unit
  exampleTestCase expected = it
    "sorts correctly - example"
    (Array.sort expected `shouldEqual` expected)

  generatedTestCase ∷ Spec Unit
  generatedTestCase = it "obeys Eq and Ord laws"
    ( liftEffect do
        checkEq (Proxy ∷ Proxy a)
        checkOrd (Proxy ∷ Proxy a)
    )

