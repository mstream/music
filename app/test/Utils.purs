module Test.Utils (boundedTestSuite, lines, orderedTestSuite, words) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Set (Set)
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Test.QuickCheck
  ( class Arbitrary
  , Result
  , quickCheck
  , (<=?)
  , (>=?)
  )
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws.Data (checkBounded, checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

lines ∷ Array String → String
lines = String.joinWith "\n"

words ∷ Array String → String
words = String.joinWith " "

type BoundedTestSuiteConf a =
  { generator ∷ Gen a
  , name ∷ String
  }

boundedTestSuite
  ∷ ∀ a
  . Arbitrary a
  ⇒ Bounded a
  ⇒ Show a
  ⇒ BoundedTestSuiteConf a
  → Spec Unit
boundedTestSuite { generator, name } = describe
  (name <> " bounded value")
  do
    generatedTestCase
    lawsTestCase
  where
  generatedTestCase ∷ Spec Unit
  generatedTestCase = it
    "is within boundries - generated"
    ( liftEffect do
        quickCheck lowerBoundaryProp
        quickCheck upperBoundaryProp
    )
    where
    lowerBoundaryProp ∷ Gen Result
    lowerBoundaryProp = do
      value ← generator
      pure $ value <=? top

    upperBoundaryProp ∷ Gen Result
    upperBoundaryProp = do
      value ← generator
      pure $ value >=? bottom

  lawsTestCase ∷ Spec Unit
  lawsTestCase = it "obeys Bounded laws"
    ( liftEffect do
        checkBounded (Proxy ∷ Proxy a)
    )

type OrderedTestSuiteConf a =
  { examples ∷ Set (NonEmptyArray a)
  , name ∷ String
  }

orderedTestSuite
  ∷ ∀ a
  . Arbitrary a
  ⇒ Ord a
  ⇒ Show a
  ⇒ OrderedTestSuiteConf a
  → Spec Unit
orderedTestSuite { examples, name } = describe
  (name <> " ordered value")
  do
    traverse_ exampleTestCase examples
    lawsTestCase
  where
  exampleTestCase ∷ NonEmptyArray a → Spec Unit
  exampleTestCase expected = it
    "sorts correctly - example"
    (ArrayNE.sort expected `shouldEqual` expected)

  lawsTestCase ∷ Spec Unit
  lawsTestCase = it "obeys Eq and Ord laws"
    ( liftEffect do
        checkEq (Proxy ∷ Proxy a)
        checkOrd (Proxy ∷ Proxy a)
    )

