module Test.Utils
  ( boundedTestSuite
  , lines
  , orderedTestSuite
  , semigroupTestSuite
  , unsafeGenSorted2
  , unsafeGenSorted3
  , unsafeGenSorted4
  , unsafeGenSorted5
  , unsafeGenSorted6
  , unsafeGenSorted7
  , unsafeGenSorted8
  , words
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result, (<=?), (>=?))
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (listOf) as Gen
import Test.QuickCheck.Laws.Data
  ( checkBounded
  , checkEq
  , checkOrd
  , checkSemigroup
  )
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
        QC.quickCheck lowerBoundaryProp
        QC.quickCheck upperBoundaryProp
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

semigroupTestSuite
  ∷ ∀ a
  . Arbitrary a
  ⇒ Eq a
  ⇒ Semigroup a
  ⇒ Show a
  ⇒ String
  → Proxy a
  → Spec Unit
semigroupTestSuite name proxy = describe
  (name <> " semigroup")
  (it "obeys Semigroup laws" (liftEffect $ checkSemigroup proxy))

unsafeGenSorted2 ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Gen (a /\ a)
unsafeGenSorted2 = unsafePartial do
  [ x1, x2 ] ← genSorted 2
  pure $ x1 /\ x2

unsafeGenSorted3 ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Gen (a /\ a /\ a)
unsafeGenSorted3 = unsafePartial do
  [ x1, x2, x3 ] ← genSorted 3
  pure $ x1 /\ x2 /\ x3

unsafeGenSorted4 ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Gen (a /\ a /\ a /\ a)
unsafeGenSorted4 = unsafePartial do
  [ x1, x2, x3, x4 ] ← genSorted 4
  pure $ x1 /\ x2 /\ x3 /\ x4

unsafeGenSorted5
  ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Gen (a /\ a /\ a /\ a /\ a)
unsafeGenSorted5 = unsafePartial do
  [ x1, x2, x3, x4, x5 ] ← genSorted 5
  pure $ x1 /\ x2 /\ x3 /\ x4 /\ x5

unsafeGenSorted6
  ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Gen (a /\ a /\ a /\ a /\ a /\ a)
unsafeGenSorted6 = unsafePartial do
  [ x1, x2, x3, x4, x5, x6 ] ← genSorted 6
  pure $ x1 /\ x2 /\ x3 /\ x4 /\ x5 /\ x6

unsafeGenSorted7
  ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Gen (a /\ a /\ a /\ a /\ a /\ a /\ a)
unsafeGenSorted7 = unsafePartial do
  [ x1, x2, x3, x4, x5, x6, x7 ] ← genSorted 7
  pure $ x1 /\ x2 /\ x3 /\ x4 /\ x5 /\ x6 /\ x7

unsafeGenSorted8
  ∷ ∀ a
  . Arbitrary a
  ⇒ Ord a
  ⇒ Gen (a /\ a /\ a /\ a /\ a /\ a /\ a /\ a)
unsafeGenSorted8 = unsafePartial do
  [ x1, x2, x3, x4, x5, x6, x7, x8 ] ← genSorted 8
  pure $ x1 /\ x2 /\ x3 /\ x4 /\ x5 /\ x6 /\ x7 /\ x8

genSorted ∷ ∀ a. Arbitrary a ⇒ Ord a ⇒ Int → Gen (Array a)
genSorted quantity = do
  values ← Gen.listOf quantity arbitrary
  pure $ Set.toUnfoldable $ Set.fromFoldable values

