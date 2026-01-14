module Test.Utils
  ( boundedTestSuite
  , codeValueTestSuite
  , lines
  , orderedTestSuite
  , words
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Either (Either(..))
import Data.Set (Set)
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Music.Model.AudioNodes.AudioNode.Code.Value as Value
import Test.QuickCheck
  ( class Arbitrary
  , Result
  , (<=?)
  , (>=?)
  )
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws.Data (checkBounded, checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

lines ∷ Array String → String
lines = String.joinWith "\n"

words ∷ Array String → String
words = String.joinWith " "

codeValueTestSuite
  ∷ ∀ a i o
  . Arbitrary a
  ⇒ Value.Codeable a i o
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → String
  → Spec Unit
codeValueTestSuite _ name = describe
  (name <> " code value")
  generatedTestCase
  where
  generatedTestCase ∷ Spec Unit
  generatedTestCase = it
    "has a valid generation implementation - generated"
    (liftEffect $ QC.quickCheck prop)

  prop ∷ Gen Result
  prop = do
    value ← QC.arbitrary
    pure case valueCodecConf.wrap (valueCodecConf.unwrap value) of
      Left errorMessage →
        QC.Failed $ "Invalid generated value (" <> show value <> "): "
          <> errorMessage
      Right _ →
        QC.Success

  valueCodecConf ∷ Value.CodecConf a i o
  valueCodecConf = Value.codecConf

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

