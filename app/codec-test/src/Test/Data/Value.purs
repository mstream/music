module Test.Data.Value (valueTestSuite) where

import Prelude

import Data.Either (Either(..))
import Data.Value (class Codeable)
import Data.Value as Value
import Effect.Class (liftEffect)
import Test.QuickCheck (Result)
import Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy)

valueTestSuite
  ∷ ∀ a i o
  . Arbitrary a
  ⇒ Codeable a i o
  ⇒ Eq a
  ⇒ Show a
  ⇒ Proxy a
  → String
  → Spec Unit
valueTestSuite _ name = describe
  (name <> " value")
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

