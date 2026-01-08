module Test.Laws (lawsTestSuite) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)

lawsTestSuite ∷ String → Effect Unit → Spec Unit
lawsTestSuite name execution = describe name
  (it "obeys class laws" (liftEffect execution))
