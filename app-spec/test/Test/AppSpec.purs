module Test.AppSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Playwright (Selector(..), URL)
import Playwright as PW
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions.String (shouldContain)
import Test.Utils (withBrowserPage)
import Untagged.Union (fromOneOf)

spec ∷ URL → Spec Unit
spec url = describe "TODO" do
  it "TODO" do
    withBrowserPage url \page → do
      mbText ← fromOneOf <$> PW.textContent page (Selector "body")
      case mbText of
        Just text →
          text `shouldContain` "MUSIC"
        Nothing →
          fail "DOM is missing body."

