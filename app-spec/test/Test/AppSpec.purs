module Test.AppSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Playwright (Selector(..), URL(..))
import Playwright as PW
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions.String (shouldContain)
import Test.Utils (withBrowserPage)
import Untagged.Union (fromOneOf)

spec ∷ String → Spec Unit
spec baseUrl = describe "Music Web App" do
  it "renders" do
    withBrowserPage (URL baseUrl) \page → do
      mbText ← fromOneOf <$> PW.textContent page (Selector "body")
      case mbText of
        Just text →
          text `shouldContain` "MUSIC"
        Nothing →
          fail "DOM is missing body."

  it "processes code hash" do
    withBrowserPage (URL $ baseUrl <> "/#PYRgBMDODGDeDuBeSBLAdgUwL5A")
      \page →
        do
          mbText ← fromOneOf <$> PW.textContent page (Selector "body")
          case mbText of
            Just text →
              text `shouldContain` "o1 osc{w=sine}"
            Nothing →
              fail "DOM is missing body."
