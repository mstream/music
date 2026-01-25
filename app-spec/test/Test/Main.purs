module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Playwright (Selector(..), URL(..))
import Playwright as PW
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Utils (withBrowserPage)
import Untagged.Union (fromOneOf)

main ∷ Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  describe "TODO" do
    it "TODO" do
      withBrowserPage (URL "https://mstream.github.io/music/") \page →
        do
          mbText ← fromOneOf <$> PW.textContent page (Selector "body")
          case mbText of
            Just text →
              text `shouldContain` "MUSIC"
            Nothing →
              fail "DOM is missing body."

