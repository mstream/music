module Test.Main (main) where

import Prelude

import Effect (Effect)
import Playwright (URL(..))
import Test.AppSpec as AppSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = do
  runSpecAndExitProcess
    [ consoleReporter ]
    (AppSpec.spec $ URL "https://mstream.github.io/music/")
