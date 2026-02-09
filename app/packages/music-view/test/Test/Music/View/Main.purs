module Test.Music.View.Main (main) where

import Prelude

import Effect (Effect)
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  pure unit
