module Test.Music.Model.Main (main) where

import Prelude

import Effect (Effect)
import Test.Music.Model.AudioNodes as AudioNodes
import Test.Music.Model.Perspective as Perspective
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  AudioNodes.spec
  Perspective.spec
