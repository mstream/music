module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Mermaid (spec) as Mermaid
import Test.Music as Music
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

foreign import mockBrowserImpl ∷ Effect Unit

main ∷ Effect Unit
main = do
  mockBrowserImpl
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  Mermaid.spec
  Music.spec
