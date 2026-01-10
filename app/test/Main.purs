module Test.Main (main) where

import Prelude

import Effect (Effect)
import Music.Model.AudioNodes.AudioNodeName (AudioNodeName)
import Music.Model.Perspective.PerspectiveName (PerspectiveName)
import Test.Laws (lawsTestSuite)
import Test.Mermaid.DiagramDef as DiagramDef
import Test.Music.Model.AudioNodes (spec) as AudioNodes
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Proxy (Proxy(..))

foreign import mockBrowserImpl ∷ Effect Unit

main ∷ Effect Unit
main = do
  mockBrowserImpl
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  AudioNodes.spec
  DiagramDef.spec
  lawsTestSuite "AudioNodeName" do
    checkEq (Proxy ∷ Proxy AudioNodeName)
    checkOrd (Proxy ∷ Proxy AudioNodeName)
  lawsTestSuite "PerspectiveName" do
    checkEq (Proxy ∷ Proxy PerspectiveName)
    checkOrd (Proxy ∷ Proxy PerspectiveName)
