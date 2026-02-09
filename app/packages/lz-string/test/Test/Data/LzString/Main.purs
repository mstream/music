module Test.Data.LzString.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.LzString (decode, encode)
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.QuickCheck (Result, quickCheck, (===), (>?))
import Test.Spec (Spec, describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main ∷ Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = describe "LzString" do
  it "roundtrips - random"
    (liftEffect $ quickCheck prop)
  where
  prop ∷ String → Result
  prop text = case encode text of
    Left _ →
      String.length text >? 1024
    Right encoded →
      decode encoded === Right text
