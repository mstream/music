module Elmish.Components.Main (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Test.Elmish.Components.Accordion as Accordion

main ∷ Effect Unit
main = do
  Boot.defaultMain
    { def: Accordion.def
    , elementId: "accordion"
    }

