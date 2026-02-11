module Elmish.Components.Main (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Test.Elmish.Components.Accordion as Accordion
import Test.Elmish.Components.NavBar as NavBar

main ∷ Effect Unit
main = do
  Boot.defaultMain
    { def: Accordion.def
    , elementId: "accordion"
    }
  Boot.defaultMain
    { def: NavBar.def
    , elementId: "navbar"
    }

