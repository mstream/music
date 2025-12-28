module Main (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Elmish.TimeMachine (withTimeMachine)
import Update (init, update)
import View (view)

main ∷ Effect Unit
main = Boot.defaultMain
  { elementId: "app"
  , def: withTimeMachine { init, view, update }
  }
