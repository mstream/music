module MainDebug (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Elmish.TimeMachine (withTimeMachine)
import Music.ComponentDef as ComponentDef

main ∷ Effect Unit
main = Boot.defaultMain
  { elementId: "app"
  , def: withTimeMachine $ ComponentDef.def true
  }

