module MainDebug (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Elmish.TimeMachine (withTimeMachine)
import Music.ComponentDef as ComponentDef
import Utils (getConf)

main ∷ Effect Unit
main = do
  conf ← getConf true
  Boot.defaultMain
    { elementId: "app"
    , def: withTimeMachine $ ComponentDef.def conf
    }
