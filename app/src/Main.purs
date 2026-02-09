module Main (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Music.ComponentDef as ComponentDef
import Utils (getConf)

main ∷ Effect Unit
main = do
  conf ← getConf false
  Boot.defaultMain
    { elementId: "app"
    , def: ComponentDef.def conf
    }
