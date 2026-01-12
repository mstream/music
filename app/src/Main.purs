module Main (main) where

import Prelude

import Effect (Effect)
import Elmish.Boot as Boot
import Music.ComponentDef (def)

main
  ∷ Effect Unit
main =
  Boot.defaultMain { elementId: "app", def }
