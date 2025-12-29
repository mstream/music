module Music.Init (init) where

import Prelude

import Data.Map as Map
import Elmish as E
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.Perspective as Perspective

exampleCode ∷ String
exampleCode = "osc1 osc{f=200.0,g=0.5,w=sine}"

init ∷ Init Model
init = do
  codePerspective ← Code.init Map.empty
  E.fork $ pure $ CodeChanged exampleCode
  pure { perspective: Perspective.Code codePerspective }
