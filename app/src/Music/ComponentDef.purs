module Music.ComponentDef (def) where

import Elmish.Component (ComponentDef)
import Music.Init (init)
import Music.Message (Message)
import Music.Model (Model)
import Music.Update (update)
import Music.View (view)

def ∷ ComponentDef Message Model
def = { init, view, update }

