module Update (init, update) where

import Prelude

import Elmish (Transition)
import Message (Message(..))
import Model (Model)
import Model.Perspective (Perspective(..))
import Update.Perspective.Controls as Controls
import Update.Types (Update)

exampleCode ∷ String
exampleCode = "osc1 osc{f=200.0,g=0.5,w=sine}"

init ∷ Transition Message Model
init = pure { perspective: Code exampleCode }

update ∷ Update Model
update model = case model.perspective of
  Code s →
    updateCodePerspective s

  Controls controlsModel →
    Controls.update controlsModel

  Diagram renderedDiagramHtml →
    updateDiagramPerspective renderedDiagramHtml

updateCodePerspective ∷ Update String
updateCodePerspective model = case _ of
  PerspectiveChanged perspective →
    pure { perspective: perspective }
  _ →
    pure { perspective: Code model }

updateDiagramPerspective ∷ Update String
updateDiagramPerspective model = case _ of
  PerspectiveChanged perspective →
    pure { perspective: perspective }
  _ →
    pure { perspective: Diagram model }

