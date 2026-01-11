module Music.Update.Perspective.Diagram (update) where

import Prelude

import Music.Init.Perspective.Code as Code
import Music.Init.Perspective.Controls as Controls
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.Perspective (DiagramPerspective, DiagramState(..))
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Update.Types (Update)

update ∷ Update DiagramPerspective
update model = case _ of
  DiagramRendered renderedDiagramHtml →
    pure
      { perspective: Perspective.Diagram model
          { state = Generated renderedDiagramHtml }
      }
  PerspectiveChanged { audioNodes, toPerspective } →
    case toPerspective of
      Code → do
        codePerspective ← Code.init audioNodes
        pure { perspective: Perspective.Code codePerspective }
      Controls → do
        controlsPerspective ← Controls.init audioNodes
        pure { perspective: Perspective.Controls controlsPerspective }
      _ →
        noop
  _ →
    noop
  where
  noop ∷ Init Model
  noop = pure { perspective: Perspective.Diagram model }
