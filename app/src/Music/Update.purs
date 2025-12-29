module Music.Update (update) where

import Music.Model (Model)
import Music.Model.Perspective as Perspective
import Music.Update.Perspective.Code (update) as Code
import Music.Update.Perspective.Controls as Controls
import Music.Update.Perspective.Diagram as Diagram
import Music.Update.Types (Update)

update ∷ Update Model
update model = case model.perspective of
  Perspective.Code codePerspective →
    Code.update codePerspective

  Perspective.Controls controlsPerspective →
    Controls.update controlsPerspective

  Perspective.Diagram diagramPerspective →
    Diagram.update diagramPerspective
