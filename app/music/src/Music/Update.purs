module Music.Update (update) where

import Control.Monad.Logger.Class (class MonadLogger)
import Effect.Aff.Class (class MonadAff)
import Music.Model (Model)
import Music.Model.Perspective as Perspective
import Music.Update.Perspective.Code (update) as Code
import Music.Update.Perspective.Controls as Controls
import Music.Update.Perspective.Diagram as Diagram
import Music.Update.Types (Update)

update ∷ ∀ m. MonadAff m ⇒ MonadLogger m ⇒ Update m Model
update model = case model.perspective of
  Perspective.Code codePerspective →
    Code.update codePerspective

  Perspective.Controls controlsPerspective →
    Controls.update controlsPerspective

  Perspective.Diagram diagramPerspective →
    Diagram.update diagramPerspective
