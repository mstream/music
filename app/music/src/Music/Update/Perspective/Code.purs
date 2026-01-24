module Music.Update.Perspective.Code (update) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Effect.Aff.Class (class MonadAff)
import Music.Init.Perspective.Controls as Controls
import Music.Init.Perspective.Diagram as Diagram
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.Perspective (CodePerspective)
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Update.Types (Update)

update ∷ ∀ m. MonadAff m ⇒ MonadLogger m ⇒ Update m CodePerspective
update model = case _ of
  CodeChanged code →
    pure { perspective: Perspective.Code model { code = code } }
  PerspectiveChanged { audioNodes, toPerspective } →
    case toPerspective of
      Controls → do
        controlsPerspective ← Controls.init audioNodes
        pure { perspective: Perspective.Controls controlsPerspective }
      Diagram → do
        diagramPerspective ← Diagram.init audioNodes
        pure { perspective: Perspective.Diagram diagramPerspective }
      _ →
        noop
  _ →
    noop
  where
  noop ∷ Init m Model
  noop = pure { perspective: Perspective.Code model }

