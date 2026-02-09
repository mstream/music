module Music.Update (update) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Music.App (Env)
import Music.Init.Types (Init)
import Music.Message (Message)
import Music.Model (Model)
import Music.Model.Perspective (Perspective(..))
import Music.Update.Perspective.Code (update) as Code
import Music.Update.Perspective.Controls as Controls
import Music.Update.Perspective.Diagram as Diagram
import Music.Update.Types (Update)

update
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadAsk Env m
  ⇒ MonadLogger m
  ⇒ Update m Model Model
update model message = do
  perspective ← updatePerspective message
  pure $ model { perspective = perspective }

  where
  updatePerspective ∷ Message → Init m Perspective
  updatePerspective = case model.perspective of
    Code codePerspective →
      Code.update codePerspective

    Controls controlsPerspective →
      Controls.update controlsPerspective

    Diagram diagramPerspective →
      Diagram.update diagramPerspective
