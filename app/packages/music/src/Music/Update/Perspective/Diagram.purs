module Music.Update.Perspective.Diagram (update) where

import Prelude

import Data.Codec as Codec
import Effect.Aff.Class (class MonadAff)
import Music.Api.Message (Message(..))
import Music.Init.Perspective.Code as Code
import Music.Init.Perspective.Controls as Controls
import Music.Init.Types (Init)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.Perspective
  ( DiagramPerspective
  , DiagramState(..)
  , Perspective
  )
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Update.Types (Update)

update
  ∷ ∀ m
  . MonadAff m
  ⇒ Update m DiagramPerspective Perspective
update diagramPerspective = case _ of
  DiagramRendered renderedDiagramHtml →
    pure $ Perspective.Diagram diagramPerspective
      { state = Generated renderedDiagramHtml }
  PerspectiveChanged { audioNodes, toPerspective } →
    case toPerspective of
      Code → do
        codePerspective ← Code.init
          $ Codec.encoder AudioNodes.stringCodec unit audioNodes
        pure $ Perspective.Code codePerspective
      Controls → do
        controlsPerspective ← Controls.init audioNodes
        pure $ Perspective.Controls controlsPerspective
      _ →
        noop
  _ →
    noop
  where
  noop ∷ Init m Perspective
  noop = pure $ Perspective.Diagram diagramPerspective
