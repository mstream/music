module Music.View.Diagram (view) where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Music.Model.Perspective (DiagramPerspective, DiagramState(..))
import Music.View.Types (ViewModelPure)

view ∷ ViewModelPure DiagramPerspective
view model = case model.state of
  Generating _ →
    H.text ""
  Generated renderedDiagramHtml →
    H.div_
      ""
      { dangerouslySetInnerHTML: { __html: renderedDiagramHtml } }
      ([] ∷ Array ReactElement)
  Invalid errorMessage →
    H.text $ "Error: " <> errorMessage
