module View.Diagram (view) where

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import View.Types (ViewModelPure)

view ∷ ViewModelPure String
view renderedDiagramHtml = H.div_
  ""
  { dangerouslySetInnerHTML: { __html: renderedDiagramHtml } }
  ([] ∷ Array ReactElement)
