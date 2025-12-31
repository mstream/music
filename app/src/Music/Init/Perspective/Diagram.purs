module Music.Init.Perspective.Diagram (init) where

import Prelude

import Data.Codec as Codec
import Elmish as E
import Mermaid as Mermaid
import Mermaid.DiagramDef (DiagramDef)
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes.Codec.Diagram as Diagram
import Music.Model.Perspective (DiagramPerspective, DiagramState(..))

init ∷ AudioNodes → Init DiagramPerspective
init audioNodes = do
  E.fork $ DiagramRendered <$> Mermaid.render diagramDef
  pure { audioNodes, state: Generating diagramDef }
  where
  diagramDef ∷ DiagramDef
  diagramDef = Codec.encoder Diagram.codec unit audioNodes

