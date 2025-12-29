module Music.Init.Perspective.Diagram (init) where

import Prelude

import Data.Codec as Codec
import Elmish as E
import Mermaid as Mermaid
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes.Codec.Diagram (Diagram)
import Music.Model.AudioNodes.Codec.Diagram as Diagram
import Music.Model.Perspective (DiagramPerspective, DiagramState(..))

init ∷ AudioNodes → Init DiagramPerspective
init audioNodes = do
  E.fork $ DiagramRendered <$> Mermaid.render diagram
  pure { audioNodes, state: Generating diagram }
  where
  diagram ∷ Diagram
  diagram = Codec.encoder Diagram.codec unit audioNodes

