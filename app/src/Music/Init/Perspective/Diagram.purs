module Music.Init.Perspective.Diagram (init) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Elmish as E
import Mermaid as Mermaid
import Mermaid.DiagramDef (DiagramDef(..))
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockDef (GroupBlock)
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes.Codec.Diagram as Diagram
import Music.Model.Perspective (DiagramPerspective, DiagramState(..))

init ∷ AudioNodes → Init DiagramPerspective
init audioNodes = case Blocks.def groupBlock of
  Left errorMessage →
    pure { audioNodes, state: Invalid errorMessage }
  Right blocksDef → do
    let
      diagramDef ∷ DiagramDef
      diagramDef = Blocks blocksDef
    E.fork $ DiagramRendered <$> Mermaid.render diagramDef
    pure { audioNodes, state: Generating diagramDef }
  where
  groupBlock ∷ GroupBlock
  groupBlock = Codec.encoder Diagram.groupBlockCodec unit audioNodes

