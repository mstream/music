module Music.Init.Perspective.Diagram (init) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Logger.Class as Logger
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Map as Map
import Effect.Aff.Class (class MonadAff, liftAff)
import Elmish as E
import Mermaid as Mermaid
import Mermaid.DiagramDef (DiagramDef(..))
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockDef (GroupBlock)
import Music.Api.Message (Message(..))
import Music.Init.Types (Init)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.Perspective (DiagramPerspective, DiagramState(..))

init
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadLogger m
  ⇒ AudioNodes
  → Init m DiagramPerspective
init audioNodes = case Blocks.def groupBlock of
  Left errorMessage →
    pure { audioNodes, state: Invalid errorMessage }
  Right blocksDef → do
    let
      diagramDef ∷ DiagramDef
      diagramDef = Blocks blocksDef
    E.fork $ do
      { diagramCode, diagramHtml } ← liftAff $ Mermaid.render diagramDef
      Logger.debug Map.empty ("Diagram code:\n" <> diagramCode)
      pure $ DiagramRendered diagramHtml
    pure { audioNodes, state: Generating diagramDef }
  where
  groupBlock ∷ GroupBlock
  groupBlock = Codec.encoder AudioNodes.groupBlockCodec unit audioNodes

