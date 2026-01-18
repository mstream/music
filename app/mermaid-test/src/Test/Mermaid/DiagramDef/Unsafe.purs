module Test.Mermaid.DiagramDef.Unsafe (unsafeBlockDiagramDef) where

import Prelude

import Data.Either (Either(..))
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockDef (GroupBlock)
import Partial.Unsafe (unsafeCrashWith)

unsafeBlockDiagramDef ∷ GroupBlock → DiagramDef
unsafeBlockDiagramDef = DiagramDef.Blocks <<< unsafeBlocksDef

unsafeBlocksDef ∷ GroupBlock → Blocks.Def
unsafeBlocksDef groupBlock = case Blocks.def groupBlock of
  Left errorMessage →
    unsafeCrashWith $ "invalid group block definition: " <> errorMessage
  Right def →
    def

