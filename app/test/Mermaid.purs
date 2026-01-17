module Test.Mermaid (spec) where

import Prelude

import Test.Mermaid.DiagramDef as DiagramDef
import Test.Spec (Spec)

spec ∷ Spec Unit
spec = do
  DiagramDef.spec
