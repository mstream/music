module Mermaid (render) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Codec as Codec
import Effect (Effect)
import Effect.Aff (Aff)
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef

render ∷ DiagramDef → Aff { diagramCode ∷ String, diagramHtml ∷ String }
render diagramDef = do
  let
    diagramCode ∷ String
    diagramCode = Codec.encoder DiagramDef.stringCodec true diagramDef

  diagramHtml ← toAffE (renderImpl diagramCode)

  pure { diagramCode, diagramHtml }

foreign import renderImpl ∷ String → Effect (Promise String)
