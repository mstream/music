module Mermaid (render) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Codec as Codec
import Effect (Effect)
import Effect.Aff (Aff)
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef

render ∷ DiagramDef → Aff String
render = toAffE <<< renderImpl <<< Codec.encoder DiagramDef.codec unit

foreign import renderImpl ∷ String → Effect (Promise String)

