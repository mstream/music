module Mermaid (render) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Model.AudioNodes.Codec.Diagram (Diagram)
import Model.AudioNodes.Codec.Diagram as Diagram

render ∷ Diagram → Aff String
render = toAffE <<< renderImpl <<< Diagram.toString

foreign import renderImpl ∷ String → Effect (Promise String)

