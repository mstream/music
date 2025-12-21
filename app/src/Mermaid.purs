module Mermaid (render) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

render ∷ String → Aff String
render = toAffE <<< renderImpl

foreign import renderImpl ∷ String → Effect (Promise String)

