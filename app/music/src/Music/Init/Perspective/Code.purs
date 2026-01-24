module Music.Init.Perspective.Code (init) where

import Prelude

import Music.Init.Types (Init)
import Music.Model.Perspective (CodePerspective)

init ∷ ∀ m. String → Init m CodePerspective
init code = pure { code }

