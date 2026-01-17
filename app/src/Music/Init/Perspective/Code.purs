module Music.Init.Perspective.Code (init) where

import Prelude

import Data.Codec as Codec
import Music.Init.Types (Init)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.Perspective (CodePerspective)

init ∷ AudioNodes → Init CodePerspective
init audioNodes = pure { code }
  where
  code ∷ String
  code = Codec.encoder AudioNodes.stringCodec unit audioNodes

