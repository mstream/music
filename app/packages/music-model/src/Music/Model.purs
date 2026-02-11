module Music.Model (Model(..)) where

import Music.Model.Perspective (Perspective)

type Model =
  { isClipboardAvailable ∷ Boolean
  , perspective ∷ Perspective
  }
