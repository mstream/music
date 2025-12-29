module Music.Model.PerspectiveName (PerspectiveName(..)) where

import Prelude

data PerspectiveName = Code | Controls | Diagram

derive instance Eq PerspectiveName

instance Ord PerspectiveName where
  compare Code _ = LT
  compare Diagram other = case other of
    Code →
      GT
    Diagram →
      EQ
    Controls →
      LT
  compare Controls _ = GT

instance Show PerspectiveName where
  show = case _ of
    Code →
      "Code"
    Controls →
      "Controls"
    Diagram →
      "Diagram"

