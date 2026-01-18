module Music.Model.AudioNodes.AudioNode.Code.Documentable
  ( class Documentable
  , Documentation
  , documentation
  ) where

import Data.Set (Set)

class Documentable ∷ ∀ k. k → Type → Constraint
class Documentable a i | a → i where
  documentation ∷ Documentation i

type Documentation a =
  { description ∷ String
  , examples ∷ Set a
  }

