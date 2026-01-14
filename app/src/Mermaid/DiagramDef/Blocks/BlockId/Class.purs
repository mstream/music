module Mermaid.DiagramDef.Blocks.BlockId.Class
  ( class CharLike
  , fromChar
  , toChar
  ) where

import Data.Maybe (Maybe)

class CharLike a where
  fromChar ∷ Char → Maybe a
  toChar ∷ a → Char

