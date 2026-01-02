module Music.Model.AudioNodeId
  ( AudioNodeId
  , codec
  ) where

import Prelude

import Data.Codec (Codec)
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId

type AudioNodeId = BlockId

codec ∷ Codec AudioNodeId String Unit
codec = BlockId.codec

