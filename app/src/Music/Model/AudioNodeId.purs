module Music.Model.AudioNodeId
  ( AudioNodeId
  , codec
  , output
  ) where

import Prelude

import Data.Codec (Codec)
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId

type AudioNodeId = BlockId

output ∷ AudioNodeId
output = BlockId.reserved

codec ∷ Codec AudioNodeId String Unit
codec = BlockId.codec

