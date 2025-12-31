module Music.Model.AudioNodeId
  ( AudioNodeId
  , codec
  ) where

import Prelude

import Data.Codec (Codec)
import Mermaid.DiagramDef.BlockDiagram.BlockId (BlockId)
import Mermaid.DiagramDef.BlockDiagram.BlockId as BlockId

type AudioNodeId = BlockId

codec ∷ Codec AudioNodeId String Unit
codec = BlockId.codec

