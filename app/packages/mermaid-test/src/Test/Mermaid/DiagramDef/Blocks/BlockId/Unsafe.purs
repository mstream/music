module Test.Mermaid.DiagramDef.Blocks.BlockId.Unsafe (unsafeBlockId) where

import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Test.Data.Codec (unsafeDecoded)

unsafeBlockId ∷ String → BlockId
unsafeBlockId = unsafeDecoded BlockId.stringCodec

