module Music.Model.AudioNodes.AudioNodeId
  ( AudioNodeId
  , codec
  , oscillators
  , output
  , userDefined
  ) where

import Prelude

import Data.Codec (Codec)
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId

type AudioNodeId = BlockId

userDefined ∷ BlockId → BlockId
userDefined suffix = prefix <> suffix
  where
  prefix ∷ BlockId
  prefix = BlockId.make BlockId.U
    [ BlockId.S
    , BlockId.E
    , BlockId.R
    ]

oscillators ∷ AudioNodeId
oscillators = BlockId.make BlockId.O
  [ BlockId.S
  , BlockId.C
  , BlockId.I
  , BlockId.L
  , BlockId.L
  , BlockId.A
  , BlockId.T
  , BlockId.O
  , BlockId.R
  , BlockId.S
  ]

output ∷ AudioNodeId
output = BlockId.make BlockId.O
  [ BlockId.U
  , BlockId.T
  , BlockId.P
  , BlockId.U
  , BlockId.T
  ]

codec ∷ Codec AudioNodeId String Unit
codec = BlockId.stringCodec

