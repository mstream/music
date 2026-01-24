module Music.Model.AudioNodes.AudioNodeId
  ( AudioNodeId
  , dummy
  , duration
  , frequency
  , gain
  , inputs
  , oscillators
  , output
  , parameters
  , sequence
  , sequencers
  , userDefined
  , wave
  ) where

import Prelude

import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..))

type AudioNodeId = BlockId

userDefined ∷ AudioNodeId → AudioNodeId
userDefined id = prefix <> id
  where
  prefix ∷ AudioNodeId
  prefix = make D [ E, F ]

duration ∷ AudioNodeId
duration = make D [ U, R, A, T, I, O, N ]

dummy ∷ AudioNodeId
dummy = make D [ U, M, M, Y ]

frequency ∷ AudioNodeId
frequency = make F
  [ R, E, Q, U, E, N, C, Y ]

gain ∷ AudioNodeId
gain = make G [ A, I, N ]

inputs ∷ AudioNodeId
inputs = make I [ N, P, U, T, S ]

oscillators ∷ AudioNodeId
oscillators = make O [ S, C, I, L, L, A, T, O, R, S ]

output ∷ AudioNodeId
output = make O [ U, T, P, U, T ]

parameters ∷ AudioNodeId
parameters = make P [ A, R, A, M, E, T, E, R, S ]

sequence ∷ AudioNodeId
sequence = make S [ E, Q, U, E, N, C, E ]

sequencers ∷ AudioNodeId
sequencers = make S [ E, Q, U, E, N, C, E, R, S ]

wave ∷ AudioNodeId
wave = make W [ A, V, E ]

make ∷ AlphaChar → Array AlphaChar → AudioNodeId
make firstChar otherChars = BlockId.make firstChar otherChars []
