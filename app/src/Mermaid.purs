module Mermaid (render) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Codec as Codec
import Data.Foldable (any)
import Data.Graph as Graph
import Data.List (List)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef as DiagramDef
import Mermaid.DiagramDef.Blocks as Blocks
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)

render ∷ DiagramDef → Aff String
render diagramDef =
  if containsEmpty diagramDef then
    pure rendered
  else
    toAffE (renderImpl rendered)
  where
  rendered ∷ String
  rendered = Codec.encoder DiagramDef.stringCodec true diagramDef

containsEmpty ∷ DiagramDef → Boolean
containsEmpty (DiagramDef.Blocks def) =
  hasEmptyGroup (Blocks.toGroupBlock def)

hasEmptyGroup ∷ GroupBlock → Boolean
hasEmptyGroup groupBlock =
  Map.isEmpty childrenMap ||
    any isEmptyChild children
  where
  childrenMap = Graph.toMap groupBlock.children

  children
    ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
  children = Map.toUnfoldable childrenMap

  isEmptyChild
    ∷ BlockId
        /\ (BlockDef /\ List BlockId)
    → Boolean
  isEmptyChild (_ /\ (blockDef /\ _)) = case blockDef of
    Group nested →
      hasEmptyGroup nested
    Node _ →
      false

foreign import renderImpl ∷ String → Effect (Promise String)
