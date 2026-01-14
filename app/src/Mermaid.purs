module Mermaid (render) where

import Prelude

import Control.Monad.State (State, evalState, get, put)
import Control.Promise (Promise, toAffE)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Graph as Graph
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
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
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Parsing (runParser)

render ∷ DiagramDef → Aff String
render diagramDef =
  let
    sanitized ∷ DiagramDef
    sanitized = sanitize diagramDef

    rendered ∷ String
    rendered = Codec.encoder DiagramDef.stringCodec true sanitized
  in
    if containsEmpty sanitized then
      pure rendered
    else
      toAffE (renderImpl rendered)

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

sanitize ∷ DiagramDef → DiagramDef
sanitize (DiagramDef.Blocks def) =
  DiagramDef.Blocks $ fromMaybe def $ case Blocks.def sanitized of
    Right def' →
      Just def'
    Left _ →
      Nothing
  where
  sanitized ∷ GroupBlock
  sanitized = sanitizeGroupBlock (Blocks.toGroupBlock def)

sanitizeGroupBlock ∷ GroupBlock → GroupBlock
sanitizeGroupBlock groupBlock = evalState (renameGroup groupBlock)
  initial
  where
  initial ∷ { mapping ∷ Map BlockId BlockId, next ∷ Int }
  initial = { mapping: Map.empty, next: 0 }

  renameGroup
    ∷ GroupBlock
    → State { mapping ∷ Map BlockId BlockId, next ∷ Int } GroupBlock
  renameGroup current = do
    let
      children ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
      children = Map.toUnfoldable (Graph.toMap current.children)
    renamed ← traverse renameChild children
    pure
      { children: Graph.fromMap (Map.fromFoldable renamed)
      , properties: current.properties
      , spacedOut: current.spacedOut
      }

  renameChild
    ∷ BlockId /\ (BlockDef /\ List BlockId)
    → State { mapping ∷ Map BlockId BlockId, next ∷ Int }
        (BlockId /\ (BlockDef /\ List BlockId))
  renameChild (oldId /\ (blockDef /\ ends)) = do
    newId ← getOrCreate oldId
    newEnds ← traverse getOrCreate ends
    newBlockDef ← case blockDef of
      Group nested →
        Group <$> renameGroup nested
      Node label →
        pure $ Node label
    pure $ newId /\ (newBlockDef /\ newEnds)

  getOrCreate
    ∷ BlockId
    → State { mapping ∷ Map BlockId BlockId, next ∷ Int } BlockId
  getOrCreate oldId = do
    state ← get
    case Map.lookup oldId state.mapping of
      Just mapped →
        pure mapped
      Nothing → do
        let
          newId ∷ BlockId
          newId = makeSafeId state.next oldId
        put
          state
            { mapping = Map.insert oldId newId state.mapping
            , next = state.next + 1
            }
        pure newId

  makeSafeId ∷ Int → BlockId → BlockId
  makeSafeId counter fallback =
    case
      runParser ("id" <> show counter)
        (Codec.decoder BlockId.stringCodec)
      of
      Right blockId →
        blockId
      Left _ →
        fallback

foreign import renderImpl ∷ String → Effect (Promise String)
