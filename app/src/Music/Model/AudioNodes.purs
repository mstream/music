module Music.Model.AudioNodes
  ( AudioNodes
  , connections
  , empty
  , fromGraph
  , nodesById
  , toGraph
  , updateAudioNode
  ) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Edge, Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (all)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Gen as Gen
import Music.Model.AudioNodes.AudioNode (AudioNode)
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype AudioNodes = AudioNodes (Graph AudioNodeId AudioNode)

empty ∷ AudioNodes
empty = AudioNodes $ Graph.fromMap Map.empty

updateAudioNode
  ∷ AudioNodes → AudioNodeId → AudioNode → String \/ AudioNodes
updateAudioNode (AudioNodes graph) nodeId node =
  if nodeId `Map.member` nodes then fromGraph $ Graph.fromMap $
    Map.insertWith
      (\(_ /\ connectionEnds) _ → node /\ connectionEnds)
      nodeId
      (node /\ Nil)
      nodes
  else Left $ "no such node: "
    <> Codec.encoder AudioNodeId.codec unit nodeId
  where
  nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  nodes = Graph.toMap graph

fromGraph ∷ Graph AudioNodeId AudioNode → String \/ AudioNodes
fromGraph graph =
  if allConnectionsAreValid then Right $ AudioNodes graph
  else Left "invalid connections"
  where
  allConnectionsAreValid ∷ Boolean
  allConnectionsAreValid = foldlWithIndex
    areNodeConnectionsValid
    true
    nodes

  areNodeConnectionsValid
    ∷ AudioNodeId → Boolean → AudioNode /\ List AudioNodeId → Boolean
  areNodeConnectionsValid nodeId acc (_ /\ connectionEnds) =
    acc && isNotConnectedWithItself && isConnectedWithExistingNodes
    where
    isNotConnectedWithItself ∷ Boolean
    isNotConnectedWithItself = all (not <<< eq nodeId) connectionEnds

    isConnectedWithExistingNodes ∷ Boolean
    isConnectedWithExistingNodes = all
      ( \connectionEnd → connectionEnd == AudioNodeId.output ||
          Map.member connectionEnd nodes
      )
      connectionEnds

  nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  nodes = Graph.toMap graph

toGraph ∷ AudioNodes → (Graph AudioNodeId AudioNode)
toGraph (AudioNodes graph) = graph

connections ∷ AudioNodes → List (Edge AudioNodeId)
connections (AudioNodes graph) = Graph.edges graph

nodesById ∷ AudioNodes → Map AudioNodeId AudioNode
nodesById (AudioNodes graph) = Tuple.fst <$> Graph.toMap graph

instance Arbitrary AudioNodes where
  arbitrary = do
    nodes ← Gen.arbitraryMap
    pure $ AudioNodes
      $ Graph.fromMap (map withNoConnections nodes)
    where
    withNoConnections
      ∷ AudioNode
      → (AudioNode /\ List AudioNodeId)
    withNoConnections node = node /\ Nil

instance Eq AudioNodes where
  eq (AudioNodes graph1) (AudioNodes graph2) = eq
    (normalizeGraph graph1)
    (normalizeGraph graph2)

instance Ord AudioNodes where
  compare (AudioNodes graph1) (AudioNodes graph2) = compare
    (normalizeGraph graph1)
    (normalizeGraph graph2)

instance Show AudioNodes where
  show (AudioNodes graph) = show $ Graph.toMap graph

normalizeGraph
  ∷ Graph AudioNodeId AudioNode
  → Map AudioNodeId (AudioNode /\ List AudioNodeId)
normalizeGraph =
  map normalizeEntry <<< Graph.toMap
  where
  normalizeEntry
    ∷ AudioNode /\ List AudioNodeId
    → AudioNode /\ List AudioNodeId
  normalizeEntry (node /\ ends) =
    node /\ List.filter (not <<< eq AudioNodeId.output) ends
