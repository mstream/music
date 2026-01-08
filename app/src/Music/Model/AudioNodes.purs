module Music.Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  , OscillatorConf
  , connections
  , empty
  , fromGraph
  , nodesById
  , toGraph
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Graph (Edge, Graph)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.Traversable (all)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Gen as Gen
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.AudioNodes.Frequency (Frequency)
import Music.Model.AudioNodes.Gain (Gain)
import Music.Model.AudioNodes.Wave (Wave)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

newtype AudioNodes = AudioNodes (Graph AudioNodeId AudioNode)

empty ∷ AudioNodes
empty = AudioNodes $ Graph.fromMap Map.empty

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
    (Graph.toMap graph1)
    (Graph.toMap graph2)

instance Ord AudioNodes where
  compare (AudioNodes graph1) (AudioNodes graph2) = compare
    (Graph.toMap graph1)
    (Graph.toMap graph2)

instance Show AudioNodes where
  show (AudioNodes graph) = show $ Graph.toMap graph

data AudioNode = Oscillator OscillatorConf

derive instance Eq AudioNode
derive instance Generic AudioNode _
derive instance Ord AudioNode

instance Arbitrary AudioNode where
  arbitrary = genericArbitrary

instance Show AudioNode where
  show = genericShow

type OscillatorConf =
  { frequency ∷ Frequency, gain ∷ Gain, wave ∷ Wave }
