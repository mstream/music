module Music.Model.AudioNodes
  ( AudioNodeEntry
  , AudioNodes
  , Entry
  , Violation(..)
  , connectedFrequencySequencer
  , connectedGainSequencer
  , connections
  , groupBlockCodec
  , make
  , nodesById
  , stringCodec
  , toGraph
  , updateAudioNode
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Graph (Edge, Graph)
import Data.Graph as Graph
import Data.Graph.NonEmpty (NonEmptyGraph)
import Data.Graph.NonEmpty as GraphNE
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (all, traverse)
import Data.Tuple (uncurry)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..))
import Music.Model.AudioNodes.AudioNode
  ( AudioNode(..)
  , frequencyBlockId
  , gainBlockId
  )
import Music.Model.AudioNodes.AudioNode as AudioNode
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId, fromBlockId, toBlockId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Parsing (ParseState(..), Parser, fail, getParserT, runParser) as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

type AudioNodeEntry =
  { audioNode ∷ AudioNode, isConnectedToOutput ∷ Boolean }

newtype AudioNodes = AudioNodes
  (NonEmptyGraph AudioNodeId AudioNodeEntry)

derive newtype instance Eq AudioNodes
derive newtype instance Ord AudioNodes
derive newtype instance Show AudioNodes

instance Arbitrary AudioNodes where
  arbitrary ∷ Gen AudioNodes
  arbitrary = do
    graph ← arbitrary ∷ Gen (NonEmptyGraph AudioNodeId AudioNodeEntry)
    let
      allEntries = Map.toUnfoldable (GraphNE.toMap graph)

      allIds = Set.fromFoldable $ Tuple.fst <$> allEntries

      fix (id /\ (node /\ edges)) =
        id /\
          ( node /\
              ( Set.filter
                  ( \e → e /= id &&
                      (Set.member e allIds)
                  )
                  edges
              )
          )

      fixedEntries = fix <$> allEntries

      sortedEntries = Array.sortWith Tuple.fst fixedEntries

    case Array.uncons sortedEntries of
      Just { head, tail } →
        case make head tail of
          Right ans → pure ans
          Left _ → arbitrary
      Nothing → arbitrary

type Violations = Map AudioNodeId (Set Violation)

data Violation
  = ConnectedWithItself
  | ConnectedWithNonExistingNode AudioNodeId
  | DuplicatedEntry
  | NoSuchNode

derive instance Eq Violation
derive instance Generic Violation _

derive instance Ord Violation

instance Show Violation where
  show = genericShow

updateAudioNode
  ∷ AudioNodes
  → AudioNodeId
  → AudioNodeEntry
  → Violations \/ (AudioNodes /\ Set AudioNodeId)
updateAudioNode (AudioNodes graph) nodeId newNode =
  case Map.pop nodeId nodes of
    Just ((_ /\ connectionEnds) /\ otherNodes) →
      let
        firstEntry ∷ Entry
        firstEntry = nodeId /\ (newNode /\ connectionEnds)

        otherEntries ∷ Array Entry
        otherEntries = foldMapWithIndex
          (\k v → [ k /\ v ])
          otherNodes
      in
        (_ /\ connectionEnds) <$> make firstEntry otherEntries
    Nothing →
      Left $ Map.singleton nodeId (Set.singleton NoSuchNode)
  where
  nodes ∷ Map AudioNodeId (AudioNodeEntry /\ Set AudioNodeId)
  nodes = GraphNE.toMap graph

type Entry = AudioNodeId /\ (AudioNodeEntry /\ Set AudioNodeId)

make ∷ Entry → Array Entry → Violations \/ AudioNodes
make firstEntry otherEntries =
  if Map.isEmpty duplicationViolations then do
    let
      nodes ∷ Map AudioNodeId (AudioNodeEntry /\ Set AudioNodeId)
      nodes = Map.fromFoldable allEntries

      violationsById ∷ Violations
      violationsById = findViolations nodes

    if Map.isEmpty violationsById then
      let
        sortedEntries = Array.sortBy (compare `on` Tuple.fst) allEntries
      in
        case Array.uncons sortedEntries of
          Just { head, tail } →
            Right $ AudioNodes $
              GraphNE.make (Tuple.fst head) (Tuple.snd head)
                (Graph.fromMap $ Map.fromFoldable tail)
          Nothing →
            Left Map.empty -- Should not happen
    else Left violationsById
  else Left duplicationViolations

  where
  on ∷ ∀ a b c. (b → b → c) → (a → b) → a → a → c
  on f g x y = f (g x) (g y)

  duplicationViolations ∷ Violations
  duplicationViolations = Map.fromFoldable
    $ (\id → id /\ Set.singleton DuplicatedEntry) `Set.map` idDuplicates

  idDuplicates ∷ Set AudioNodeId
  idDuplicates = Map.keys $ Map.filter (_ > 1) idFrequencies

  idFrequencies ∷ Map AudioNodeId Int
  idFrequencies = foldl
    (\acc id → Map.insertWith (+) id 1 acc)
    Map.empty
    (Tuple.fst <$> allEntries)

  allEntries ∷ Array Entry
  allEntries = [ firstEntry ] <> otherEntries

findViolations
  ∷ Map AudioNodeId (AudioNodeEntry /\ Set AudioNodeId) → Violations
findViolations nodes = Map.filter
  (not <<< eq Set.empty)
  (foldlWithIndex f Map.empty nodes)
  where
  f
    ∷ AudioNodeId
    → Violations
    → AudioNodeEntry /\ Set AudioNodeId
    → Violations
  f nodeId acc (_ /\ connectionEnds) =
    Map.insert nodeId nodeViolations acc
    where
    nodeViolations ∷ Set Violation
    nodeViolations =
      ( if isNotConnectedWithItself then Set.empty
        else Set.singleton ConnectedWithItself
      )
        <>
          ( ConnectedWithNonExistingNode `Set.map` 
              nonExistingNodesReferences
          )

    isNotConnectedWithItself ∷ Boolean
    isNotConnectedWithItself = all (not <<< eq nodeId) connectionEnds

    nonExistingNodesReferences ∷ Set AudioNodeId
    nonExistingNodesReferences = Set.fromFoldable $ Set.filter
      ( \connectionEnd → not $ Map.member connectionEnd nodes
      )
      connectionEnds

toGraph ∷ AudioNodes → (Graph AudioNodeId AudioNodeEntry)
toGraph (AudioNodes graph) = GraphNE.toGraph graph

connections ∷ AudioNodes → Set (Edge AudioNodeId)
connections (AudioNodes graph) = GraphNE.edges graph

nodesById ∷ AudioNodes → Map AudioNodeId AudioNodeEntry
nodesById (AudioNodes graph) = Tuple.fst <$> GraphNE.toMap graph

connectedFrequencySequencer
  ∷ AudioNodeId → AudioNodes → Maybe (Sequencer Frequency)
connectedFrequencySequencer nodeId =
  foldl f Nothing <<< Graph.toMap <<< toGraph
  where
  f
    ∷ Maybe (Sequencer Frequency)
    → (AudioNodeEntry /\ Set AudioNodeId)
    → Maybe (Sequencer Frequency)
  f acc ({ audioNode } /\ connectionEnds) = case audioNode of
    FrequencySequencer conf →
      if Set.member nodeId connectionEnds then Just conf else acc
    _ →
      acc

connectedGainSequencer
  ∷ AudioNodeId → AudioNodes → Maybe (Sequencer Gain)
connectedGainSequencer nodeId =
  foldl f Nothing <<< Graph.toMap <<< toGraph
  where
  f
    ∷ Maybe (Sequencer Gain)
    → (AudioNodeEntry /\ Set AudioNodeId)
    → Maybe (Sequencer Gain)
  f acc ({ audioNode } /\ connectionEnds) = case audioNode of
    GainSequencer conf →
      if Set.member nodeId connectionEnds then Just conf else acc
    _ →
      acc

stringCodec ∷ Codec AudioNodes String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder AudioNodes String
stringDecoder = do
  lines ← PC.sepBy parseLine (PS.char '\n')
  let
    nodes = lines
      # List.mapMaybe case _ of
          LNode id node → Just (id /\ (node /\ Set.empty))
          _ → Nothing
      # Map.fromFoldable

    edges = lines # foldl
      ( \acc item → case item of
          LConn from to → Map.insertWith append from (Set.singleton to)
            acc
          _ → acc
      )
      Map.empty

    combine id (node /\ _) =
      let
        allEdges = fromMaybe Set.empty (Map.lookup id edges)
        isConnectedToOutput = Set.member "output" (renderId `Set.map` allEdges)
        edgesWithoutOutput = Set.filter (\e → renderId e /= "output") allEdges
      in
        id /\
          ( { audioNode: node, isConnectedToOutput } /\ edgesWithoutOutput
          )

    entries = Map.toUnfoldable nodes <#> uncurry combine

  case Array.uncons entries of
    Just { head, tail } →
      case make head tail of
        Left err → P.fail $ show err
        Right ans → pure ans
    Nothing → P.fail "AudioNodes string decoder: no nodes found"

data Line = LNode AudioNodeId AudioNode | LConn AudioNodeId AudioNodeId

parseLine ∷ P.Parser String Line
parseLine = PC.try parseConn <|> parseNode
  where
  nodeIdParser = fromBlockId <$> Codec.decoder BlockId.stringCodec

  parseConn = do
    from ← nodeIdParser
    _ ← PS.string "->"
    to ← nodeIdParser
    pure (LConn from to)

  parseNode = do
    id ← nodeIdParser
    _ ← PS.char ' '
    node ← Codec.decoder AudioNode.stringCodec
    pure (LNode id node)

stringEncoder ∷ Encoder AudioNodes String Unit
stringEncoder _ (AudioNodes graph) = String.joinWith "\n"
  (nodeLines <> connectionLines)
  where
  nodeLines = renderNodeLine <$> sortedEntries

  renderNodeLine (nodeId /\ ({ audioNode } /\ _)) =
    renderId nodeId <> " " <> Codec.encoder AudioNode.stringCodec unit audioNode

  connectionLines = renderedConnections

  renderedConnections = 
    sortedEntries >>= \(nodeId /\ ({ isConnectedToOutput } /\ ends)) →
      (renderConnection nodeId <$> Array.fromFoldable ends)
        <>
          ( if isConnectedToOutput then
              [ renderId nodeId <> "->output" ]
            else []
          )

  renderConnection from to =
    renderId from <> "->" <> renderId to

  sortedEntries = Array.sortWith (Tuple.fst >>> renderId)
    (Map.toUnfoldable $ GraphNE.toMap graph)

renderId ∷ AudioNodeId → String
renderId = Codec.encoder AudioNodeId.stringCodec unit

groupBlockCodec ∷ Codec AudioNodes GroupBlock Unit
groupBlockCodec = Codec.codec groupBlockDecoder groupBlockEncoder

groupBlockDecoder ∷ Decoder AudioNodes GroupBlock
groupBlockDecoder = do
  P.ParseState input _ _ ← P.getParserT
  case decodeGroupBlock input of
    Left err → P.fail err
    Right ans → pure ans
  where
  decodeGroupBlock gb = do
    let
      childrenMap = GraphNE.toMap gb.children

      lookupGroup key = case Map.lookup key childrenMap of
        Just (Group g /\ _) → Right g
        _ → Right
          { children: GraphNE.singleton (BlockId.make P [ L, A, C, E, H, O, L, D, E, R ] [])
              (Node "empty" /\ Set.empty)
          , properties: { columns: Nothing }
          , spacedOut: false
          }

      decodeNode (bid /\ (def /\ edges)) = case def of
        Group g → do
          node ←
            case P.runParser g (Codec.decoder AudioNode.groupBlockCodec) of
              Left err → Left $ show err
              Right n → Right n
          let id = fromBlockId (baseId bid)
          let isConnectedToOutput = Set.member outputBlockId edges
          let edges' = (fromBlockId <<< baseId) `Set.map` (Set.filter (_ /= outputBlockId) edges)
          pure (id /\ ({ audioNode: node, isConnectedToOutput } /\ edges'))
        _ → Left $ "Invalid node definition for " <> show bid

    oscGroup ← lookupGroup oscillatorsBlockId
    seqGroup ← lookupGroup sequencersBlockId

    let
      getEntries g = 
        let m = GraphNE.toMap g.children in
        if Map.member (BlockId.make P [ L, A, C, E, H, O, L, D, E, R ] []) m then []
        else Map.toUnfoldable m

      allEntries = getEntries oscGroup <> getEntries seqGroup

    decodedEntries ← traverse decodeNode allEntries

    case Array.uncons decodedEntries of
      Just { head, tail } →
        case make head tail of
          Left err → Left $ show err
          Right ans → Right ans
      Nothing → Left "No audio nodes found in group block"

  baseId ∷ BlockId → BlockId
  baseId bid = fromMaybe bid $ do
    let s = Codec.encoder BlockId.stringCodec unit bid
    let sWithoutPrefix = fromMaybe s (String.stripPrefix (String.Pattern "def_") s)
    let s' = fromMaybe sWithoutPrefix $
          stripSuffix "_frequency" sWithoutPrefix
          <|> stripSuffix "_gain" sWithoutPrefix
          <|> stripSuffix "_wave" sWithoutPrefix
          <|> stripSuffix "_duration" sWithoutPrefix
          <|> stripSuffix "_sequence" sWithoutPrefix
          <|> stripSuffix "_inputs" sWithoutPrefix
          <|> stripSuffix "_parameters" sWithoutPrefix
          <|> stripSuffix "_dummy" sWithoutPrefix
          <|> stripSuffix "_empty" sWithoutPrefix
    case P.runParser s' (Codec.decoder BlockId.stringCodec) of
      Left _ → Nothing
      Right bid' → Just bid'

  stripSuffix sx s = String.stripSuffix (String.Pattern sx) s

groupBlockEncoder ∷ Encoder AudioNodes GroupBlock Unit
groupBlockEncoder _ (AudioNodes graph) =
  let
    entries = Map.toUnfoldable (GraphNE.toMap graph)
    nodeTypes = nodesById (AudioNodes graph)
    edgeMap = entries <#> (\(id /\ (_ /\ edges)) → id /\ edges) #
      Map.fromFoldable

    translateTarget sourceId targetId =
      case Map.lookup targetId nodeTypes of
        Just { audioNode: Oscillator _ } →
          let
            bid = toBlockId targetId
            suffix = case (nodesById (AudioNodes graph) # Map.lookup sourceId) of
              Just { audioNode: FrequencySequencer _ } → frequencyBlockId
              Just { audioNode: GainSequencer _ } → gainBlockId
              _ → BlockId.make E [ M, P, T, Y ] []
          in
            bid <> suffix
        _ → toBlockId targetId

    isOscillator (_ /\ ({ audioNode } /\ _)) = case audioNode of
      Oscillator _ → true
      _ → false

    isSequencer (_ /\ ({ audioNode } /\ _)) = case audioNode of
      FrequencySequencer _ → true
      GainSequencer _ → true
      _ → false

    oscillators = Array.filter isOscillator entries
    sequencers = Array.filter isSequencer entries

    encodeNode (id /\ ({ audioNode, isConnectedToOutput } /\ _)) = 
      let
        bid = toBlockId id
        edges = fromMaybe Set.empty $ Map.lookup id edgeMap
        edges' = translateTarget id `Set.map` edges
        finalEdges = if isConnectedToOutput then Set.insert outputBlockId edges' else edges'
      in
        bid /\
          ( Group (Codec.encoder AudioNode.groupBlockCodec bid audioNode) /\
              finalEdges
          )

    buildGroup nodes = case Array.uncons nodes of
      Just { head, tail } →
        let 
          h = encodeNode head
          t = encodeNode <$> tail
          gMap = Map.fromFoldable t
          g = Graph.fromMap gMap
        in 
          GraphNE.make (Tuple.fst h) (Tuple.snd h) g
      Nothing → GraphNE.singleton (BlockId.make P [ L, A, C, E, H, O, L, D, E, R ] []) 
        (Node "empty" /\ Set.empty)

    oscGroup = buildGroup oscillators
    seqGroup = buildGroup sequencers

    rootChildren = 
      ( if Array.null oscillators then [] 
        else 
          [ oscillatorsBlockId /\
              ( Group 
                  { children: oscGroup 
                  , properties: { columns: Nothing } 
                  , spacedOut: false 
                  } /\ Set.empty 
              )
          ]
      )
        <>
          ( if Array.null sequencers then [] 
            else 
              [ sequencersBlockId /\
                  ( Group 
                      { children: seqGroup 
                      , properties: { columns: Nothing } 
                      , spacedOut: false 
                      } /\ Set.empty 
                  )
              ]
          )
        <>
          [ outputBlockId /\ (Node "output" /\ Set.empty) ]
  in 
    case Array.uncons rootChildren of
      Just { head, tail } →
        let 
          gMap = Map.fromFoldable tail
          g = Graph.fromMap gMap
          rootG = GraphNE.make (Tuple.fst head) (Tuple.snd head) g
        in 
          { children: rootG 
          , properties: { columns: Just C1 } 
          , spacedOut: true 
          }
      Nothing → 
        { children: GraphNE.singleton outputBlockId 
            (Node "output" /\ Set.empty)
        , properties: { columns: Just C1 } 
        , spacedOut: true 
        }

oscillatorsBlockId ∷ BlockId
oscillatorsBlockId = BlockId.make O [ S, C, I, L, L, A, T, O, R, S ] []

outputBlockId ∷ BlockId
outputBlockId = BlockId.make O [ U, T, P, U, T ] []

sequencersBlockId ∷ BlockId
sequencersBlockId = BlockId.make S [ E, Q, U, E, N, C, E, R, S ] []