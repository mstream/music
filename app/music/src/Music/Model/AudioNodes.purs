module Music.Model.AudioNodes
  ( AudioNodes
  , Entry
  , Violation(..)
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
import Data.Foldable (any, foldl)
import Data.FoldableWithIndex (foldMapWithIndex, foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Graph (Edge, Graph)
import Data.Graph as Graph
import Data.Graph.NonEmpty (NonEmptyGraph)
import Data.Graph.NonEmpty as GraphNE
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (all, traverse)
import Data.Tuple (uncurry)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode as AudioNode
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Parsing (ParseState(..), Parser, fail, getParserT, runParser) as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)

newtype AudioNodes = AudioNodes (NonEmptyGraph AudioNodeId AudioNode)

derive newtype instance Eq AudioNodes
derive newtype instance Ord AudioNodes
derive newtype instance Show AudioNodes

instance Arbitrary AudioNodes where
  arbitrary = do
    firstNodeId ← genNodeId
    firstNode ← genNode
    pure $ AudioNodes $ GraphNE.singleton firstNodeId
      (firstNode /\ Nil)
    where
    genNode ∷ Gen AudioNode
    genNode = arbitrary

    genNodeId ∷ Gen AudioNodeId
    genNodeId = AudioNodeId.userDefined <$> arbitrary

type Violations = Map AudioNodeId (Set Violation)

data Violation
  = ConnectedWithItself
  | ConnectedWithNonExistingNode AudioNodeId
  | DuplicatedEntry
  | MissingUserDefinedPrefix
  | NoSuchNode

derive instance Eq Violation
derive instance Generic Violation _

derive instance Ord Violation

instance Show Violation where
  show = genericShow

updateAudioNode
  ∷ AudioNodes → AudioNodeId → AudioNode → Violations \/ AudioNodes
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
        make firstEntry otherEntries
    Nothing →
      Left $ Map.singleton nodeId (Set.singleton NoSuchNode)
  where
  nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
  nodes = GraphNE.toMap graph

type Entry = AudioNodeId /\ (AudioNode /\ List AudioNodeId)

make ∷ Entry → Array Entry → Violations \/ AudioNodes
make firstEntry otherEntries =
  if Map.isEmpty duplicationViolations then do
    let
      nodes ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId)
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
  ∷ Map AudioNodeId (AudioNode /\ List AudioNodeId) → Violations
findViolations nodes = Map.filter
  (not <<< eq Set.empty)
  (foldlWithIndex f Map.empty nodes)
  where
  f
    ∷ AudioNodeId
    → Violations
    → AudioNode /\ List AudioNodeId
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
          ( if isJust $ String.stripPrefix (Pattern "def_") nodeIdString then
              Set.empty
            else Set.singleton MissingUserDefinedPrefix
          )
        <>
          ( ConnectedWithNonExistingNode `Set.map`
              nonExistingNodesReferences
          )

    nodeIdString ∷ String
    nodeIdString = Codec.encoder BlockId.stringCodec unit nodeId

    isNotConnectedWithItself ∷ Boolean
    isNotConnectedWithItself = all (not <<< eq nodeId) connectionEnds

    nonExistingNodesReferences ∷ Set AudioNodeId
    nonExistingNodesReferences = Set.fromFoldable $ List.filter
      ( \connectionEnd → connectionEnd /= AudioNodeId.output
          && not (Map.member connectionEnd nodes)
          &&
            not (isSubNode connectionEnd)
      )
      connectionEnds

    isSubNode ∷ AudioNodeId → Boolean
    isSubNode ce =
      any (flip isSubPort ce) (Map.keys nodes)

    isSubPort ∷ AudioNodeId → AudioNodeId → Boolean
    isSubPort base ce =
      ce == base <> AudioNodeId.duration
        || ce == base <> AudioNodeId.frequency
        || ce == base <> AudioNodeId.gain
        || ce == base <> AudioNodeId.inputs
        || ce == base <> AudioNodeId.sequence
        ||
          ce == base <> AudioNodeId.wave

toGraph ∷ AudioNodes → (Graph AudioNodeId AudioNode)
toGraph (AudioNodes graph) = GraphNE.toGraph graph

connections ∷ AudioNodes → List (Edge AudioNodeId)
connections (AudioNodes graph) = Graph.edges $ GraphNE.toGraph graph

nodesById ∷ AudioNodes → Map AudioNodeId AudioNode
nodesById (AudioNodes graph) = Tuple.fst <$> GraphNE.toMap graph

stringCodec ∷ Codec AudioNodes String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder AudioNodes String
stringDecoder = do
  lines ← PC.sepBy parseLine (PS.char '\n')
  let
    nodes = lines
      # List.mapMaybe case _ of
          LNode id node → Just (id /\ (node /\ Nil))
          _ → Nothing
      # Map.fromFoldable

    edges = lines # List.foldl
      ( \acc item → case item of
          LConn from to → Map.insertWith append from (List.singleton to)
            acc
          _ → acc
      )
      Map.empty

    combine id (node /\ _) = id /\
      (node /\ (fromMaybe Nil $ Map.lookup id edges))

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
  nodeIdParser = do
    bid ← Codec.decoder BlockId.stringCodec
    let s = Codec.encoder BlockId.stringCodec unit bid
    if s == "output" then pure bid
    else
      case
        P.runParser ("def_" <> s) (Codec.decoder BlockId.stringCodec)
        of
        Right bid' → pure bid'
        Left _ → pure bid

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
stringEncoder _ = encodeString

encodeString
  ∷ AudioNodes
  → String
encodeString (AudioNodes graph) = String.joinWith "\n"
  (nodeLines <> connectionLines)
  where
  nodeLines ∷ Array String
  nodeLines = renderNodeLine <$> sortedEntries

  renderNodeLine
    ∷ AudioNodeId /\ (AudioNode /\ List AudioNodeId)
    → String
  renderNodeLine (nodeId /\ (node /\ _)) =
    renderId nodeId <> " " <> renderNodeConf node

  renderNodeConf ∷ AudioNode → String
  renderNodeConf = Codec.encoder AudioNode.stringCodec unit

  connectionLines ∷ Array String
  connectionLines = renderedConnections

  renderedConnections ∷ Array String
  renderedConnections =
    sortedEntries >>= \(nodeId /\ (_ /\ ends)) →
      renderConnection nodeId <$> Array.fromFoldable ends

  renderConnection ∷ AudioNodeId → AudioNodeId → String
  renderConnection from to =
    renderId from <> "->" <> renderId to

  sortedEntries
    ∷ Array (AudioNodeId /\ (AudioNode /\ List AudioNodeId))
  sortedEntries = Array.sortWith
    ( \(nodeId /\ _) →
        Codec.encoder BlockId.stringCodec unit nodeId
    )
    (Map.toUnfoldable $ GraphNE.toMap graph)

type DecodeState =
  { connections ∷ Map AudioNodeId (Array AudioNodeId)
  , nodes ∷ Map AudioNodeId AudioNode
  }

renderId ∷ AudioNodeId → String
renderId nodeId = case stripped of
  Just rest | usesDefaultPrefix rest →
    rest
  _ →
    raw
  where
  raw ∷ String
  raw = Codec.encoder BlockId.stringCodec unit nodeId

  stripped ∷ Maybe String
  stripped = String.stripPrefix (Pattern "def_") raw

usesDefaultPrefix ∷ String → Boolean
usesDefaultPrefix s = s /= "output"

groupBlockCodec ∷ Codec AudioNodes GroupBlock Unit
groupBlockCodec = Codec.codec groupBlockDecoder groupBlockEncoder

groupBlockDecoder ∷ Decoder AudioNodes GroupBlock
groupBlockDecoder = do
  P.ParseState input _ _ ← P.getParserT
  case decodeGroupBlock input of
    Left err → P.fail err
    Right ans → pure ans
  where
  decodeGroupBlock ∷ GroupBlock → Either String AudioNodes
  decodeGroupBlock gb = do
    let
      childrenMap = GraphNE.toMap gb.children

      lookupGroup key = case Map.lookup key childrenMap of
        Just (Group g /\ _) → Right g
        _ → Right
          { children: GraphNE.singleton (unsafeBlockId "placeholder")
              (Node "empty" /\ Nil)
          , properties: { columns: Nothing }
          , spacedOut: false
          } -- Treat missing group as empty group

      decodeNode (id /\ (def /\ edges)) = case def of
        Group g → do
          node ←
            case
              P.runParser g (Codec.decoder AudioNode.groupBlockCodec)
              of
              Left err → Left $ show err
              Right n → Right n
          let edges' = baseId <$> edges
          pure (id /\ (node /\ edges'))
        _ → Left $ "Invalid node definition for " <> show id

    oscGroup ← lookupGroup oscillatorsId
    seqGroup ← lookupGroup sequencersId

    let
      getEntries g =
        let
          m = GraphNE.toMap g.children
        in
          if Map.member (unsafeBlockId "placeholder") m then []
          else Map.toUnfoldable m

      oscEntries = getEntries oscGroup
      seqEntries = getEntries seqGroup

      allEntries = oscEntries <> seqEntries

    decodedEntries ← traverse decodeNode allEntries

    case Array.uncons decodedEntries of
      Just { head, tail } →
        case make head tail of
          Left err → Left $ show err
          Right ans → Right ans
      Nothing → Left "No audio nodes found in group block"

  oscillatorsId = unsafeBlockId "oscillators"
  sequencersId = unsafeBlockId "sequencers"

  unsafeBlockId s =
    case P.runParser s (Codec.decoder BlockId.stringCodec) of
      Right bid → bid
      Left _ → AudioNodeId.output -- Fallback

  baseId ∷ AudioNodeId → AudioNodeId
  baseId id =
    fromMaybe id $
      stripSuffix AudioNodeId.frequency id
        <|> stripSuffix AudioNodeId.gain id
        <|> stripSuffix AudioNodeId.wave id
        <|> stripSuffix AudioNodeId.duration id
        <|> stripSuffix AudioNodeId.sequence id
        <|> stripSuffix AudioNodeId.inputs id
        <|>
          stripSuffix AudioNodeId.parameters id

  stripSuffix ∷ AudioNodeId → AudioNodeId → Maybe AudioNodeId
  stripSuffix suffix id =
    let
      s = Codec.encoder BlockId.stringCodec unit id
      sx = Codec.encoder BlockId.stringCodec unit suffix
    in
      case String.stripSuffix (Pattern ("_" <> sx)) s of
        Just rest →
          case P.runParser rest (Codec.decoder BlockId.stringCodec) of
            Right bid → Just bid
            Left _ → Nothing
        Nothing → Nothing

groupBlockEncoder ∷ Encoder AudioNodes GroupBlock Unit
groupBlockEncoder _ (AudioNodes graph) =
  let
    entries = Map.toUnfoldable (GraphNE.toMap graph)
    nodeTypes = nodesById (AudioNodes graph)
    edgeMap = entries <#> (\(id /\ (_ /\ edges)) → id /\ edges) #
      Map.fromFoldable

    translateTarget sourceNode targetId =
      case Map.lookup targetId nodeTypes of
        Just (Oscillator _) →
          case sourceNode of
            FrequencySequencer _ → targetId <> AudioNodeId.frequency
            GainSequencer _ → targetId <> AudioNodeId.gain
            _ → targetId
        _ → targetId

    isOscillator (_ /\ (node /\ _)) = case node of
      Oscillator _ → true
      _ → false

    isSequencer (_ /\ (node /\ _)) = case node of
      FrequencySequencer _ → true
      GainSequencer _ → true
      _ → false

    oscillators = Array.filter isOscillator entries
    sequencers = Array.filter isSequencer entries

    encodeNode (id /\ (node /\ _)) =
      let
        edges = fromMaybe Nil $ Map.lookup id edgeMap
        edges' = translateTarget node <$> edges
      in
        id /\
          ( Group (Codec.encoder AudioNode.groupBlockCodec id node) /\
              edges'
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
      Nothing → GraphNE.singleton (unsafeBlockId "placeholder")
        (Node "empty" /\ Nil)

    oscGroup = buildGroup oscillators
    seqGroup = buildGroup sequencers

    rootChildren =
      ( if Array.null oscillators then []
        else
          [ oscillatorsId /\
              ( Group
                  { children: oscGroup
                  , properties: { columns: Nothing }
                  , spacedOut: false
                  } /\ Nil
              )
          ]
      )
        <>
          ( if Array.null sequencers then []
            else
              [ sequencersId /\
                  ( Group
                      { children: seqGroup
                      , properties: { columns: Nothing }
                      , spacedOut: false
                      } /\ Nil
                  )
              ]
          )
        <>
          [ AudioNodeId.output /\ (Node "output" /\ Nil) ]
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
        { children: GraphNE.singleton AudioNodeId.output
            (Node "output" /\ Nil)
        , properties: { columns: Just C1 }
        , spacedOut: true
        }

  where
  oscillatorsId = unsafeBlockId "oscillators"
  sequencersId = unsafeBlockId "sequencers"
  unsafeBlockId s =
    case P.runParser s (Codec.decoder BlockId.stringCodec) of
      Right bid → bid
      Left _ → AudioNodeId.output