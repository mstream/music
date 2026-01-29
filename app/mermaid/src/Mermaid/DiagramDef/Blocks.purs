module Mermaid.DiagramDef.Blocks
  ( Def
  , def
  , defStringCodec
  , defStringDecoder
  , defStringEncoder
  , toGroupBlock
  ) where

import Prelude

import Control.Lazy (defer)
import Data.Array as Array
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (any, findMap, foldl)
import Data.Graph as Graph
import Data.Graph.NonEmpty as GraphNE
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Parsing (Parser)
import Parsing as P
import Parsing.Combinators as PC
import Parsing.String as PS
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt) as Gen

newtype Def = Def GroupBlock

derive newtype instance Eq Def
derive newtype instance Ord Def
derive newtype instance Show Def

instance Arbitrary Def where
  arbitrary = do
    n ← Gen.chooseInt 1 3
    groupBlock /\ _ ← BlockDef.genGroupBlock n Set.empty
    pure $ Def groupBlock

def ∷ GroupBlock → String \/ Def
def groupBlock = Right $ Def groupBlock

toGroupBlock ∷ Def → GroupBlock
toGroupBlock (Def groupBlock) = groupBlock

defStringCodec ∷ Codec Def String Boolean
defStringCodec = Codec.codec defStringDecoder defStringEncoder

defStringDecoder ∷ Decoder Def String
defStringDecoder = do
  items ← parseItems
  let
    allEdges ∷ Map BlockId (Set BlockId)
    allEdges = items # collectEdges # foldl
      ( \acc (from /\ to) →
          Map.insertWith append from (Set.singleton to) acc
      )
      Map.empty
  case buildGroupBlock allEdges items of
    Left err → P.fail err
    Right gb → pure $ Def gb

data ParsedItem
  = PCol Columns
  | PNode BlockId String
  | PGroup BlockId (List ParsedItem)
  | PEdge BlockId BlockId
  | PSpace

collectEdges ∷ List ParsedItem → List (BlockId /\ BlockId)
collectEdges = List.concatMap case _ of
  PEdge from to →
    List.singleton (from /\ to)
  PGroup _ items →
    collectEdges items
  _ →
    Nil

parseItems ∷ Parser String (List ParsedItem)
parseItems = List.fromFoldable <$> PC.many (skipSpaces *> parseItem)

skipSpaces ∷ Parser String Unit
skipSpaces = void $ PC.many
  (PS.satisfy \c → c == ' ' || c == '\n' || c == '\t')

parseItem ∷ Parser String ParsedItem
parseItem = PC.choice
  [ parseColumns
  , defer \_ → parseGroup
  , parseSpace
  , parseEdgeOrNode
  ]

parseColumns ∷ Parser String ParsedItem
parseColumns = do
  _ ← PC.try (PS.string "columns ")
  n ← PC.choice
    [ PS.string "1" $> C1
    , PS.string "2" $> C2
    , PS.string "3" $> C3
    , PS.string "4" $> C4
    , PS.string "5" $> C5
    , PS.string "6" $> C6
    , PS.string "7" $> C7
    , PS.string "8" $> C8
    , PS.string "9" $> C9
    ]
  pure $ PCol n

parseSpace ∷ Parser String ParsedItem
parseSpace = PC.try (PS.string "space") $> PSpace

blockIdParser ∷ Parser String BlockId
blockIdParser = Codec.decoder BlockId.stringCodec

parseGroup ∷ Parser String ParsedItem
parseGroup = do
  _ ← PC.try (PS.string "block:")
  id ← blockIdParser
  items ← List.fromFoldable <$> PC.manyTill (skipSpaces *> parseItem)
    (PC.try (skipSpaces *> PS.string "end"))
  pure $ PGroup id items

parseEdgeOrNode ∷ Parser String ParsedItem
parseEdgeOrNode = PC.try do
  id1 ← blockIdParser
  PC.choice
    [ do
        _ ← PS.string " --> "
        id2 ← blockIdParser
        pure $ PEdge id1 id2
    , do
        _ ← PS.string "[\""
        label ← parseLabel
        _ ← PS.string "\"]"
        pure $ PNode id1 label
    ]

parseLabel ∷ Parser String String
parseLabel = do
  chars ← PC.many (PS.satisfy \c → c /= '"')
  pure $ SCU.fromCharArray $ Array.fromFoldable chars

buildGroupBlock
  ∷ Map BlockId (Set BlockId)
  → List ParsedItem
  → Either String GroupBlock
buildGroupBlock allEdges items = do
  let
    col ∷ Maybe Columns
    col = items # findMap case _ of
      PCol c → Just c
      _ → Nothing

    space ∷ Boolean
    space = items # any case _ of
      PSpace → true
      _ → false

  rawNodes ∷ List (BlockId /\ BlockDef) ← items
    # List.mapMaybe
        ( case _ of
            PNode id label → Just $ pure (id /\ Node label)
            PGroup id nestedItems → Just do
              gb ← buildGroupBlock allEdges nestedItems
              pure (id /\ Group gb)
            _ → Nothing
        )
    # sequence

  let
    combine
      ∷ BlockId → BlockDef → (BlockId /\ (BlockDef /\ Set BlockId))
    combine id blockDef = id /\
      (blockDef /\ (fromMaybe Set.empty $ Map.lookup id allEdges))

    combinedList ∷ List (BlockId /\ (BlockDef /\ Set BlockId))
    combinedList = rawNodes <#> uncurry combine

  case List.uncons combinedList of
    Nothing → Left "Group block must have at least one child"
    Just { head: (hId /\ hData), tail } →
      let
        graphMap = Map.fromFoldable tail
        graph = Graph.fromMap graphMap
        neGraph = GraphNE.make hId hData graph
      in
        Right
          { children: neGraph
          , properties: { columns: col }
          , spacedOut: space
          }

defStringEncoder ∷ Encoder Def String Boolean
defStringEncoder useIndent (Def groupBlock) =
  String.joinWith "\n" (result.lines <> edgeLines)
  where
  result
    ∷ { edges ∷ Array (BlockId /\ BlockId)
      , lines ∷ Array String
      }
  result = encodeGroup 1 groupBlock

  edgeLines ∷ Array String
  edgeLines = result.edges <#> renderEdge (makeIndent 1)

  encodeGroup
    ∷ Int
    → GroupBlock
    → { edges ∷ Array (BlockId /\ BlockId), lines ∷ Array String }
  encodeGroup indent groupBlock0 =
    let
      indentString ∷ String
      indentString = makeIndent indent

      childrenMap ∷ Map BlockId (BlockDef /\ Set BlockId)
      childrenMap = GraphNE.toMap groupBlock0.children

      orderedChildren
        ∷ Array (BlockId /\ (BlockDef /\ Set BlockId))
      orderedChildren = Array.sortBy compareChild
        (Map.toUnfoldable childrenMap)

      encodedChildren
        ∷ Array
            { edges ∷ Array (BlockId /\ BlockId)
            , lines ∷ Array String
            }
      encodedChildren = orderedChildren
        <#> encodeChild indent indentString

      definitionLines ∷ Array String
      definitionLines =
        addSpacing indentString groupBlock0.spacedOut
          (_.lines <$> encodedChildren)

      columnLine ∷ Array String
      columnLine = case groupBlock0.properties.columns of
        Just c →
          [ indentString <> "columns " <> BlockDef.columnsToString c ]
        Nothing →
          []
    in
      { edges: Array.concat (_.edges <$> encodedChildren)
      , lines: columnLine <> definitionLines
      }

  encodeChild
    ∷ Int
    → String
    → BlockId /\ (BlockDef /\ Set BlockId)
    → { edges ∷ Array (BlockId /\ BlockId), lines ∷ Array String }
  encodeChild
    currentIndent
    indentString
    (bid /\ (blockDef /\ connectionEnds)) =
    case blockDef of
      Node label →
        { edges: edgesFromConnections bid connectionEnds
        , lines:
            [ indentString
                <> Codec.encoder BlockId.stringCodec unit bid
                <> "[\""
                <> label
                <> "\"]"
            ]
        }
      Group nested →
        let
          nestedResult
            ∷ { edges ∷ Array (BlockId /\ BlockId)
              , lines ∷ Array String
              }
          nestedResult = encodeGroup (currentIndent + 1) nested

          startLine ∷ String
          startLine = indentString <> "block:"
            <> Codec.encoder BlockId.stringCodec unit bid

          endLine ∷ String
          endLine = indentString <> "end"
        in
          { edges:
              edgesFromConnections bid connectionEnds <>
                nestedResult.edges
          , lines:
              Array.cons startLine nestedResult.lines <> [ endLine ]
          }
    where
    edgesFromConnections
      ∷ BlockId
      → Set BlockId
      → Array (BlockId /\ BlockId)
    edgesFromConnections start =
      Array.fromFoldable <<< Set.map (start /\ _)

  compareChild
    ∷ BlockId /\ (BlockDef /\ Set BlockId)
    → BlockId /\ (BlockDef /\ Set BlockId)
    → Ordering
  compareChild a b = case tagOf a `compare` tagOf b of
    EQ →
      nameOf a `compare` nameOf b
    other →
      other
    where
    tagOf
      ∷ BlockId /\ (BlockDef /\ Set BlockId)
      → Int
    tagOf (_ /\ (blockDef /\ _)) = case blockDef of
      Node _ → 0
      Group _ → 1

    nameOf
      ∷ BlockId /\ (BlockDef /\ Set BlockId)
      → String
    nameOf (bid /\ _) = Codec.encoder BlockId.stringCodec unit bid

  addSpacing
    ∷ String
    → Boolean
    → Array (Array String)
    → Array String
  addSpacing indentString spaced segments =
    case Array.uncons segments of
      Nothing →
        if spaced then [ indentString <> "space" ] else []
      Just { head, tail } →
        head <> go tail <> trailingSpace
    where
    go ∷ Array (Array String) → Array String
    go remaining = case Array.uncons remaining of
      Nothing →
        []
      Just { head, tail } →
        spacer <> head <> go tail

    spacer ∷ Array String
    spacer =
      if spaced then [ indentString <> "space" ] else []

    trailingSpace ∷ Array String
    trailingSpace =
      if spaced && Array.length segments < 2 then spacer else []

  renderEdge ∷ String → BlockId /\ BlockId → String
  renderEdge indentString (start /\ end) =
    indentString
      <> Codec.encoder BlockId.stringCodec unit start
      <> " --> "
      <> Codec.encoder BlockId.stringCodec unit end

  makeIndent ∷ Int → String
  makeIndent n =
    if useIndent then
      String.joinWith "" (Array.replicate (n * 2) " ")
    else
      ""
