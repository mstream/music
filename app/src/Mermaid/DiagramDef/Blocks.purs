module Mermaid.DiagramDef.Blocks
  ( Def
  , def
  , defStringCodec
  , defStringDecoder
  , defStringEncoder
  , toGroupBlock
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as ArrayNE
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (all, foldl)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as StringCU
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef (BlockDef(..), GroupBlock)
import Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Parsing (ParseState(..), runParser)
import Parsing as P
import Test.QuickCheck.Arbitrary
  ( class Arbitrary
  )
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen
  ( arrayOf
  , elements
  , sized
  ) as Gen

newtype Def = Def GroupBlock

derive newtype instance Eq Def
derive newtype instance Ord Def
derive newtype instance Show Def

instance Arbitrary Def where
  arbitrary = do
    groupBlock ← Gen.sized
      ( map (\(group /\ _) → group)
          <<< BlockDef.genGroupBlock Set.empty
      )
    Def <$>
      case ArrayNE.fromFoldable (BlockDef.groupBlockIds groupBlock) of
        Just nonEmptyBlockIds → withRandomConnections
          ( Set.fromFoldable <$>
              (Gen.arrayOf $ Gen.elements nonEmptyBlockIds)
          )
          groupBlock
        Nothing →
          pure groupBlock
    where
    withRandomConnections
      ∷ Gen (Set BlockId) → GroupBlock → Gen GroupBlock
    withRandomConnections genConnectionEnds groupBlock = do
      childrenWithConnections ← sequence
        $ Graph.toMap groupBlock.children <#> \(blockDef /\ _) → do
            connectionEnds ← List.fromFoldable <$> genConnectionEnds
            case blockDef of
              Group groupBlock' → do
                groupBlockWithConnections ← withRandomConnections
                  genConnectionEnds
                  groupBlock'
                pure $
                  Group groupBlockWithConnections /\ connectionEnds
              Node s →
                pure $ Node s /\ connectionEnds

      pure groupBlock
        { children = Graph.fromMap childrenWithConnections }

def ∷ GroupBlock → String \/ Def
def groupBlock = Right $ Def groupBlock

toGroupBlock ∷ Def → GroupBlock
toGroupBlock (Def groupBlock) = groupBlock

defStringCodec ∷ Codec Def String Boolean
defStringCodec = Codec.codec defStringDecoder defStringEncoder

defStringDecoder ∷ Decoder Def String
defStringDecoder = do
  ParseState input _ _ ← P.getParserT
  case decodeGroupBlock input of
    Left err →
      P.fail err
    Right groupBlock →
      pure $ Def groupBlock
  where
  decodeGroupBlock ∷ String → String \/ GroupBlock
  decodeGroupBlock input = do
    let
      lines ∷ Array String
      lines = String.split (Pattern "\n") input
    groupBlock /\ rest ← parseGroup lines
    if all String.null rest then
      Right groupBlock
    else
      Left "unexpected content after group definition"

  parseGroup
    ∷ Array String
    → String \/ (GroupBlock /\ Array String)
  parseGroup lines = do
    let
      initialChildren ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
      initialChildren = []
    parseResult ← parseLines
      { columns: Nothing
      , children: initialChildren
      , edges: []
      , lines
      , spacedOut: false
      }
    attachEdges parseResult.groupBlock parseResult.edges
      <#> (_ /\ parseResult.rest)
    where
    parseLines
      ∷ { columns ∷ Maybe BlockDef.Columns
        , children ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
        , edges ∷ Array (BlockId /\ BlockId)
        , lines ∷ Array String
        , spacedOut ∷ Boolean
        }
      → String
          \/
            { groupBlock ∷ GroupBlock
            , edges ∷ Array (BlockId /\ BlockId)
            , rest ∷ Array String
            }
    parseLines
      state@
        { columns, children, edges, lines: remainingLines, spacedOut } =
      case Array.uncons remainingLines of
        Nothing →
          Right
            { edges
            , groupBlock:
                { children:
                    Graph.fromMap (Map.fromFoldable children)
                , properties: { columns }
                , spacedOut
                }
            , rest: []
            }
        Just { head, tail } →
          case stripIndent head of
            "" →
              Right
                { edges
                , groupBlock:
                    { children:
                        Graph.fromMap (Map.fromFoldable children)
                    , properties: { columns }
                    , spacedOut
                    }
                , rest: tail
                }
            "end" →
              Right
                { edges
                , groupBlock:
                    { children:
                        Graph.fromMap (Map.fromFoldable children)
                    , properties: { columns }
                    , spacedOut
                    }
                , rest: tail
                }
            "space" →
              parseLines (state { spacedOut = true, lines = tail })
            l
              | Just c ← parseColumns l →
                  parseLines (state { columns = Just c, lines = tail })
            l
              | Just (start /\ end) ← parseEdge l →
                  parseLines
                    ( state
                        { edges = Array.snoc edges (start /\ end)
                        , lines = tail
                        }
                    )
            l
              | Just (blockId /\ label) ← parseNode l →
                  parseLines
                    ( state
                        { children = Array.snoc children
                            (blockId /\ (Node label /\ Nil))
                        , lines = tail
                        }
                    )
            l
              | Just blockId ← parseBlockStart l →
                  case parseGroup tail of
                    Left err →
                      Left err
                    Right (nested /\ rest) →
                      parseLines
                        ( state
                            { children =
                                Array.snoc children
                                  (blockId /\ (Group nested /\ Nil))
                            , lines = rest
                            }
                        )
            other →
              Left $ "cannot parse line: " <> other

    stripIndent ∷ String → String
    stripIndent = dropLeadingSpaces

    parseColumns ∷ String → Maybe BlockDef.Columns
    parseColumns line = do
      suffix ← String.stripPrefix (Pattern "columns ") line
      case suffix of
        "1" → Just BlockDef.C1
        "2" → Just BlockDef.C2
        "3" → Just BlockDef.C3
        "4" → Just BlockDef.C4
        "5" → Just BlockDef.C5
        "6" → Just BlockDef.C6
        "7" → Just BlockDef.C7
        "8" → Just BlockDef.C8
        "9" → Just BlockDef.C9
        _ → Nothing

    parseEdge ∷ String → Maybe (BlockId /\ BlockId)
    parseEdge line = do
      parts ← nonEmptyPair $ String.split (Pattern "-->") line
      let
        startString ∷ String
        startString = trimSpaces parts.first

        endString ∷ String
        endString = trimSpaces parts.second
      startId ← decodeBlockId startString
      endId ← decodeBlockId endString
      pure $ startId /\ endId

    parseNode ∷ String → Maybe (BlockId /\ String)
    parseNode line = do
      idx ← String.indexOf (Pattern "[\"") line
      let
        idString ∷ String
        idString = String.take idx line

        remainder ∷ String
        remainder = String.drop (idx + 2) line
      label ← String.stripSuffix (Pattern "\"]") remainder
      blockId ← decodeBlockId idString
      pure $ blockId /\ label

    parseBlockStart ∷ String → Maybe BlockId
    parseBlockStart line =
      decodeBlockId =<< String.stripPrefix (Pattern "block:") line

    decodeBlockId ∷ String → Maybe BlockId
    decodeBlockId s =
      case runParser s (Codec.decoder BlockId.stringCodec) of
        Left _ →
          Nothing
        Right blockId →
          Just blockId

    trimSpaces ∷ String → String
    trimSpaces = dropTrailingSpaces <<< dropLeadingSpaces

    nonEmptyPair
      ∷ Array String
      → Maybe { first ∷ String, second ∷ String }
    nonEmptyPair arr = case arr of
      [ a, b ] →
        Just { first: a, second: b }
      _ →
        Nothing

    dropLeadingSpaces ∷ String → String
    dropLeadingSpaces =
      StringCU.fromCharArray
        <<< Array.dropWhile (_ == ' ')
        <<< StringCU.toCharArray

    dropTrailingSpaces ∷ String → String
    dropTrailingSpaces =
      StringCU.fromCharArray
        <<< Array.reverse
        <<< Array.dropWhile (_ == ' ')
        <<< Array.reverse
        <<< StringCU.toCharArray

  attachEdges
    ∷ GroupBlock
    → Array (BlockId /\ BlockId)
    → String \/ GroupBlock
  attachEdges groupBlock edges =
    foldl applyEdge (Right groupBlock) edges
    where
    applyEdge
      ∷ String \/ GroupBlock
      → BlockId /\ BlockId
      → String \/ GroupBlock
    applyEdge (Left err) _ = Left err
    applyEdge (Right gb) (start /\ end) = addEdge gb start end

    addEdge
      ∷ GroupBlock
      → BlockId
      → BlockId
      → String \/ GroupBlock
    addEdge currentGroup start end =
      case addEdgeToChildren start end childrenMap of
        Right updatedChildren →
          Right currentGroup
            { children = Graph.fromMap updatedChildren }
        Left err →
          Left err
      where
      childrenMap ∷ Map BlockId (BlockDef /\ List BlockId)
      childrenMap = Graph.toMap currentGroup.children

    addEdgeToChildren
      ∷ BlockId
      → BlockId
      → Map BlockId (BlockDef /\ List BlockId)
      → String \/ Map BlockId (BlockDef /\ List BlockId)
    addEdgeToChildren start end childrenMap =
      case Map.lookup start childrenMap of
        Just (blockDef /\ ends) →
          Right $ Map.insert start
            (blockDef /\ (ends <> (Cons end Nil)))
            childrenMap
        Nothing →
          updateNested (Map.toUnfoldable childrenMap)
      where
      updateNested
        ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
        → String \/ Map BlockId (BlockDef /\ List BlockId)
      updateNested entries = case Array.uncons entries of
        Nothing →
          Left $ "unknown block id: "
            <> Codec.encoder BlockId.stringCodec unit start
        Just { head: bid /\ (blockDef /\ ends), tail } →
          case blockDef of
            Group nested →
              case addEdgeGroup nested start end of
                Right nestedUpdated →
                  Right $ Map.insert bid (Group nestedUpdated /\ ends)
                    childrenMap
                Left _ →
                  updateNested tail
            Node _ →
              updateNested tail

    addEdgeGroup
      ∷ GroupBlock
      → BlockId
      → BlockId
      → String \/ GroupBlock
    addEdgeGroup nested start end =
      case addEdgeToChildren start end (Graph.toMap nested.children) of
        Right updatedChildren →
          Right nested { children = Graph.fromMap updatedChildren }
        Left err →
          Left err

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

      childrenMap ∷ Map BlockId (BlockDef /\ List BlockId)
      childrenMap = Graph.toMap groupBlock0.children

      orderedChildren
        ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
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
    → BlockId /\ (BlockDef /\ List BlockId)
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
      → List BlockId
      → Array (BlockId /\ BlockId)
    edgesFromConnections start =
      Array.fromFoldable <<< map (start /\ _)

  compareChild
    ∷ BlockId /\ (BlockDef /\ List BlockId)
    → BlockId /\ (BlockDef /\ List BlockId)
    → Ordering
  compareChild a b = case tagOf a `compare` tagOf b of
    EQ →
      nameOf a `compare` nameOf b
    other →
      other
    where
    tagOf
      ∷ BlockId /\ (BlockDef /\ List BlockId)
      → Int
    tagOf (_ /\ (blockDef /\ _)) = case blockDef of
      Node _ → 0
      Group _ → 1

    nameOf
      ∷ BlockId /\ (BlockDef /\ List BlockId)
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
