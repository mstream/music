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
import Data.Either (Either(..), note)
import Data.Either.Nested (type (\/))
import Data.Foldable (foldr)
import Data.Graph as Graph
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  , columnsToString
  )
import Mermaid.DiagramDef.Blocks.BlockDef as BlockDef
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Parsing (ParseState(..), parseErrorMessage, runParser)
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

instance Eq Def where
  eq (Def groupBlock1) (Def groupBlock2) =
    Group groupBlock1 == Group groupBlock2

instance Ord Def where
  compare (Def groupBlock1) (Def groupBlock2) =
    compare (Group groupBlock1) (Group groupBlock2)

instance Show Def where
  show (Def groupBlock) = show $ Group groupBlock

def ∷ GroupBlock → String \/ Def
def groupBlock = Right $ Def groupBlock

toGroupBlock ∷ Def → GroupBlock
toGroupBlock (Def groupBlock) = groupBlock

defStringCodec ∷ Codec Def String Boolean
defStringCodec = Codec.codec defStringDecoder defStringEncoder

defStringDecoder ∷ Decoder Def String
defStringDecoder = do
  ParseState input _ _ ← P.getParserT
  case parseDef input of
    Left err →
      P.fail err
    Right groupBlock →
      pure $ Def groupBlock

defStringEncoder ∷ Encoder Def String Boolean
defStringEncoder useIndent (Def groupBlock) =
  String.joinWith "\n" (bodyLines <> connectionLines)
  where
  indentUnit ∷ String
  indentUnit = if useIndent then "  " else ""

  rendered ∷ Rendered
  rendered = renderGroup indentUnit baseIndent groupBlock

  bodyLines ∷ Array String
  bodyLines = rendered.lines

  connectionLines ∷ Array String
  connectionLines =
    renderConnection indentUnit baseIndent <$> rendered.connections

baseIndent ∷ Int
baseIndent = 1

type Rendered =
  { connections ∷ Array (BlockId /\ BlockId)
  , lines ∷ Array String
  }

parseDef ∷ String → Either String GroupBlock
parseDef input = do
  let
    rows = String.split (Pattern "\n") input
  groupBlock /\ remaining ← parseGroup false rows
  if allBlank remaining then Right groupBlock
  else Left "Unexpected text after diagram"

renderGroup ∷ String → Int → GroupBlock → Rendered
renderGroup indentUnit indentLevel groupBlock =
  { connections: collectConnections groupBlock
  , lines: headerLines <> childrenLines
  }
  where
  indent ∷ String
  indent = indentText indentUnit indentLevel

  headerLines ∷ Array String
  headerLines = case groupBlock.properties.columns of
    Nothing →
      []
    Just columns →
      [ indent <> "columns " <> columnsToString columns ]

  childrenLines ∷ Array String
  childrenLines = Array.concat spacedBlocks

  spacedBlocks ∷ Array (Array String)
  spacedBlocks =
    if groupBlock.spacedOut then
      case Array.length childBlocks of
        0 →
          [ [ spaceLine ] ]
        1 →
          childBlocks <> [ [ spaceLine ] ]
        _ →
          Array.intersperse [ spaceLine ] childBlocks
    else
      childBlocks

  childBlocks ∷ Array (Array String)
  childBlocks = encodeChild <$> Map.toUnfoldable childMap

  spaceLine ∷ String
  spaceLine = indent <> "space"

  childMap ∷ Map BlockId (BlockDef /\ List BlockId)
  childMap = Graph.toMap groupBlock.children

  encodeChild
    ∷ BlockId /\ (BlockDef /\ List BlockId)
    → Array String
  encodeChild (blockId /\ (blockDef /\ _)) = case blockDef of
    Node label →
      [ indent <> show blockId <> "[\"" <> label <> "\"]" ]
    Group nested →
      let
        nestedRendered ∷ Rendered
        nestedRendered =
          renderGroup indentUnit (indentLevel + 1) nested

        nestedIndent ∷ String
        nestedIndent = indentText indentUnit indentLevel
      in
        Array.cons
          (nestedIndent <> "block:" <> show blockId)
          ( nestedRendered.lines
              <> [ nestedIndent <> "end" ]
          )

renderConnection ∷ String → Int → BlockId /\ BlockId → String
renderConnection indentUnit indentLevel (start /\ end) =
  indentText indentUnit indentLevel
    <> show start
    <> " --> "
    <> show end

collectConnections ∷ GroupBlock → Array (BlockId /\ BlockId)
collectConnections { children } =
  Array.concatMap collectEntry
    (Map.toUnfoldable (Graph.toMap children))
  where
  collectEntry
    ∷ BlockId /\ (BlockDef /\ List BlockId)
    → Array (BlockId /\ BlockId)
  collectEntry (blockId /\ (blockDef /\ ends)) =
    fromCurrent <> nested
    where
    fromCurrent ∷ Array (BlockId /\ BlockId)
    fromCurrent =
      (\end → blockId /\ end) <$> Array.fromFoldable ends

    nested ∷ Array (BlockId /\ BlockId)
    nested = case blockDef of
      Group nestedGroup →
        collectConnections nestedGroup
      Node _ →
        []

parseGroup
  ∷ Boolean
  → Array String
  → Either String (GroupBlock /\ Array String)
parseGroup stopAtEnd lines = do
  let
    trimmedLines = dropBlanks lines
    leadingSpaces /\ withoutSpaces = spanSpaces trimmedLines
    spacedStart = not $ Array.null leadingSpaces
  columns /\ afterColumns ← parseColumns withoutSpaces
  children /\ spacedOutChildren /\ afterChildren ←
    parseChildren stopAtEnd spacedStart afterColumns
  let
    groupBlock ∷ GroupBlock
    groupBlock =
      { children: Graph.fromMap children
      , properties: { columns }
      , spacedOut: spacedOutChildren || spacedStart
      }
  groupBlockWithConnections /\ remaining ← parseConnections
    stopAtEnd
    groupBlock
    afterChildren
  if stopAtEnd then
    case Array.uncons remaining of
      Nothing →
        Left "Expected end"
      Just { head, tail } →
        if trimIndent head == "end" then
          Right (groupBlockWithConnections /\ tail)
        else
          Left "Expected end"
  else
    Right (groupBlockWithConnections /\ remaining)

parseColumns
  ∷ Array String
  → Either String (Maybe Columns /\ Array String)
parseColumns lines = case Array.uncons lines of
  Nothing →
    Right (Nothing /\ [])
  Just { head, tail } → do
    let trimmed = trimIndent head
    if hasPrefix "columns " trimmed then do
      columns ← toColumns =<< stripPrefix "columns " trimmed
      Right (Just columns /\ tail)
    else
      Right (Nothing /\ head `Array.cons` tail)

parseChildren
  ∷ Boolean
  → Boolean
  → Array String
  → Either String
      ( Map BlockId (BlockDef /\ List BlockId)
          /\ Boolean
          /\ Array String
      )
parseChildren stopAtEnd spacedStart = go Map.empty spacedStart
  where
  go
    ∷ Map BlockId (BlockDef /\ List BlockId)
    → Boolean
    → Array String
    → Either String
        ( Map BlockId (BlockDef /\ List BlockId)
            /\ Boolean
            /\ Array String
        )
  go acc spacedOut lines = case Array.uncons lines of
    Nothing →
      Right (acc /\ spacedOut /\ [])
    Just { head, tail } → do
      let trimmed = trimIndent head
      if allBlank [ trimmed ] then
        go acc spacedOut tail
      else if stopAtEnd && trimmed == "end" then
        Right (acc /\ spacedOut /\ head `Array.cons` tail)
      else if isConnectionLine trimmed then
        Right (acc /\ spacedOut /\ head `Array.cons` tail)
      else if trimmed == "space" then
        go acc true tail
      else if hasPrefix "block:" trimmed then do
        blockIdText ← stripPrefix "block:" trimmed
        blockId ← decodeBlockId blockIdText
        nested /\ remaining ← parseGroup true tail
        go
          ( Map.insert blockId (Group nested /\ Nil) acc
          )
          spacedOut
          remaining
      else if hasPrefix "columns " trimmed then
        Left "Columns must appear before blocks"
      else do
        nodeId /\ label ← parseNode trimmed
        go
          (Map.insert nodeId (Node label /\ Nil) acc)
          spacedOut
          tail

parseConnections
  ∷ Boolean
  → GroupBlock
  → Array String
  → Either String (GroupBlock /\ Array String)
parseConnections stopAtEnd groupBlock = go groupBlock
  where
  allIds ∷ Set BlockId
  allIds = BlockDef.groupBlockIds groupBlock

  go
    ∷ GroupBlock
    → Array String
    → Either String (GroupBlock /\ Array String)
  go current lines = case Array.uncons lines of
    Nothing →
      Right (current /\ [])
    Just { head, tail } → do
      let trimmed = trimIndent head
      if allBlank [ trimmed ] then
        go current tail
      else if stopAtEnd && trimmed == "end" then
        Right (current /\ head `Array.cons` tail)
      else if isConnectionLine trimmed then do
        startText /\ endText ← parseConnection trimmed
        startId ← decodeBlockId startText
        endId ← decodeBlockId endText
        if Set.member endId allIds then do
          updated ← addConnection startId endId current
          go updated tail
        else
          Left $ "Unknown connection target: " <> show endId
      else
        Left $ "Unexpected line: " <> trimmed

addConnection
  ∷ BlockId
  → BlockId
  → GroupBlock
  → Either String GroupBlock
addConnection start end groupBlock = case Map.lookup start children of
  Just (blockDef /\ ends) →
    Right groupBlock
      { children = Graph.fromMap
          ( Map.insert start (blockDef /\ List.snoc ends end) children
          )
      }
  Nothing →
    updateNested (Map.toUnfoldable children)
  where
  children ∷ Map BlockId (BlockDef /\ List BlockId)
  children = Graph.toMap groupBlock.children

  updateNested
    ∷ Array (BlockId /\ (BlockDef /\ List BlockId))
    → Either String GroupBlock
  updateNested entries = case Array.uncons entries of
    Nothing →
      Left $ "Connection start not defined: " <> show start
    Just { head, tail } → case head of
      blockId /\ (blockDef /\ ends) → case blockDef of
        Node _ →
          updateNested tail
        Group nested → case addConnection start end nested of
          Left _ →
            updateNested tail
          Right updatedNested →
            Right groupBlock
              { children = Graph.fromMap
                  ( Map.insert blockId
                      (Group updatedNested /\ ends)
                      children
                  )
              }

parseNode ∷ String → Either String (BlockId /\ String)
parseNode line = do
  openIndex ← note "Missing '['" (String.indexOf (Pattern "[") line)
  let
    idText = String.take openIndex line
    rest = String.drop (openIndex + 1) line
  content ← case String.stripSuffix (Pattern "]") rest of
    Nothing →
      Left "Missing closing ']'"
    Just inside →
      parseLabel inside
  blockId ← decodeBlockId idText
  Right (blockId /\ content)
  where
  parseLabel ∷ String → Either String String
  parseLabel text = case String.stripPrefix (Pattern "\"") text of
    Nothing →
      Left "Missing opening quote"
    Just noOpen → case String.stripSuffix (Pattern "\"") noOpen of
      Nothing →
        Left "Missing closing quote"
      Just label →
        Right label

parseConnection ∷ String → Either String (String /\ String)
parseConnection line = case String.split (Pattern "-->") line of
  [ start, end ] →
    Right (String.trim start /\ String.trim end)
  _ →
    Left "Invalid connection line"

decodeBlockId ∷ String → Either String BlockId
decodeBlockId text = case runParser text decoder of
  Left parseError →
    Left $ parseErrorMessage parseError
  Right blockId →
    Right blockId
  where
  decoder ∷ P.Parser String BlockId
  decoder = Codec.decoder BlockId.stringCodec

toColumns ∷ String → Either String Columns
toColumns = case _ of
  "1" →
    Right C1
  "2" →
    Right C2
  "3" →
    Right C3
  "4" →
    Right C4
  "5" →
    Right C5
  "6" →
    Right C6
  "7" →
    Right C7
  "8" →
    Right C8
  "9" →
    Right C9
  other →
    Left $ "Unsupported column count: " <> other

stripPrefix ∷ String → String → Either String String
stripPrefix prefix text =
  case
    String.stripPrefix (Pattern prefix) text
    of
    Just remainder →
      Right remainder
    Nothing →
      Left $ "Expected prefix: " <> prefix

trimIndent ∷ String → String
trimIndent = String.trim

hasPrefix ∷ String → String → Boolean
hasPrefix prefix text =
  String.take (String.length prefix) text == prefix

indentText ∷ String → Int → String
indentText unit level = String.joinWith "" (Array.replicate level unit)

dropBlanks ∷ Array String → Array String
dropBlanks lines = case Array.uncons lines of
  Nothing →
    []
  Just { head, tail } →
    if String.trim head == "" then dropBlanks tail
    else head `Array.cons` tail

spanSpaces ∷ Array String → Array String /\ Array String
spanSpaces lines = case Array.uncons lines of
  Nothing →
    [] /\ []
  Just { head, tail } →
    if trimIndent head == "space" then
      let
        spaces /\ rest = spanSpaces tail
      in
        head `Array.cons` spaces /\ rest
    else
      [] /\ lines

allBlank ∷ Array String → Boolean
allBlank = foldr (\line acc → String.trim line == "" && acc) true

isConnectionLine ∷ String → Boolean
isConnectionLine = String.contains (Pattern "-->")
