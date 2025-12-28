module Model.AudioNodes.Codec.Diagram
  ( Diagram
  , codec
  , fromString
  , toString
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function.Uncurried (Fn2, Fn5, mkFn5, runFn2)
import Data.Identity (Identity)
import Data.Map (fromFoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Model.AudioNodeId (AudioNodeId)
import Model.AudioNodeId as AudioNodeId
import Model.AudioNodes
  ( AudioNode(..)
  , AudioNodes
  )
import Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Model.AudioNodes.Frequency as Frequency
import Model.AudioNodes.Gain as Gain
import Model.AudioNodes.Wave as Wave
import Parsing
  ( ParseError
  , ParseState(..)
  , Parser
  , ParserT(..)
  , runParser
  )
import Parsing.String (char, string)
import Parsing.String.Basic (skipSpaces)

newtype Diagram = Diagram String

derive instance Eq Diagram
derive instance Ord Diagram
derive newtype instance Show Diagram

fromString ∷ Partial ⇒ String → Diagram
fromString s = case runParser s stringDecoder of
  Right _ →
    Diagram s

toString ∷ Diagram → String
toString (Diagram s) = s

codec ∷ AudioNodesCodec Diagram Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder Diagram
decoder = ParserT f
  where
  f
    ∷ ∀ r
    . Fn5
        (ParseState Diagram)
        ((Unit → r) → r)
        (Identity (Unit → r) → r)
        (Fn2 (ParseState Diagram) ParseError r)
        (Fn2 (ParseState Diagram) AudioNodes r)
        r
  f = mkFn5 \state@(ParseState (Diagram s) _ _) _ _ throw done →
    case runParser s stringDecoder of
      Left parsingError →
        runFn2 throw state parsingError
      Right audioNodes →
        runFn2 done state audioNodes

stringDecoder ∷ AudioNodesDecoder String
stringDecoder = do
  _ ← string "block"
  nodes ← manyNodes
  pure $ fromFoldable nodes
  where
  manyNodes =
    ( do
        node ← nodeLine
        more ← manyNodes
        pure $ cons node more
    ) <|> pure []

  nodeLine = char '\n' *> skipSpaces *> audioNodeEntry

encoder ∷ AudioNodesEncoder Diagram Unit
encoder _ = Diagram <<< foldlWithIndex
  (\nodeId acc node → acc <> "\n  " <> renderEntry nodeId node)
  "block"

renderEntry ∷ AudioNodeId → AudioNode → String
renderEntry nodeId node = case node of
  Oscillator { frequency, gain, wave } →
    Codec.encoder AudioNodeId.codec unit nodeId
      <> "[\"osc{f="
      <> Codec.encoder Frequency.codec unit frequency
      <> ",g="
      <> Codec.encoder Gain.codec unit gain
      <> ",w="
      <> Codec.encoder Wave.codec unit wave
      <> "}\"]"

audioNodeEntry ∷ Parser String (AudioNodeId /\ AudioNode)
audioNodeEntry = do
  id ← Codec.decoder AudioNodeId.codec
  skipSpaces

  _ ← char '['
  _ ← char '\"'

  _ ← string "osc"

  _ ← char '{'
  skipSpaces

  _ ← char 'f'
  _ ← char '='
  frequency ← Codec.decoder Frequency.codec
  skipSpaces

  _ ← char ','
  skipSpaces

  _ ← char 'g'
  _ ← char '='
  gain ← Codec.decoder Gain.codec
  skipSpaces

  _ ← char ','
  skipSpaces

  _ ← char 'w'
  _ ← char '='
  wave ← Codec.decoder Wave.codec
  skipSpaces

  _ ← char '}'
  _ ← char '\"'
  _ ← char ']'

  pure $ id /\ Oscillator { frequency, gain, wave }
