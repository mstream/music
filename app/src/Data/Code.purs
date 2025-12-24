module Data.Code (codec) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons)
import Data.CodePoint.Unicode (isAlphaNum)
import Data.Codec as Codec
import Data.Codec.AudioNodes
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (fromFoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Model.AudioNode
  ( AudioNode(..)
  , AudioNodeId
  , Wave(..)
  , audioNodeId
  , unAudioNodeId
  )
import Parsing (Parser, fail)
import Parsing.String (char, string)
import Parsing.String.Basic (number, skipSpaces, takeWhile1)

codec ∷ AudioNodesCodec Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder
decoder = do
  first ← audioNode
  rest ← manyRest
  pure $ fromFoldable (cons first rest)
  where
  manyRest =
    ( do
        node ← restAudioNode
        more ← manyRest
        pure $ cons node more
    ) <|> pure []

  restAudioNode = char '\n' *> skipSpaces *> audioNode

encoder ∷ AudioNodesEncoder Unit
encoder _ =
  foldlWithIndex renderLine ""
  where
  renderLine nodeId acc node =
    let
      rendered = render (nodeId /\ node)
    in
      if acc == "" then rendered else acc <> "\n" <> rendered

render ∷ AudioNodeId /\ AudioNode → String
render (nodeId /\ node) = case node of
  Oscillator { frequency, gain, wave } →
    unAudioNodeId nodeId <> " osc"
      <> "{f=" <> show frequency
      <> ",g=" <> show gain
      <> ",w=" <> renderWave wave <> "}"

audioNode ∷ Parser String (AudioNodeId /\ AudioNode)
audioNode = do
  id ← audioNodeId
  skipSpaces

  _ ← string "osc"
  skipSpaces

  _ ← char '{'
  skipSpaces

  _ ← char 'f'
  _ ← char '='
  frequency ← number
  skipSpaces

  _ ← char ','
  skipSpaces

  _ ← char 'g'
  _ ← char '='
  gain ← number
  skipSpaces

  _ ← char ','
  skipSpaces

  _ ← char 'w'
  _ ← char '='
  wave ← parseWave
  skipSpaces

  _ ← char '}'

  pure $ id /\ Oscillator { frequency, gain, wave }

renderWave ∷ Wave → String
renderWave wave =
  case wave of
    Sine →
      "sine"
    Square →
      "square"

parseWave ∷ Parser String Wave
parseWave = do
  waveText ← takeWhile1 isAlphaNum
  case waveText of
    "sine" →
      pure Sine
    "square" →
      pure Square
    _ →
      fail $ "Unrecognized wave type: " <> waveText
