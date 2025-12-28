module Model.AudioNodes.Codec.Code (codec) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons)
import Data.Codec as Codec
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (fromFoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Model.AudioNodeId (AudioNodeId)
import Model.AudioNodeId as AudioNodeId
import Model.AudioNodes (AudioNode(..))
import Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  )
import Model.AudioNodes.Frequency as Frequency
import Model.AudioNodes.Gain as Gain
import Model.AudioNodes.Wave as Wave
import Parsing (Parser)
import Parsing.String (char, string)
import Parsing.String.Basic (skipSpaces)

codec ∷ AudioNodesCodec String Unit
codec = Codec.codec decoder encoder

decoder ∷ AudioNodesDecoder String
decoder = do
  first ← audioNodeEntry
  rest ← manyRest
  pure $ fromFoldable (cons first rest)
  where
  manyRest =
    ( do
        node ← restAudioNode
        more ← manyRest
        pure $ cons node more
    ) <|> pure []

  restAudioNode = char '\n' *> skipSpaces *> audioNodeEntry

encoder ∷ AudioNodesEncoder String Unit
encoder _ =
  foldlWithIndex renderLine ""
  where
  renderLine nodeId acc node =
    let
      rendered = renderEntry (nodeId /\ node)
    in
      if acc == "" then rendered else acc <> "\n" <> rendered

renderEntry ∷ AudioNodeId /\ AudioNode → String
renderEntry (nodeId /\ node) = case node of
  Oscillator { frequency, gain, wave } →
    Codec.encoder AudioNodeId.codec unit nodeId <> " osc"
      <> "{f="
      <> Codec.encoder Frequency.codec unit frequency
      <> ",g="
      <> Codec.encoder Gain.codec unit gain
      <> ",w="
      <> Codec.encoder Wave.codec unit wave
      <> "}"

audioNodeEntry ∷ Parser String (AudioNodeId /\ AudioNode)
audioNodeEntry = do
  id ← Codec.decoder AudioNodeId.codec
  skipSpaces

  _ ← string "osc"
  skipSpaces

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

  pure $ id /\ Oscillator { frequency, gain, wave }

