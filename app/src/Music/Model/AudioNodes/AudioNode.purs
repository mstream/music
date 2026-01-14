module Music.Model.AudioNodes.AudioNode
  ( AudioNode(..)
  , groupBlockCodec
  , stringCodec
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Codec (Codec, Decoder, Encoder)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.Graph as Graph
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Parsing (ParseState(..), Parser, getParserT)
import Parsing (fail, runParser) as P
import Parsing.Combinators (between, choice) as P
import Parsing.String (char, string) as P
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data AudioNode
  = FrequencySequencer (Sequencer Frequency)
  | GainSequencer (Sequencer Gain)
  | Oscillator Oscillator

derive instance Eq AudioNode
derive instance Generic AudioNode _
derive instance Ord AudioNode

instance Arbitrary AudioNode where
  arbitrary = genericArbitrary

instance Show AudioNode where
  show = genericShow

stringCodec ∷ Codec AudioNode String Unit
stringCodec = Codec.codec stringDecoder stringEncoder

stringDecoder ∷ Decoder AudioNode String
stringDecoder = P.choice
  [ frequencySequencerParser
  , gainSequencerParser
  , oscillatorParser
  ]
  where
  frequencySequencerParser ∷ Parser String AudioNode
  frequencySequencerParser =
    audioNodeParser "fseq" FrequencySequencer do
      duration ← Codec.decoder Duration.stringCodec
      void $ P.char ','
      sequence ← Codec.decoder Sequence.frequencyStringCodec
      pure { duration, sequence }

  gainSequencerParser ∷ Parser String AudioNode
  gainSequencerParser =
    audioNodeParser "gseq" GainSequencer do
      duration ← Codec.decoder Duration.stringCodec
      void $ P.char ','
      sequence ← Codec.decoder Sequence.gainStringCodec
      pure { duration, sequence }

  oscillatorParser ∷ Parser String AudioNode
  oscillatorParser =
    audioNodeParser "osc" Oscillator do
      wave ← Codec.decoder Wave.parameterStringCodec
      pure { wave }

  audioNodeParser
    ∷ ∀ a
    . String
    → (a → AudioNode)
    → Parser String a
    → Parser String AudioNode
  audioNodeParser name ctor confParser = ctor <$> P.between
    (P.string $ name <> "{")
    (P.string "}")
    confParser

stringEncoder ∷ Encoder AudioNode String Unit
stringEncoder _ = case _ of
  FrequencySequencer conf →
    renderFrequencySequencer conf
  GainSequencer conf →
    renderGainSequencer conf
  Oscillator conf →
    renderOscillator conf
  where
  renderFrequencySequencer ∷ Sequencer Frequency → String
  renderFrequencySequencer conf =
    "fseq{"
      <> renderDuration conf.duration
      <> ","
      <> renderFrequencySequence conf.sequence
      <> "}"

  renderGainSequencer ∷ Sequencer Gain → String
  renderGainSequencer conf =
    "gseq{"
      <> renderDuration conf.duration
      <> ","
      <> renderGainSequence conf.sequence
      <> "}"

  renderDuration ∷ Duration → String
  renderDuration = Codec.encoder Duration.stringCodec unit

  renderFrequencySequence ∷ Sequence Frequency → String
  renderFrequencySequence = Codec.encoder Sequence.frequencyStringCodec
    unit

  renderGainSequence ∷ Sequence Gain → String
  renderGainSequence = Codec.encoder Sequence.gainStringCodec unit

  renderOscillator ∷ Oscillator → String
  renderOscillator conf = "osc{"
    <> Codec.encoder Wave.parameterStringCodec unit conf.wave
    <> "}"

groupBlockCodec ∷ Codec AudioNode GroupBlock AudioNodeId
groupBlockCodec = Codec.codec groupBlockDecoder groupBlockEncoder

groupBlockDecoder ∷ Decoder AudioNode GroupBlock
groupBlockDecoder = do
  ParseState input _ _ ← getParserT
  case decodeGroupBlock input of
    Left err →
      P.fail err
    Right audioNode →
      pure audioNode
  where
  decodeGroupBlock ∷ GroupBlock → String \/ AudioNode
  decodeGroupBlock groupBlock =
    decodeOscillator <|> decodeFrequencySequencer <|>
      decodeGainSequencer
    where
    labels ∷ Array String
    labels =
      Graph.vertices groupBlock.children
        # Array.fromFoldable
        # Array.mapMaybe nodeLabel

    decodeOscillator ∷ String \/ AudioNode
    decodeOscillator = do
      wave ← decodeWith Wave.parameterStringCodec "wave" labels
      pure $ Oscillator { wave }

    decodeFrequencySequencer ∷ String \/ AudioNode
    decodeFrequencySequencer = do
      duration ← decodeWith Duration.stringCodec "duration" labels
      sequence ← decodeWith Sequence.frequencyStringCodec
        "frequency sequence"
        labels
      pure $ FrequencySequencer { duration, sequence }

    decodeGainSequencer ∷ String \/ AudioNode
    decodeGainSequencer = do
      duration ← decodeWith Duration.stringCodec "duration" labels
      sequence ← decodeWith Sequence.gainStringCodec
        "gain sequence"
        labels
      pure $ GainSequencer { duration, sequence }

    nodeLabel ∷ BlockDef → Maybe String
    nodeLabel = case _ of
      Node label →
        Just label
      Group _ →
        Nothing

  decodeWith
    ∷ ∀ a
    . Codec a String Unit
    → String
    → Array String
    → String \/ a
  decodeWith codec label labels =
    case Array.findMap (run codec) labels of
      Just value →
        Right value
      Nothing →
        Left $ "Missing " <> label
    where
    run ∷ Codec a String Unit → String → Maybe a
    run c s = case P.runParser s (Codec.decoder c) of
      Left _ →
        Nothing
      Right value →
        Just value

groupBlockEncoder ∷ Encoder AudioNode GroupBlock AudioNodeId
groupBlockEncoder nodeId = case _ of
  FrequencySequencer conf →
    sequencerGroupBlockEncoder Sequence.frequencyStringCodec nodeId conf
  GainSequencer conf →
    sequencerGroupBlockEncoder Sequence.gainStringCodec nodeId conf
  Oscillator conf →
    oscillatorGroupBlockEncoder nodeId conf

sequencerGroupBlockEncoder
  ∷ ∀ a
  . Codec (Sequence a) String Unit
  → Encoder (Sequencer a) GroupBlock AudioNodeId
sequencerGroupBlockEncoder sequenceStringCodec nodeId conf =
  { children: Graph.fromMap $ Map.fromFoldable
      [ durationParameterId /\ (durationParameter /\ Nil)
      , sequenceParameterId /\ (sequenceParameter /\ Nil)
      ]
  , properties: { columns: Just C2 }
  , spacedOut: false
  }
  where
  durationParameterId ∷ BlockId
  durationParameterId = nodeId <> AudioNodeId.duration

  sequenceParameterId ∷ BlockId
  sequenceParameterId = nodeId <> AudioNodeId.sequence

  durationParameter ∷ BlockDef
  durationParameter = Node $ Codec.encoder Duration.stringCodec unit
    conf.duration

  sequenceParameter ∷ BlockDef
  sequenceParameter = Node $ Codec.encoder sequenceStringCodec unit
    conf.sequence

oscillatorGroupBlockEncoder ∷ Encoder Oscillator GroupBlock AudioNodeId
oscillatorGroupBlockEncoder nodeId conf =
  { children: Graph.fromMap $ Map.fromFoldable
      [ frequencyInputId /\ (frequencyInput /\ Nil)
      , gainInputId /\ (gainInput /\ Nil)
      , waveParameterId /\ (waveParameter /\ Nil)
      ]
  , properties: { columns: Just C2 }
  , spacedOut: false
  }
  where
  frequencyInputId ∷ BlockId
  frequencyInputId = nodeId <> AudioNodeId.frequency

  gainInputId ∷ BlockId
  gainInputId = nodeId <> AudioNodeId.gain

  waveParameterId ∷ BlockId
  waveParameterId = nodeId <> AudioNodeId.wave

  frequencyInput ∷ BlockDef
  frequencyInput = Node "f"

  gainInput ∷ BlockDef
  gainInput = Node "g"

  waveParameter ∷ BlockDef
  waveParameter = Node $ Codec.encoder Wave.parameterStringCodec unit
    conf.wave
