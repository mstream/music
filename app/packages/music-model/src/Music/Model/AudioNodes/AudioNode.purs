module Music.Model.AudioNodes.AudioNode
  ( AudioNode(..)
  , frequencyBlockId
  , gainBlockId
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
import Data.Graph.NonEmpty as GraphNE
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockDef
  ( BlockDef(..)
  , Columns(..)
  , GroupBlock
  , Shape(..)
  )
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..))
import Music.Model.AudioNodes.AudioNode.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Note (Note)
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Parsing (ParseState(..), Parser, getParserT)
import Parsing (fail, runParser) as P
import Parsing.Combinators (between, choice) as P
import Parsing.String (char, string) as P
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data AudioNode
  = FrequencySequencer (Sequencer Frequency)
  | GainSequencer (Sequencer Gain)
  | NoteSequencer (Sequencer Note)
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
  , noteSequencerParser
  , oscillatorParser
  ]
  where
  frequencySequencerParser ∷ Parser String AudioNode
  frequencySequencerParser =
    audioNodeParser "fseq" FrequencySequencer do
      duration ← Codec.decoder Duration.parameterStringCodec
      void $ P.char ','
      sequence ← Codec.decoder Sequence.frequencyParameterStringCodec
      pure { duration, sequence }

  gainSequencerParser ∷ Parser String AudioNode
  gainSequencerParser =
    audioNodeParser "gseq" GainSequencer do
      duration ← Codec.decoder Duration.parameterStringCodec
      void $ P.char ','
      sequence ← Codec.decoder Sequence.gainParameterStringCodec
      pure { duration, sequence }

  noteSequencerParser ∷ Parser String AudioNode
  noteSequencerParser =
    audioNodeParser "nseq" NoteSequencer do
      duration ← Codec.decoder Duration.parameterStringCodec
      void $ P.char ','
      sequence ← Codec.decoder Sequence.noteParameterStringCodec
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
  NoteSequencer conf →
    renderNoteSequencer conf
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

  renderNoteSequencer ∷ Sequencer Note → String
  renderNoteSequencer conf =
    "nseq{"
      <> renderDuration conf.duration
      <> ","
      <> renderNoteSequence conf.sequence
      <> "}"

  renderDuration ∷ Duration → String
  renderDuration = Codec.encoder Duration.parameterStringCodec unit

  renderFrequencySequence ∷ Sequence Frequency → String
  renderFrequencySequence = Codec.encoder
    Sequence.frequencyParameterStringCodec
    unit

  renderGainSequence ∷ Sequence Gain → String
  renderGainSequence = Codec.encoder Sequence.gainParameterStringCodec
    unit

  renderNoteSequence ∷ Sequence Note → String
  renderNoteSequence = Codec.encoder Sequence.noteParameterStringCodec
    unit

  renderOscillator ∷ Oscillator → String
  renderOscillator conf = "osc{"
    <> Codec.encoder Wave.parameterStringCodec unit conf.wave
    <> "}"

groupBlockCodec ∷ Codec AudioNode GroupBlock BlockId
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
    decodeOscillator
      <|> decodeFrequencySequencer
      <|> decodeGainSequencer
      <|> decodeNoteSequencer
    where
    labels ∷ Array String
    labels = allLabels groupBlock
      where
      allLabels ∷ GroupBlock → Array String
      allLabels gb =
        GraphNE.vertices gb.children
          # Array.fromFoldable
          # Array.concatMap nodeLabels

      nodeLabels ∷ BlockDef → Array String
      nodeLabels = case _ of
        Node { contents } →
          [ contents ]
        Group nested →
          allLabels nested

    decodeOscillator ∷ String \/ AudioNode
    decodeOscillator = do
      wave ← decodeWith Wave.parameterStringCodec "wave" labels
      pure $ Oscillator { wave }

    decodeFrequencySequencer ∷ String \/ AudioNode
    decodeFrequencySequencer = do
      duration ← decodeWith Duration.parameterStringCodec "duration"
        labels
      sequence ← decodeWith Sequence.frequencyParameterStringCodec
        "frequency sequence"
        labels
      pure $ FrequencySequencer { duration, sequence }

    decodeGainSequencer ∷ String \/ AudioNode
    decodeGainSequencer = do
      duration ← decodeWith Duration.parameterStringCodec "duration"
        labels
      sequence ← decodeWith Sequence.gainParameterStringCodec
        "gain sequence"
        labels
      pure $ GainSequencer { duration, sequence }

    decodeNoteSequencer ∷ String \/ AudioNode
    decodeNoteSequencer = do
      duration ← decodeWith Duration.parameterStringCodec "duration"
        labels
      sequence ← decodeWith Sequence.noteParameterStringCodec
        "note sequence"
        labels
      pure $ NoteSequencer { duration, sequence }

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

groupBlockEncoder ∷ Encoder AudioNode GroupBlock BlockId
groupBlockEncoder parentBlockId = case _ of
  FrequencySequencer conf →
    sequencerGroupBlockEncoder Sequence.frequencyParameterStringCodec
      parentBlockId
      conf
  GainSequencer conf →
    sequencerGroupBlockEncoder Sequence.gainParameterStringCodec
      parentBlockId
      conf
  NoteSequencer conf →
    sequencerGroupBlockEncoder Sequence.noteParameterStringCodec
      parentBlockId
      conf
  Oscillator conf →
    oscillatorGroupBlockEncoder parentBlockId conf

sequencerGroupBlockEncoder
  ∷ ∀ a
  . Codec (Sequence a) String Unit
  → Encoder (Sequencer a) GroupBlock BlockId
sequencerGroupBlockEncoder sequenceStringCodec parentBlockId conf =
  { children: GraphNE.make dummyId
      (dummy /\ Set.empty)
      ( Graph.fromMap $ Map.fromFoldable
          [ parametersGroupId /\ (parametersGroup /\ Set.empty) ]
      )
  , properties: { columns: Just C1 }
  , spacedOut: false
  }
  where
  dummyId ∷ BlockId
  dummyId = parentBlockId <> dummyBlockId

  dummy ∷ BlockDef
  dummy = Node { contents: " ", shape: Rectangle }

  parametersGroupId ∷ BlockId
  parametersGroupId = parentBlockId <> parametersBlockId

  durationParameterId ∷ BlockId
  durationParameterId = parentBlockId <> durationBlockId

  sequenceParameterId ∷ BlockId
  sequenceParameterId = parentBlockId <> sequenceBlockId

  durationParameter ∷ BlockDef
  durationParameter = Node
    { contents: Codec.encoder Duration.parameterStringCodec
        unit
        conf.duration
    , shape: Rectangle
    }

  sequenceParameter ∷ BlockDef
  sequenceParameter = Node
    { contents: Codec.encoder sequenceStringCodec unit
        conf.sequence
    , shape: Rectangle
    }

  parametersGroup ∷ BlockDef
  parametersGroup = Group
    { children: GraphNE.make durationParameterId
        (durationParameter /\ Set.empty)
        ( Graph.fromMap $ Map.fromFoldable
            [ sequenceParameterId /\ (sequenceParameter /\ Set.empty) ]
        )
    , properties: { columns: Just C2 }
    , spacedOut: false
    }

oscillatorGroupBlockEncoder ∷ Encoder Oscillator GroupBlock BlockId
oscillatorGroupBlockEncoder parentBlockId conf =
  { children:
      GraphNE.make inputsGroupId
        (inputsGroup /\ Set.empty)
        ( Graph.fromMap $ Map.fromFoldable
            [ parametersGroupId /\ (parametersGroup /\ Set.empty) ]
        )
  , properties: { columns: Just C1 }
  , spacedOut: false
  }
  where
  inputsGroupId ∷ BlockId
  inputsGroupId = parentBlockId <> inputsBlockId

  inputsGroup ∷ BlockDef
  inputsGroup = Group
    { children: GraphNE.make frequencyInputId
        (frequencyInput /\ Set.empty)
        ( Graph.fromMap $ Map.fromFoldable
            [ gainInputId /\ (gainInput /\ Set.empty) ]
        )
    , properties: { columns: Just C2 }
    , spacedOut: false
    }

  parametersGroupId ∷ BlockId
  parametersGroupId = parentBlockId <>
    parametersBlockId

  parametersGroup ∷ BlockDef
  parametersGroup = Group
    { children: GraphNE.singleton waveParameterId
        (waveParameter /\ Set.empty)
    , properties: { columns: Just C1 }
    , spacedOut: false
    }

  frequencyInputId ∷ BlockId
  frequencyInputId = parentBlockId <> frequencyBlockId

  gainInputId ∷ BlockId
  gainInputId = parentBlockId <> gainBlockId

  waveParameterId ∷ BlockId
  waveParameterId = parentBlockId <> waveBlockId

  frequencyInput ∷ BlockDef
  frequencyInput = Node { contents: "f", shape: Rectangle }

  gainInput ∷ BlockDef
  gainInput = Node { contents: "g", shape: Rectangle }

  waveParameter ∷ BlockDef
  waveParameter = Node
    { contents: Codec.encoder Wave.parameterStringCodec unit
        conf.wave
    , shape: Rectangle
    }

dummyBlockId ∷ BlockId
dummyBlockId = BlockId.make D [ U, M, M, Y ] []

durationBlockId ∷ BlockId
durationBlockId = BlockId.make D [ U, R, A, T, I, O, N ] []

frequencyBlockId ∷ BlockId
frequencyBlockId = BlockId.make F
  [ R, E, Q, U, E, N, C, Y ]
  []

gainBlockId ∷ BlockId
gainBlockId = BlockId.make G [ A, I, N ] []

inputsBlockId ∷ BlockId
inputsBlockId = BlockId.make I [ N, P, U, T, S ] []

parametersBlockId ∷ BlockId
parametersBlockId = BlockId.make P [ A, R, A, M, E, T, E, R, S ] []

sequenceBlockId ∷ BlockId
sequenceBlockId = BlockId.make S [ E, Q, U, E, N, C, E ] []

waveBlockId ∷ BlockId
waveBlockId = BlockId.make W [ A, V, E ] []

