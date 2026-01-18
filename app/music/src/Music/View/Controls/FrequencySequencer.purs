module Music.View.Controls.FrequencySequencer (view) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.HTML.Events (InputChangeEvent)
import Elmish.HTML.Events (handleEffect, inputText) as E
import Elmish.HTML.Styled as H
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Message (Message(..))
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( Sequence(..)
  )
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.View.Types (ViewModel)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

type FrequencySequencerControls =
  { conf ∷ Sequencer Frequency, id ∷ AudioNodeId }

view ∷ ViewModel FrequencySequencerControls Message
view model dispatch =
  let
    id ∷ String
    id = elementId "controls"
  in
    H.div_ "" { id } [ frequencyElement ]
  where
  frequencyElement ∷ ReactElement
  frequencyElement = H.div ""
    [ H.label_ "" { htmlFor: id } "frequency"
    , H.text $ Codec.encoder
        Frequency.valueStringCodec
        unit
        (ArrayNE.head $ Sequence.toArray model.conf.sequence)
    , H.input_ ""
        { id
        , min: show minValue
        , max: show maxValue
        , onChange: E.handleEffect onChangeHandler
        , step: show step
        , type: "range"
        , value: show currentValue
        }
    ]
    where
    onChangeHandler ∷ InputChangeEvent → Effect Unit
    onChangeHandler event = case parsingResult of
      Left parseError →
        Console.error $ "Invalid frequency input: " <>
          P.parseErrorMessage parseError
      Right newFrequency →
        dispatch $ ControlsAdjusted model.id
          ( FrequencySequencer model.conf
              { sequence = Sequence $ ArrayNE.singleton newFrequency }
          )
      where
      parsingResult ∷ ParseError \/ Frequency
      parsingResult = P.runParser
        ( case Number.fromString $ E.inputText event of
            Just x →
              show $ Number.round $ Number.exp x
            Nothing →
              (E.inputText event)
        )
        (Codec.decoder Frequency.valueStringCodec)

    step ∷ Number
    step = (maxValue - minValue) / 100.0

    maxValue ∷ Number
    maxValue = Number.log $ Frequency.toNumber top

    minValue ∷ Number
    minValue = Number.log $ Frequency.toNumber bottom

    currentValue ∷ Number
    currentValue = Number.log $ Frequency.toNumber $ ArrayNE.head $
      Sequence.toArray model.conf.sequence

    id ∷ String
    id = elementId "frequency"

  elementId ∷ String → String
  elementId suffix = "frequency-sequencer-"
    <> Codec.encoder BlockId.stringCodec unit model.id
    <> "-"
    <>
      suffix
