module Music.View.Controls.Sequencer (view) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Value (class Codeable, codecConf)
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.HTML.Events (InputChangeEvent)
import Elmish.HTML.Events (handleEffect, inputText) as E
import Elmish.HTML.Styled as H
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Message (Message(..))
import Music.Model.AudioNodes.AudioNode (AudioNode)
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( Sequence(..)
  )
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.View.Types (ViewModel)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

type SequencerControls a =
  { conf ∷ Sequencer a
  , ctor ∷ Sequencer a → AudioNode
  , fromSliderNumber ∷ Number → Number
  , id ∷ AudioNodeId
  , name ∷ String
  , toSliderNumber ∷ Number → Number
  , valueStringCodec ∷ Codec a String Unit
  }

view
  ∷ ∀ a
  . Bounded a
  ⇒ Codeable a Number Unit
  ⇒ Show a
  ⇒ ViewModel (SequencerControls a) Message
view model dispatch =
  let
    id ∷ String
    id = elementId "controls"
  in
    H.div_ "" { id } [ durationElement, sequenceElement ]
  where
  durationElement ∷ ReactElement
  durationElement = H.div ""
    [ H.label_ "" { htmlFor: id } "duartion"
    , H.text $ Codec.encoder Duration.valueStringCodec unit
        model.conf.duration
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
        Console.error $ "Invalid duration input: " <>
          P.parseErrorMessage parseError
      Right newValue →
        dispatch $ ControlsAdjusted model.id
          { audioNode: model.ctor $ model.conf
              { duration = newValue }
          , isConnectedToOutput: false
          }
      where
      parsingResult ∷ ParseError \/ Duration
      parsingResult = P.runParser
        ( case Number.fromString $ E.inputText event of
            Just x →
              show $ Int.round x
            Nothing →
              (E.inputText event)
        )
        (Codec.decoder Duration.valueStringCodec)

    step ∷ Number
    step = 1.0

    maxValue ∷ Number
    maxValue = toSliderNumber top

    minValue ∷ Number
    minValue = toSliderNumber bottom

    currentValue ∷ Number
    currentValue = toSliderNumber model.conf.duration

    toSliderNumber ∷ Duration → Number
    toSliderNumber = Int.toNumber <<< codecConf.unwrap

    id ∷ String
    id = elementId "duration"

  sequenceElement ∷ ReactElement
  sequenceElement = H.div ""
    [ H.label_ "" { htmlFor: id } model.name
    , H.text $ Codec.encoder
        model.valueStringCodec
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
      Right newValue →
        dispatch $ ControlsAdjusted model.id
          { audioNode: model.ctor $ model.conf
              { sequence = Sequence $ ArrayNE.singleton newValue }
          , isConnectedToOutput: false
          }
      where
      parsingResult ∷ ParseError \/ a
      parsingResult = P.runParser
        ( case Number.fromString $ E.inputText event of
            Just x →
              show $ model.fromSliderNumber x
            Nothing →
              (E.inputText event)
        )
        (Codec.decoder model.valueStringCodec)

    step ∷ Number
    step = (maxValue - minValue) / 100.0

    maxValue ∷ Number
    maxValue = toSliderNumber top

    minValue ∷ Number
    minValue = toSliderNumber bottom

    currentValue ∷ Number
    currentValue = toSliderNumber $ ArrayNE.head $
      Sequence.toArray model.conf.sequence

    id ∷ String
    id = elementId model.name

    toSliderNumber ∷ a → Number
    toSliderNumber = model.toSliderNumber <<< codecConf.unwrap

  elementId ∷ String → String
  elementId suffix = model.name <> "-sequencer-"
    <> Codec.encoder BlockId.stringCodec unit
      (AudioNodeId.toBlockId model.id)
    <> "-"
    <>
      suffix
