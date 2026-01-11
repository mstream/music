module Music.View.Controls.Oscillator (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.HTML.Events
  ( InputChangeEvent
  , SelectChangeEvent
  )
import Elmish.HTML.Events (handleEffect, inputText, selectSelectedValue) as E
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.View.Types (ViewModel)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

type OscillatorControls = { conf ∷ Oscillator, id ∷ AudioNodeId }

view ∷ ViewModel OscillatorControls Message
view model dispatch =
  let
    id ∷ String
    id = elementId "controls"
  in
    H.div_ "" { id } [ gainElement, frequencyElement, waveElement ]
  where
  gainElement ∷ ReactElement
  gainElement = H.div ""
    [ H.label_ "" { htmlFor: id } "gain"
    , H.text $ Codec.encoder Gain.stringCodec unit model.conf.gain
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
        Console.error $ "Invalid gain input: " <>
          P.parseErrorMessage parseError
      Right newGain →
        dispatch $ ControlsAdjusted model.id
          (Oscillator model.conf { gain = newGain })
      where
      parsingResult ∷ ParseError \/ Gain
      parsingResult = P.runParser
        (E.inputText event)
        (Codec.decoder Gain.stringCodec)

    step ∷ Number
    step = (maxValue - minValue) / 100.0

    maxValue ∷ Number
    maxValue = Gain.toNumber top

    minValue ∷ Number
    minValue = Gain.toNumber bottom

    currentValue ∷ Number
    currentValue = Gain.toNumber model.conf.gain

    id ∷ String
    id = elementId "gain"

  frequencyElement ∷ ReactElement
  frequencyElement = H.div ""
    [ H.label_ "" { htmlFor: id } "frequency"
    , H.text $ Codec.encoder
        Frequency.stringCodec
        unit
        model.conf.frequency
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
          (Oscillator model.conf { frequency = newFrequency })
      where
      parsingResult ∷ ParseError \/ Frequency
      parsingResult = P.runParser
        ( case Number.fromString $ E.inputText event of
            Just x →
              show $ Number.round $ Number.exp x
            Nothing →
              (E.inputText event)
        )
        (Codec.decoder Frequency.stringCodec)

    step ∷ Number
    step = (maxValue - minValue) / 100.0

    maxValue ∷ Number
    maxValue = Number.log $ Frequency.toNumber top

    minValue ∷ Number
    minValue = Number.log $ Frequency.toNumber bottom

    currentValue ∷ Number
    currentValue = Number.log $ Frequency.toNumber model.conf.frequency

    id ∷ String
    id = elementId "frequency"

  waveElement ∷ ReactElement
  waveElement = H.div ""
    [ H.label_ "" { htmlFor: id } "wave shape"
    , H.select_ "" { id, onChange: E.handleEffect onChangeHandler }
        [ option Sine
        , option Square
        ]
    ]
    where
    onChangeHandler ∷ SelectChangeEvent → Effect Unit
    onChangeHandler event = case parsingResult of
      Left parseError →
        Console.error $ "Invalid wave input: " <>
          P.parseErrorMessage parseError
      Right newWave →
        dispatch $ ControlsAdjusted model.id
          (Oscillator model.conf { wave = newWave })
      where
      parsingResult ∷ ParseError \/ Wave
      parsingResult = P.runParser
        (E.selectSelectedValue event)
        (Codec.decoder Wave.stringCodec)

    option ∷ Wave → ReactElement
    option = H.option "" <<< Codec.encoder Wave.stringCodec unit

    id ∷ String
    id = elementId "wave"

  elementId ∷ String → String
  elementId suffix = "oscillator-"
    <> Codec.encoder AudioNodeId.codec unit model.id
    <> "-"
    <>
      suffix
