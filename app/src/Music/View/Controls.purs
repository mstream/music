module View.Controls (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.Dispatch (handleEffect, (<|))
import Elmish.HTML.Events (InputChangeEvent, inputText)
import Elmish.HTML.Styled as H
import Message (Message(..))
import Model.AudioNodeId (AudioNodeId)
import Model.AudioNodeId as AudioNodeId
import Model.AudioNodes (AudioNode(..), OscillatorConf)
import Model.AudioNodes.Frequency as Frequency
import Model.AudioNodes.Gain as Gain
import Model.Perspective (ControlsPerspective)
import Model.Playback (Playback(..))
import Parsing (parseErrorMessage, runParser)
import View.Components.Accordion as Accordion
import View.Types (ViewModel)

view ∷ ViewModel ControlsPerspective
view model dispatch =
  H.div ""
    [ H.div_ "" { role: "group" } [ playButton, stopButton ]
    , controls
    ]
  where
  playButton ∷ ReactElement
  playButton = case model.playback of
    Stopped →
      H.button_ "" { onClick: dispatch <| PlayRequested } "Play"
    _ →
      H.button_ "secondary" {} "Play"

  stopButton ∷ ReactElement
  stopButton = case model.playback of
    Playing _ →
      H.button_ "" { onClick: dispatch <| StopRequested }
        "Stop"

    _ →
      H.button_ "secondary" {} "Stop"

  controls ∷ ReactElement
  controls = Accordion.view items

  items ∷ Accordion.Items AudioNodeId
  items = mapWithIndex
    ( \nodeId node →
        { contents: renderItemContents nodeId node, open: false }
    )
    model.audioNodes

  renderItemContents ∷ AudioNodeId → AudioNode → ReactElement
  renderItemContents nodeId node = case node of
    Oscillator conf →
      H.div ""
        [ renderOscillator nodeId conf
        ]

  renderOscillator ∷ AudioNodeId → OscillatorConf → ReactElement
  renderOscillator oscillatorId conf =
    let
      id ∷ String
      id = elementId "controls"
    in
      H.div_ "" { id } [ gainElement, frequencyElement ]
    where
    gainElement ∷ ReactElement
    gainElement =
      let
        id ∷ String
        id = elementId "gain"

        onChangeHandler ∷ InputChangeEvent → Effect Unit
        onChangeHandler onChangeEvent =
          case
            runParser (inputText onChangeEvent)
              (Codec.decoder Gain.htmlInputCodec)
            of
            Left parseError →
              Console.error $ "Invalid gain input: " <>
                parseErrorMessage parseError
            Right newGain →
              dispatch $ ControlsAdjusted oscillatorId
                (Oscillator conf { gain = newGain })
      in
        H.div ""
          [ H.label_ "" { htmlFor: id } "gain"
          , H.text $ show conf.gain
          , H.input_ ""
              { id
              , min: "0"
              , max: "1"
              , onChange: handleEffect onChangeHandler
              , step: "0.01"
              , type: "range"
              }
          ]

    frequencyElement ∷ ReactElement
    frequencyElement =
      let
        id ∷ String
        id = elementId "frequency"

        onChangeHandler ∷ InputChangeEvent → Effect Unit
        onChangeHandler onChangeEvent =
          case
            runParser (inputText onChangeEvent)
              (Codec.decoder Frequency.htmlInputCodec)
            of
            Left parseError →
              Console.error $ "Invalid frequency input: " <>
                parseErrorMessage parseError
            Right newFrequency →
              dispatch $ ControlsAdjusted oscillatorId
                (Oscillator conf { frequency = newFrequency })

      in
        H.div ""
          [ H.label_ "" { htmlFor: id } "frequency"
          , H.text $ show conf.frequency
          , H.input_ ""
              { id
              , min: "1.75"
              , max: "4.25"
              , onChange: handleEffect onChangeHandler
              , step: "0.05"
              , type: "range"
              }
          , H.text $ "wave: " <> show conf.wave
          ]

    elementId ∷ String → String
    elementId suffix = "oscillator-"
      <> Codec.encoder AudioNodeId.codec unit oscillatorId
      <> "-"
      <>
        suffix
