module View (view) where

import Prelude

import Audio (createControls)
import Data.Array (fromFoldable)
import Data.Code as Code
import Data.Codec (encoder)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List, reverse, singleton)
import Data.Maybe (Maybe, maybe)
import Elmish (Dispatch, ReactElement, (<|))
import Elmish.Dispatch (handleEffect)
import Elmish.HTML.Events (inputText)
import Elmish.HTML.Styled as H
import Message (Message(..))
import Model (InitializedModel, Model(..), PlaybackModel(..))
import Model.AudioNode
  ( AudioNode(..)
  , AudioNodeId
  , AudioNodes
  , OscillatorConf
  )

type ViewVoid = Dispatch Message → ReactElement
type View m = m → Dispatch Message → ReactElement

view ∷ View Model
view model dispatch = go dispatch
  where
  go ∷ ViewVoid
  go = case model of
    Initialized initializedModel →
      viewInitialized initializedModel
    Uninitialized →
      viewUninitialized

viewUninitialized ∷ ViewVoid
viewUninitialized dispatch =
  H.div "" [ H.button_ "" { onClick } "Start" ]
  where
  onClick = handleEffect \_ → do
    ctrls ← createControls
    dispatch $ ControlsCreated ctrls

viewInitialized ∷ View InitializedModel
viewInitialized model dispatch =
  H.div ""
    [ H.button_ "" { onClick: dispatch <| PlayRequested }
        "Play"
    , H.button_ "" { onClick: dispatch <| StopRequested } "Stop"
    , H.div ""
        [ H.text case model.playback of
            Starting →
              "starting"
            Started _ →
              "started"
            Stopped →
              "stopped"
        ]
    , H.canvas_ "" { height: "200px", id: "analyzer", width: "800px" }
        ""
    , code model.nodes
    , diagram model.nodesDiagramSvg
    , viewControls model.nodes dispatch
    ]

code ∷ AudioNodes → ReactElement
code nodes = H.pre ""
  [ H.code "" (encoder Code.codec unit nodes) ]

diagram ∷ Maybe String → ReactElement
diagram = maybe
  (H.div "" [ H.text "not rendered" ])
  ( \html → H.div_ "" { dangerouslySetInnerHTML: { __html: html } }
      ([] ∷ Array ReactElement)
  )

viewControls ∷ View AudioNodes
viewControls nodes dispatch =
  H.div "" (fromFoldable $ reverse $ renderElements nodes)
  where
  renderElements ∷ AudioNodes → List ReactElement
  renderElements = foldMapWithIndex
    (\nodeId → singleton <<< renderElement nodeId)

  renderElement ∷ AudioNodeId → AudioNode → ReactElement
  renderElement nodeId node = case node of
    Oscillator conf →
      H.div ""
        [ renderOscillator nodeId conf
        ]

  renderOscillator ∷ AudioNodeId → OscillatorConf → ReactElement
  renderOscillator oscillatorId { frequency, gain, wave } =
    let
      id ∷ String
      id = elementId "controls"
    in
      H.div ""
        [ H.label_ "" { htmlFor: id } (show oscillatorId)
        , H.div_ "" { id } [ gainElement, frequencyElement ]
        ]
    where
    gainElement ∷ ReactElement
    gainElement =
      let
        id ∷ String
        id = elementId "gain"
      in
          H.div ""
            [ H.label_ "" { htmlFor: id } "gain"
            , H.text $ show gain
            , H.input_ ""
              { id
              , min: "0"
              , max: "1"
              , onChange: dispatch <| \e → GainInputChanged
                  (inputText e)
              , step: "0.01"
              , type: "range"
              }
          ]

    frequencyElement ∷ ReactElement
    frequencyElement =
      let
        id ∷ String
        id = elementId "frequency"
      in
        H.div ""
          [ H.label_ "" { htmlFor: id } "frequency"
          , H.text $ show frequency
          , H.input_ ""
            { id
            , min: "1.75"
            , max: "4.25"
            , onChange: dispatch <| \e → FrequencyInputChanged
                (inputText e)
            , step: "0.05"
            , type: "range"
            }
          , H.text $ "wave: " <> show wave
          ]

    elementId ∷ String → String
    elementId suffix = "oscillator-" <> show oscillatorId <> "-" <>
      suffix
