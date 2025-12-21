module View (view) where

import Prelude

import Audio (createControls)
import Data.Foldable (foldl)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Elmish (Dispatch, ReactElement, (<|))
import Elmish.Dispatch (handleEffect)
import Elmish.HTML.Events (inputText)
import Elmish.HTML.Styled as H
import Message (Message(..))
import Model (InitializedModel, Model(..), PlaybackModel(..))
import Model.AudioNode (AudioNode)
import View.Code as Code

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
        [ H.label_ "" { htmlFor: "gain" } "gain"
        , H.text $ show model.gain
        , H.input_ ""
            { id: "gain"
            , min: "0"
            , max: "1"
            , onChange: dispatch <| \e → GainInputChanged (inputText e)
            , step: "0.01"
            , type: "range"
            }
        ]
    , H.div ""
        [ H.label_ "" { htmlFor: "frequency" } "frequency"
        , H.text $ show model.frequency
        , H.input_ "frequency"
            { min: "1.75"
            , max: "4.25"
            , onChange: dispatch <| \e → FrequencyInputChanged
                (inputText e)
            , step: "0.05"
            , type: "range"
            }
        ]
    , H.div ""
        [ H.text case model.playback of
            Starting → "starting"
            Started _ → "started"
            Stopped → "stopped"
        ]
    , H.canvas_ "" { height: "200px", id: "analyzer", width: "800px" }
        ""
    , code model.nodes
    , diagram model.nodesDiagramSvg
    ]

code ∷ List AudioNode → ReactElement
code nodes = H.pre ""
  [ H.code ""
      (foldl (\acc node → acc <> "\n" <> Code.render node) "" nodes)
  ]

diagram ∷ Maybe String → ReactElement
diagram = maybe
  (H.div "" [ H.text "not rendered" ])
  ( \html → H.div_ "" { dangerouslySetInnerHTML: { __html: html } }
      ([] ∷ Array ReactElement)
  )
