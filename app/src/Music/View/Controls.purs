module Music.View.Controls (view) where

import Data.FunctorWithIndex (mapWithIndex)
import Elmish (ReactElement)
import Elmish.Dispatch ((<|))
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNode(..))
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Playback (Playback(..))
import Music.View.Components.Accordion as Accordion
import Music.View.Controls.Oscillator as Oscillator
import Music.View.Types (ViewModel)

view ∷ ViewModel ControlsPerspective Message
view model dispatch =
  H.div ""
    [ H.div_ "" { id: "analyser" } [ H.text "" ]
    , H.div_ "" { role: "group" } [ playButton, stopButton ]
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

  items ∷ Accordion.Model AudioNodeId
  items = mapWithIndex
    ( \nodeId node →
        { contents: renderItemContents nodeId node, open: false }
    )
    (AudioNodes.nodesById model.audioNodes)

  renderItemContents ∷ AudioNodeId → AudioNode → ReactElement
  renderItemContents id = case _ of
    Oscillator conf →
      H.div ""
        [ Oscillator.view { conf, id } dispatch ]
