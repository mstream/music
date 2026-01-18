module Music.View.Controls (view) where

import Prelude

import Data.Codec as Codec
import Data.FunctorWithIndex (mapWithIndex)
import Elmish (ReactElement)
import Elmish.Dispatch ((<|))
import Elmish.HTML.Styled as H
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Message (Message(..))
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Playback (Playback(..))
import Music.View.Components.Accordion as Accordion
import Music.View.Controls.FrequencySequencer as FrequencySequencer
import Music.View.Controls.GainSequencer as GainSequencer
import Music.View.Controls.Oscillator as Oscillator
import Music.View.Types (ViewModel, ViewVoid)

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
  controls = Accordion.view (Codec.encoder BlockId.stringCodec unit)
    items

  items ∷ Accordion.Model AudioNodeId
  items = mapWithIndex
    ( \nodeId node →
        { contents: renderItemContents nodeId node, open: false }
    )
    (AudioNodes.nodesById model.audioNodes)

  renderItemContents ∷ AudioNodeId → AudioNode → ReactElement
  renderItemContents id node = H.div "" [ viewContents dispatch ]
    where
    viewContents ∷ ViewVoid Message
    viewContents = case node of
      FrequencySequencer conf →
        FrequencySequencer.view { conf, id }
      GainSequencer conf →
        GainSequencer.view { conf, id }
      Oscillator conf →
        Oscillator.view { conf, id }
