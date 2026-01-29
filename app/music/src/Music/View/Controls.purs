module Music.View.Controls (view) where

import Prelude

import Data.Codec as Codec
import Data.FunctorWithIndex (mapWithIndex)
import Data.Number as Number
import Elmish (ReactElement)
import Elmish.Dispatch ((<|))
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodeEntry)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency as Frequency
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.Model.Perspective (ControlsPerspective)
import Music.Model.Playback (Playback(..))
import Music.View.Components.Accordion as Accordion
import Music.View.Controls.Oscillator as Oscillator
import Music.View.Controls.Sequencer as Sequencer
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
  controls = Accordion.view showTitle items
    where
    showTitle ∷ AudioNodeId → String
    showTitle = Codec.encoder AudioNodeId.stringCodec unit

  items ∷ Accordion.Model AudioNodeId
  items = mapWithIndex
    ( \nodeId audioNodeEntry →
        { contents: renderItemContents nodeId audioNodeEntry
        , open: false
        }
    )
    (AudioNodes.nodesById model.audioNodes)

  renderItemContents ∷ AudioNodeId → AudioNodeEntry → ReactElement
  renderItemContents id { audioNode, isConnectedToOutput } = H.div ""
    [ viewContents dispatch ]
    where
    viewContents ∷ ViewVoid Message
    viewContents = case audioNode of
      FrequencySequencer conf →
        Sequencer.view
          { conf
          , ctor: FrequencySequencer
          , fromSliderNumber: Number.round <<< Number.exp
          , id
          , name: "frequency"
          , toSliderNumber: Number.log
          , valueStringCodec: Frequency.valueStringCodec
          }
      GainSequencer conf →
        Sequencer.view
          { conf
          , ctor: GainSequencer
          , fromSliderNumber: identity
          , id
          , name: "gain"
          , toSliderNumber: identity
          , valueStringCodec: Gain.valueStringCodec
          }
      Oscillator conf →
        Oscillator.view { conf, id, isConnectedToOutput }
