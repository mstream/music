module Music.View.Controls.Sequencer (view) where

import Prelude

import Data.Codec as Codec
import Data.Value (class Codeable, codecConf)
import Elmish.HTML.Styled as H
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Message (Message(..))
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.View.Controls.Sequencer.Duration (view) as Duration
import Music.View.Controls.Sequencer.Sequence (view) as Sequence
import Music.View.Controls.Sequencer.Types (SequencerControls)
import Music.View.Types (ViewModel)

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
    H.div_ "" { id }
      [ Duration.view
          (elementId "duration")
          ( \newValue → dispatch
              $ ControlsAdjusted model.id
                  { audioNode: model.ctor $ model.conf
                      { duration = newValue }
                  , isConnectedToOutput: false
                  }
          )
          model.conf.duration
      , Sequence.view
          model.name
          (\index → elementId "sequence-element-" <> show index)
          ( \newValue → dispatch
              $ ControlsAdjusted model.id
                  { audioNode: model.ctor $ model.conf
                      { sequence = newValue }
                  , isConnectedToOutput: false
                  }
          )
          model.fromSliderNumber
          (model.toSliderNumber <<< codecConf.unwrap)
          model.valueStringCodec
          model.conf.sequence
      ]
  where
  elementId ∷ String → String
  elementId suffix = model.name <> "-sequencer-"
    <> Codec.encoder BlockId.stringCodec unit
      (AudioNodeId.toBlockId model.id)
    <> "-"
    <>
      suffix
