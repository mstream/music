module Music.View.Controls.Sequencer.Types (SequencerControls) where

import Prelude

import Data.Codec (Codec)
import Music.Model.AudioNodes.AudioNode (AudioNode)
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)

type SequencerControls a =
  { conf ∷ Sequencer a
  , ctor ∷ Sequencer a → AudioNode
  , fromSliderNumber ∷ Number → Number
  , id ∷ AudioNodeId
  , name ∷ String
  , toSliderNumber ∷ Number → Number
  , valueStringCodec ∷ Codec a String Unit
  }
