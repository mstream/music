module Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer) where

import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)

type Sequencer a =
  { duration ∷ Duration, sequence ∷ Sequence a }
