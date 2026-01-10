module Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator) where

import Music.Model.AudioNodes.AudioNode.Oscillator.Frequency (Frequency)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave)

type Oscillator =
  { frequency ∷ Frequency, gain ∷ Gain, wave ∷ Wave }

