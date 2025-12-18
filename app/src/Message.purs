module Message (Message(..)) where

import Effect.Timer (IntervalId)
import Model (Controls)

data Message
  = ControlsCreated Controls
  | GainInputChanged String
  | FrequencyInputChanged String
  | PlayRequested
  | PlaybackStarted IntervalId
  | StopRequested
