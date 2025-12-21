module Message (Message(..)) where

import Effect.Timer (IntervalId)
import Model (Controls)

data Message
  = ControlsCreated Controls
  | DiagramRendered String
  | GainInputChanged String
  | FrequencyInputChanged String
  | PlayRequested
  | PlaybackStarted IntervalId
  | StopRequested
