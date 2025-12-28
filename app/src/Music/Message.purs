module Message (Message(..)) where

import Audio.WebAudio.Types (AudioContext)
import Model.AudioNodeId (AudioNodeId)
import Model.AudioNodes (AudioNode)
import Model.Perspective (Perspective)

data Message
  = AudioContextCreated AudioContext
  | AudioNodeControlsSelected AudioNodeId
  | ControlsAdjusted AudioNodeId AudioNode
  | DiagramRendered String
  | PerspectiveChanged Perspective
  | PlayRequested
  | PlaybackStarted AudioContext
  | StopRequested
