module Music.Message (Message(..)) where

import Audio.WebAudio.Types (AudioContext)
import Music.Model.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes (AudioNode)
import Music.Model.Perspective (Perspective)
import Music.Model.Playback (PlaybackControls)

data Message
  = AudioContextCreated AudioContext
  | AudioNodeControlsSelected AudioNodeId
  | ControlsAdjusted AudioNodeId AudioNode
  | DiagramRendered String
  | PerspectiveChanged Perspective
  | PlayRequested
  | PlaybackStarted PlaybackControls
  | StopRequested
