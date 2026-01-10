module Music.Message (Message(..), PerspectiveChange) where

import Audio.WebAudio.Types (AudioContext)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes.AudioNode (AudioNode)
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Perspective.PerspectiveName (PerspectiveName)
import Music.Model.Playback (PlaybackControls)

data Message
  = AudioContextCreated AudioContext
  | CodeChanged String
  | ControlsAdjusted AudioNodeId AudioNode
  | DiagramRendered String
  | PerspectiveChanged PerspectiveChange
  | PlayRequested
  | PlaybackStarted PlaybackControls
  | StopRequested

type PerspectiveChange =
  { audioNodes ∷ AudioNodes
  , toPerspective ∷ PerspectiveName
  }
