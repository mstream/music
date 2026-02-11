module Music.Message (Message(..), PerspectiveChange) where

import Audio.WebAudio.Types (AudioContext)
import Music.Model.AudioNodes (AudioNodeEntry, AudioNodes)
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.Perspective.PerspectiveName (PerspectiveName)
import Music.Model.Playback (Playing)

data Message
  = AudioContextCreated AudioContext
  | CodeChanged String
  | ControlsAdjusted AudioNodeId AudioNodeEntry
  | DiagramRendered String
  | PerspectiveChanged PerspectiveChange
  | PlayRequested
  | PlaybackTick
  | PlaybackStarted Playing
  | CopyLinkToClipboardFailed
  | CopyLinkToClipboardRequested String
  | CopyLinkToClipboardSucceeded
  | StopRequested

type PerspectiveChange =
  { audioNodes ∷ AudioNodes
  , toPerspective ∷ PerspectiveName
  }
