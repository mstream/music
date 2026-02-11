module Test.Music.Model.AudioNodes.AudioNodeId.Unsafe
  ( unsafeAudioNodeId
  ) where

import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Test.Data.Codec (unsafeDecoded)

unsafeAudioNodeId ∷ String → AudioNodeId
unsafeAudioNodeId = unsafeDecoded AudioNodeId.stringCodec

