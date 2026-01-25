module Test.Music.Model.AudioNodes.Unsafe (unsafeAudioNodes) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Music.Model.AudioNodes (AudioNodeEntry, AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Partial.Unsafe (unsafeCrashWith)
import Test.Music.Model.AudioNodes.AudioNodeId.Unsafe
  ( unsafeAudioNodeId
  )

unsafeAudioNodes
  ∷ Array (String /\ (AudioNodeEntry /\ Array String))
  → AudioNodes
unsafeAudioNodes = Array.uncons >>> case _ of
  Just { head, tail } →
    case AudioNodes.make (toEntry head) (toEntry <$> tail) of
      Left violations →
        unsafeCrashWith $
          "audio node entries violate integrity constraints: " <>
            show violations
      Right audioNodes →
        audioNodes

  Nothing →
    unsafeCrashWith "audio nodes should have at least one entry"

  where
  toEntry
    ∷ String /\ (AudioNodeEntry /\ Array String) → AudioNodes.Entry
  toEntry (id /\ (audioNodeEntry /\ connectionEnds)) =
    unsafeAudioNodeId id /\
      ( audioNodeEntry /\
          (Set.fromFoldable $ unsafeAudioNodeId <$> connectionEnds)
      )

