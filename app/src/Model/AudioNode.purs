module Model.AudioNode
  ( AudioNode(..)
  , AudioNodeId
  , AudioNodes
  , OscillatorConf
  , Wave(..)
  , audioNodeId
  , unAudioNodeId
  , dummyAudioNodeId1
  , dummyAudioNodeId2
  ) where

import Prelude

import Data.CodePoint.Unicode (isAlphaNum)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Parsing (Parser)
import Parsing.String.Basic (takeWhile1)

type AudioNodes = Map AudioNodeId AudioNode

newtype AudioNodeId = AudioNodeId String

derive instance Eq AudioNodeId
derive instance Ord AudioNodeId
derive newtype instance Show AudioNodeId

audioNodeId ∷ Parser String AudioNodeId
audioNodeId = AudioNodeId <$> takeWhile1 isAlphaNum

unAudioNodeId ∷ AudioNodeId → String
unAudioNodeId (AudioNodeId s) = s

dummyAudioNodeId1 ∷ AudioNodeId
dummyAudioNodeId1 = AudioNodeId "dummy1"

dummyAudioNodeId2 ∷ AudioNodeId
dummyAudioNodeId2 = AudioNodeId "dummy2"

data AudioNode = Oscillator OscillatorConf

derive instance Eq AudioNode
derive instance Generic AudioNode _
derive instance Ord AudioNode

instance Show AudioNode where
  show = genericShow

type OscillatorConf = { frequency ∷ Number, gain ∷ Number, wave ∷ Wave }

data Wave = Sine | Square

derive instance Eq Wave
derive instance Generic Wave _
derive instance Ord Wave

instance Show Wave where
  show = genericShow
