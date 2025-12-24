module Data.Codec.AudioNodes
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  ) where

import Data.Codec (Codec, Decoder, Encoder)
import Data.Map (Map)
import Model.AudioNode (AudioNode, AudioNodeId)

type AudioNodesCodec o = Codec (Map AudioNodeId AudioNode) o

type AudioNodesDecoder = Decoder (Map AudioNodeId AudioNode)

type AudioNodesEncoder o = Encoder (Map AudioNodeId AudioNode) o

