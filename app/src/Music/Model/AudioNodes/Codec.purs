module Music.Model.AudioNodes.Codec
  ( AudioNodesCodec
  , AudioNodesDecoder
  , AudioNodesEncoder
  ) where

import Data.Codec (Codec, Decoder, Encoder)
import Music.Model.AudioNodes (AudioNodes)

type AudioNodesCodec e o = Codec AudioNodes e o

type AudioNodesDecoder e = Decoder AudioNodes e

type AudioNodesEncoder e o = Encoder AudioNodes e o

