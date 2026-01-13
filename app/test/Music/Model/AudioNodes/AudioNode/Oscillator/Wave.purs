module Test.Music.Model.AudioNodes.AudioNode.Oscillator.Wave (spec) where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Test.Codec (codecTestSuite)
import Test.Laws (lawsTestSuite)
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Wave.stringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ Sine /\ "w=sine"
        , Square /\ "w=square"
        ]
    , name: "wave/string"
    }
  lawsTestSuite "Wave" do
    checkEq (Proxy ∷ Proxy Wave)
    checkOrd (Proxy ∷ Proxy Wave)

