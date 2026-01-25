module Test.Music.Model.AudioNodes.AudioNode.Oscillator.Wave (spec) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Test.Data.Codec (codecTestSuite)
import Test.Data.Value (valueTestSuite)
import Test.Spec (Spec)
import Test.Utils (orderedTestSuite)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec = do
  codecsTestSuite $ Map.fromFoldable
    [ exampleSine /\ { parameter: "w=sine", value: "sine" }
    , exampleSquare /\ { parameter: "w=square", value: "square" }
    ]
  valueTestSuite (Proxy ∷ Proxy Wave) "Wave"
  orderedTestSuite
    { examples: Set.fromFoldable
        [ orderedExamples
        ]
    , name: "Wave"
    }

type CodecsTestSuiteConf = Map Wave
  { parameter ∷ String, value ∷ String }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite examples = do
  parameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  valueStringCodecTestSuite ((_.value) <$> examples)

parameterStringCodecTestSuite ∷ Map Wave String → Spec Unit
parameterStringCodecTestSuite examples = codecTestSuite
  { codec: Wave.parameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=sine", "w = sine", "w:sine" ]
  , encoderOpts: unit
  , examples
  , name: "wave parameter/string"
  }

valueStringCodecTestSuite ∷ Map Wave String → Spec Unit
valueStringCodecTestSuite examples = codecTestSuite
  { codec: Wave.valueStringCodec
  , counterExamples: Set.fromFoldable [ "unknown" ]
  , encoderOpts: unit
  , examples
  , name: "wave value/string"
  }

orderedExamples ∷ NonEmptyArray Wave
orderedExamples = ArrayNE.cons' exampleSine [ exampleSquare ]

exampleSine ∷ Wave
exampleSine = Sine

exampleSquare ∷ Wave
exampleSquare = Square
