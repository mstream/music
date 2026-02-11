module Test.Music.Model.AudioNodes.AudioNode.Note
  ( spec
  , unsafeNote
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Music.Model.AudioNodes.AudioNode.Note (Note)
import Music.Model.AudioNodes.AudioNode.Note as Note
import Test.Data.Codec (codecTestSuite, unsafeDecoded)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Utils (boundedTestSuite, orderedTestSuite)

spec ∷ Spec Unit
spec = do
  codecsTestSuite $ Map.fromFoldable
    [ exampleC1 /\ { parameter: "n=C1", value: "C1" }
    , exampleCs1 /\
        { parameter: "n=C#1", value: "C#1" }
    , exampleC2 /\ { parameter: "n=C2", value: "C2" }
    ]

  boundedTestSuite
    { generator: Gen.elements orderedExamples
    , name: "Note"
    }
  orderedTestSuite
    { examples: Set.fromFoldable [ orderedExamples ]
    , name: "Note"
    }

type CodecsTestSuiteConf = Map Note
  { parameter ∷ String, value ∷ String }

codecsTestSuite ∷ CodecsTestSuiteConf → Spec Unit
codecsTestSuite examples = do
  parameterStringCodecTestSuite
    ((_.parameter) <$> examples)
  valueStringCodecTestSuite ((_.value) <$> examples)

parameterStringCodecTestSuite ∷ Map Note String → Spec Unit
parameterStringCodecTestSuite examples = codecTestSuite
  { codec: Note.parameterStringCodec
  , counterExamples: Set.fromFoldable
      [ "a=C1", "n = C1", "n:C1" ]
  , encoderOpts: unit
  , examples
  , name: "note parameter/string"
  }

valueStringCodecTestSuite ∷ Map Note String → Spec Unit
valueStringCodecTestSuite examples = codecTestSuite
  { codec: Note.valueStringCodec
  , counterExamples: Set.fromFoldable [ "abc", "-1", "2" ]
  , encoderOpts: unit
  , examples
  , name: "note value/string"
  }

orderedExamples ∷ NonEmptyArray Note
orderedExamples = ArrayNE.cons' exampleC1
  [ exampleCs1, exampleC2 ]

exampleC1 ∷ Note
exampleC1 = unsafeNote "C1"

exampleCs1 ∷ Note
exampleCs1 = unsafeNote "C#1"

exampleC2 ∷ Note
exampleC2 = unsafeNote "C2"

unsafeNote ∷ String → Note
unsafeNote = unsafeDecoded Note.valueStringCodec
