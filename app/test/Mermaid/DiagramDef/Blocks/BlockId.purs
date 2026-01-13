module Test.Mermaid.DiagramDef.Blocks.BlockId (spec, unsafeBlockId) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.Laws (lawsTestSuite)
import Test.QuickCheck.Laws.Data (checkSemigroup)
import Test.Spec (Spec)
import Test.Utils (orderedTestSuite)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: BlockId.stringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ unsafeBlockId "abc" /\ "abc" ]
    , name: "BlockId/String"
    }
  orderedTestSuite
    { examples: Set.fromFoldable
        [ orderedExamples
        ]
    , name: "BlockId"
    }
  lawsTestSuite "BlockId" do
    checkSemigroup (Proxy ∷ Proxy BlockId)

orderedExamples ∷ NonEmptyArray BlockId
orderedExamples = ArrayNE.cons' exampleA
  [ exampleB, exampleC ]

exampleA ∷ BlockId
exampleA = unsafeBlockId "a"

exampleB ∷ BlockId
exampleB = unsafeBlockId "b"

exampleC ∷ BlockId
exampleC = unsafeBlockId "c"

unsafeBlockId ∷ String → BlockId
unsafeBlockId = unsafeDecoded BlockId.stringCodec

