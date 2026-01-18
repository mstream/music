module Test.Mermaid.DiagramDef.Blocks.BlockId (spec, unsafeBlockId) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..))
import Mermaid.DiagramDef.Blocks.BlockId.NumChar (NumChar(..))
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.Laws (lawsTestSuite)
import Test.QuickCheck.Laws.Data (checkSemigroup)
import Test.Spec (Spec)
import Test.Utils (codeValueTestSuite, orderedTestSuite)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: BlockId.stringCodec
    , encoderOpts: unit
    , examples: Map.fromFoldable
        [ BlockId.make A [ B, C ] [] /\ "abc"
        , BlockId.make A [ B, C ] [ N1, N2 ] /\ "abc12"
        , (BlockId.make A [ B, C ] [ N1, N2 ] <> BlockId.make D [] [])
            /\
              "abc12_d"
        , ( BlockId.make A [ B, C ] [ N1, N2 ] <> BlockId.make D []
              [ N4 ]
          )
            /\ "abc12_d4"
        ]
    , name: "BlockId/String"
    }
  codeValueTestSuite (Proxy ∷ Proxy BlockId) "BlockId"
  orderedTestSuite
    { examples: Set.fromFoldable
        [ orderedExamples
        ]
    , name: "BlockId"
    }
  lawsTestSuite "BlockId" do
    checkSemigroup (Proxy ∷ Proxy BlockId)

orderedExamples ∷ NonEmptyArray BlockId
orderedExamples = ArrayNE.cons' (unsafeBlockId "a")
  [ unsafeBlockId "b", unsafeBlockId "c" ]

unsafeBlockId ∷ String → BlockId
unsafeBlockId = unsafeDecoded BlockId.stringCodec

