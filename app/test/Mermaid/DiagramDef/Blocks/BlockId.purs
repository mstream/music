module Test.Mermaid.DiagramDef.Blocks.BlockId (spec, unsafeBlockId) where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Test.Codec (codecTestSuite, unsafeDecoded)
import Test.Laws (lawsTestSuite)
import Test.QuickCheck.Laws.Data (checkSemigroup)
import Test.Spec (Spec)
import Test.Utils (orderTestSuite)
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
  orderTestSuite
    { examples: Set.fromFoldable
        [ [ unsafeBlockId "aaa"
          , unsafeBlockId "bbb"
          , unsafeBlockId "ccc"
          ]
        ]
    , name: "BlockId"
    }
  lawsTestSuite "BlockId" do
    checkSemigroup (Proxy ∷ Proxy BlockId)

unsafeBlockId ∷ String → BlockId
unsafeBlockId = unsafeDecoded BlockId.stringCodec

