module Test.Mermaid.DiagramDef.Blocks.BlockId (spec) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Codec as Codec
import Data.Map as Map
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Mermaid.DiagramDef.Blocks.BlockId (BlockId)
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Mermaid.DiagramDef.Blocks.BlockId.AlphaChar (AlphaChar(..))
import Mermaid.DiagramDef.Blocks.BlockId.NumChar (NumChar(..))
import Parsing.String (eof)
import Test.Data.Codec (codecTestSuite)
import Test.Data.Value (valueTestSuite)
import Test.Mermaid.DiagramDef.Blocks.BlockId.Unsafe (unsafeBlockId)
import Test.Spec (Spec)
import Test.Utils (orderedTestSuite, semigroupTestSuite)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec = do
  codecTestSuite
    { codec: Codec.codec
        (Codec.decoder BlockId.stringCodec <* eof)
        (Codec.encoder BlockId.stringCodec)
    , counterExamples: Set.fromFoldable
        [ "_abc"
        , "abc_"
        , "1abc"
        , "ab1cd"
        , "ABCD"
        , "ab-cd"
        ]
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
  valueTestSuite (Proxy ∷ Proxy BlockId) "BlockId"
  orderedTestSuite
    { examples: Set.fromFoldable
        [ orderedExamples
        ]
    , name: "BlockId"
    }
  semigroupTestSuite "BlockId" (Proxy ∷ Proxy BlockId)

orderedExamples ∷ NonEmptyArray BlockId
orderedExamples = ArrayNE.cons' (unsafeBlockId "a")
  [ unsafeBlockId "b", unsafeBlockId "c" ]