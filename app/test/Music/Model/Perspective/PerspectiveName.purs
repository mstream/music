module Test.Music.Model.Perspective.PerspectiveName (spec) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as ArrayNE
import Data.Set as Set
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Test.Spec (Spec)
import Test.Utils (orderedTestSuite)

spec ∷ Spec Unit
spec = do
  orderedTestSuite
    { examples: Set.fromFoldable
        [ orderedExamples
        ]
    , name: "PerspectiveName"
    }

orderedExamples ∷ NonEmptyArray PerspectiveName
orderedExamples = ArrayNE.cons' Code [ Diagram, Controls ]

