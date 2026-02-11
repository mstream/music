module Test.Music.Model.AudioNodes.AudioNode.Sequencer (spec) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Natural as Natural
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer as Sequencer
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (d2) as Duration
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( fromFoldable1
  ) as Sequence
import Test.Music.Model.AudioNodes.AudioNode.Sequencer.Duration (spec) as Duration
import Test.Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (spec) as Sequence
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec = do
  Duration.spec
  Sequence.spec

  describe "Sequencer.valueAt" do
    describe "1,2 with duration of two" do
      let
        sequencer ∷ Sequencer Int
        sequencer =
          { duration: Duration.d2
          , sequence: Sequence.fromFoldable1 $ ArrayNE.cons' 1 [ 2 ]
          }
      it "should return 1 for the first step" do
        shouldEqual
          1
          (sequencer `Sequencer.valueAt` Natural.intToNat 0)
      it "should return 1 for the second step" do
        shouldEqual
          1
          (sequencer `Sequencer.valueAt` Natural.intToNat 1)
      it "should return 2 for the third step" do
        shouldEqual
          2
          (sequencer `Sequencer.valueAt` Natural.intToNat 2)
      it "should return 2 for the fourth step" do
        shouldEqual
          2
          (sequencer `Sequencer.valueAt` Natural.intToNat 3)
      it "should return 1 for the fifth step" do
        shouldEqual
          1
          (sequencer `Sequencer.valueAt` Natural.intToNat 4)

