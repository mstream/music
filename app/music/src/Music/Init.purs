module Music.Init (init) where

import Prelude

import Data.Codec as Codec
import Effect.Class (class MonadEffect)
import Elmish as E
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.Perspective as Perspective
import Random.LCG (mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen as Gen

init ∷ ∀ m. MonadEffect m ⇒ Init m Model
init = do
  codePerspective ← Code.init ""
  E.fork $ pure $ CodeChanged exampleCode
  pure { perspective: Perspective.Code codePerspective }

exampleCode ∷ String
exampleCode = Codec.encoder AudioNodes.stringCodec unit
  exampleAudioNodes

exampleAudioNodes ∷ AudioNodes
exampleAudioNodes = Gen.evalGen arbitrary
  { newSeed: mkSeed 123, size: 2 }

