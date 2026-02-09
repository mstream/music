module Test.Data.Codec (codecTestSuite, unsafeDecoded) where

import Prelude

import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Data.Set (Set)
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Parsing (ParseError, parseErrorMessage, runParser)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (class Arbitrary, Result(..), quickCheck, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type CodecTestSuiteConf d e o =
  { codec ∷ Codec d e o
  , counterExamples ∷ Set e
  , encoderOpts ∷ o
  , examples ∷ Map d e
  , name ∷ String
  }

codecTestSuite
  ∷ ∀ d e o
  . Arbitrary d
  ⇒ Eq e
  ⇒ Eq d
  ⇒ Show d
  ⇒ Show e
  ⇒ CodecTestSuiteConf d e o
  → Spec Unit
codecTestSuite { codec, counterExamples, encoderOpts, examples, name } =
  describe (name <> " codec") do
    traverse_ counterExampleTestCase counterExamples
    traverseWithIndex_ exampleTestCase examples
    randomTestCase
  where
  parse ∷ e → ParseError \/ d
  parse s = runParser s (Codec.decoder codec)

  render ∷ d → e
  render = (Codec.encoder codec) encoderOpts

  counterExampleTestCase ∷ e → Spec Unit
  counterExampleTestCase renderedExample = it
    "fails to parse - counter-example"
    ( case parse renderedExample of
        Left _ →
          pure unit
        Right parsed →
          fail $ "Parsing of '"
            <> show renderedExample
            <> "' succeeded but it should fail: "
            <> show parsed
    )

  exampleTestCase ∷ d → e → Spec Unit
  exampleTestCase parsedExample renderedExample = it
    "roundtrips - example"
    ( case parse renderedExample of
        Left parseError →
          fail $ show parseError <> "\n" <> show renderedExample
        Right parsed → do
          parsedExample `shouldEqual` parsed
          renderedExample `shouldEqual` (render parsed)
    )

  randomTestCase ∷ Spec Unit
  randomTestCase = it "roundtrips - random"
    (liftEffect $ quickCheck prop)
    where
    prop ∷ d → Result
    prop decoded =
      case parse rendered of
        Left parseError →
          Failed $ show parseError <> "\n" <> show rendered
        Right parsed → do
          decoded === parsed
      where
      rendered ∷ e
      rendered = render decoded

unsafeDecoded ∷ ∀ d e o. Show e ⇒ Codec d e o → e → d
unsafeDecoded codec encoded =
  case runParser encoded (Codec.decoder codec) of
    Left parseError →
      unsafeCrashWith $ "Could not parse '" <> show encoded <> "': " <>
        parseErrorMessage parseError
    Right decoded →
      decoded
