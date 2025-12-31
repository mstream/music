module Test.Codec (codecTestSuite, unsafeDecoded) where

import Prelude

import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map (Map)
import Parsing (ParseError, parseErrorMessage, runParser)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type CodeTestSuiteConf d e o =
  { codec ∷ Codec d e o
  , encoderOpts ∷ o
  , examples ∷ Map d e
  , name ∷ String
  }

codecTestSuite
  ∷ ∀ d e o
  . Eq e
  ⇒ Eq d
  ⇒ Show d
  ⇒ Show e
  ⇒ CodeTestSuiteConf d e o
  → Spec Unit
codecTestSuite { codec, encoderOpts, examples, name } = describe
  (name <> " codec")
  (traverseWithIndex_ testCase examples)
  where
  parse ∷ e → ParseError \/ d
  parse s = runParser s (Codec.decoder codec)

  render ∷ d → e
  render = (Codec.encoder codec) encoderOpts

  testCase ∷ d → e → Spec Unit
  testCase parsedExample renderedExample = it "roundtrips"
    ( case parse renderedExample of
        Left parseError →
          fail $ show parseError
        Right parsed → do
          shouldEqual parsedExample parsed
          shouldEqual renderedExample (render parsed)
    )

unsafeDecoded ∷ ∀ d e o. Codec d e o → e → d
unsafeDecoded codec encoded =
  case runParser encoded (Codec.decoder codec) of
    Left parseError →
      unsafeCrashWith $ parseErrorMessage parseError
    Right decoded →
      decoded

