module View.Code
  ( audioNode
  , render
  ) where

import Prelude

import Data.CodePoint.Unicode (isAlphaNum)
import Model.AudioNode (AudioComponent(..), AudioNode, Wave(..))
import Parsing (Parser, fail)
import Parsing.String (char, string)
import Parsing.String.Basic (number, skipSpaces, takeWhile1)

render ∷ AudioNode → String
render { id, component } =
  case component of
    Oscillator { frequency, gain, wave } →
      "osc "
        <> id
        <> " {f="
        <> show frequency
        <> ",g="
        <> show gain
        <> ",w="
        <> renderWave wave
        <> "}"

audioNode ∷ Parser String AudioNode
audioNode = do
  _ ← string "osc"
  skipSpaces

  id ← takeWhile1 isAlphaNum
  skipSpaces

  _ ← char '{'
  skipSpaces

  _ ← char 'f'
  _ ← char '='
  frequency ← number
  skipSpaces

  _ ← char ','
  skipSpaces

  _ ← char 'g'
  _ ← char '='
  gain ← number
  skipSpaces

  _ ← char ','
  skipSpaces

  _ ← char 'w'
  _ ← char '='
  wave ← parseWave
  skipSpaces

  _ ← char '}'

  pure { id, component: Oscillator { frequency, gain, wave } }

renderWave ∷ Wave → String
renderWave wave =
  case wave of
    Sine →
      "sine"
    Square →
      "square"

parseWave ∷ Parser String Wave
parseWave = do
  waveText ← takeWhile1 isAlphaNum
  case waveText of
    "sine" →
      pure Sine
    "square" →
      pure Square
    _ →
      fail $ "Unrecognized wave type: " <> waveText
