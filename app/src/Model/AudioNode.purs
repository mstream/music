module Model.AudioNode
  ( AudioComponent(..)
  , AudioNode
  , OscillatorConf
  , Wave(..)
  , audioNode
  , render
  ) where

import Prelude

import Data.CodePoint.Unicode (isAlphaNum)
import Data.Generic.Rep (class Generic)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Parsing (Parser, fail)
import Parsing.String (char, string)
import Parsing.String.Basic (number, skipSpaces, takeWhile1)

type AudioNode = { component ∷ AudioComponent, id ∷ String }
data AudioComponent = Oscillator OscillatorConf

derive instance Eq AudioComponent
derive instance Generic AudioComponent _

instance Show AudioComponent where
  show = genericShow

type OscillatorConf = { frequency ∷ Number, gain ∷ Number, wave ∷ Wave }

data Wave = Sine | Square

derive instance Eq Wave
derive instance Generic Wave _

instance Show Wave where
  show = genericShow

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
