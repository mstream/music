module Music.View.Controls.Sequencer.Duration (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Value (codecConf)
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish.HTML.Events (InputChangeEvent)
import Elmish.HTML.Events (handleEffect, inputText) as E
import Elmish.HTML.Styled as H
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration (Duration)
import Music.Model.AudioNodes.AudioNode.Sequencer.Duration as Duration
import Music.View.Types (ViewModelPure)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

view ∷ String → (Duration → Effect Unit) → ViewModelPure Duration
view id handleChange model = H.div ""
  [ H.label_ "" { htmlFor: id } "duartion"
  , H.text $ Codec.encoder Duration.valueStringCodec unit model
  , H.input_ ""
      { id
      , min: show minValue
      , max: show maxValue
      , onChange: E.handleEffect onChangeHandler
      , step: show step
      , type: "range"
      , value: show currentValue
      }
  ]
  where
  onChangeHandler ∷ InputChangeEvent → Effect Unit
  onChangeHandler event = case parsingResult of
    Left parseError →
      Console.error $ "Invalid duration input: " <>
        P.parseErrorMessage parseError
    Right newValue →
      handleChange newValue
    where
    parsingResult ∷ ParseError \/ Duration
    parsingResult = P.runParser
      ( case Number.fromString $ E.inputText event of
          Just x →
            show $ Int.round x
          Nothing →
            (E.inputText event)
      )
      (Codec.decoder Duration.valueStringCodec)

  step ∷ Number
  step = 1.0

  maxValue ∷ Number
  maxValue = toSliderNumber top

  minValue ∷ Number
  minValue = toSliderNumber bottom

  currentValue ∷ Number
  currentValue = toSliderNumber model

  toSliderNumber ∷ Duration → Number
  toSliderNumber = Int.toNumber <<< codecConf.unwrap

