module Music.View.Controls.Sequencer.Sequence (view) where

import Prelude

import Data.Array as Array
import Data.Codec (Codec)
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Value (class Codeable)
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.HTML.Events (InputChangeEvent)
import Elmish.HTML.Events (handleEffect, inputText) as E
import Elmish.HTML.Styled as H
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence (Sequence)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.View.Types (ViewModelPure)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

view
  ∷ ∀ a
  . Bounded a
  ⇒ Codeable a Number Unit
  ⇒ Show a
  ⇒ String
  → (Int → String)
  → (Sequence a → Effect Unit)
  → (Number → Number)
  → (a → Number)
  → Codec a String Unit
  → ViewModelPure (Sequence a)
view
  name
  id
  handleChange
  fromSliderNumber
  toSliderNumber
  valueStringCodec
  model =
  H.div ""
    (Array.fromFoldable $ viewSequenceElement `mapWithIndex` model)
  where
  viewSequenceElement ∷ Int → a → ReactElement
  viewSequenceElement index currentValue = H.div ""
    [ H.label_ "" { htmlFor: id index } (name <> " " <> show index)
    , H.text $ Codec.encoder
        valueStringCodec
        unit
        currentValue
    , H.input_ ""
        { id: id index
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
        Console.error $ "Invalid sequence element input: " <>
          P.parseErrorMessage parseError
      Right newElementValue →
        case Sequence.updateAt index newElementValue model of
          Just newSequence →
            handleChange newSequence
          Nothing →
            -- this should never happen when index is in bound
            pure unit
      where
      parsingResult ∷ ParseError \/ a
      parsingResult = P.runParser
        ( case Number.fromString $ E.inputText event of
            Just x →
              show $ fromSliderNumber x
            Nothing →
              (E.inputText event)
        )
        (Codec.decoder valueStringCodec)

    step ∷ Number
    step = (maxValue - minValue) / 100.0

    maxValue ∷ Number
    maxValue = toSliderNumber top

    minValue ∷ Number
    minValue = toSliderNumber bottom

