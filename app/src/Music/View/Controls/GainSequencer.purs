module Music.View.Controls.GainSequencer (view) where

import Prelude

import Data.Array.NonEmpty as ArrayNE
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.HTML.Events (InputChangeEvent)
import Elmish.HTML.Events (handleEffect, inputText) as E
import Elmish.HTML.Styled as H
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Message (Message(..))
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain (Gain)
import Music.Model.AudioNodes.AudioNode.Oscillator.Gain as Gain
import Music.Model.AudioNodes.AudioNode.Sequencer (Sequencer)
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence
  ( Sequence(..)
  )
import Music.Model.AudioNodes.AudioNode.Sequencer.Sequence as Sequence
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.View.Types (ViewModel)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

type GainSequencerControls = { conf ∷ Sequencer Gain, id ∷ AudioNodeId }

view ∷ ViewModel GainSequencerControls Message
view model dispatch =
  let
    id ∷ String
    id = elementId "controls"
  in
    H.div_ "" { id } [ gainElement ]
  where
  gainElement ∷ ReactElement
  gainElement = H.div ""
    [ H.label_ "" { htmlFor: id } "gain"
    , H.text $ Codec.encoder Gain.stringCodec unit
        (ArrayNE.head $ Sequence.toArray model.conf.sequence)
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
        Console.error $ "Invalid gain input: " <>
          P.parseErrorMessage parseError
      Right newGain →
        dispatch $ ControlsAdjusted model.id
          ( GainSequencer model.conf
              { sequence = Sequence $ ArrayNE.singleton newGain }
          )
      where
      parsingResult ∷ ParseError \/ Gain
      parsingResult = P.runParser
        (E.inputText event)
        (Codec.decoder Gain.stringCodec)

    step ∷ Number
    step = (maxValue - minValue) / 100.0

    maxValue ∷ Number
    maxValue = Gain.toNumber top

    minValue ∷ Number
    minValue = Gain.toNumber bottom

    currentValue ∷ Number
    currentValue = Gain.toNumber $ ArrayNE.head $ Sequence.toArray
      model.conf.sequence

    id ∷ String
    id = elementId "gain"

  elementId ∷ String → String
  elementId suffix = "gain-sequencer-"
    <> Codec.encoder BlockId.stringCodec unit model.id
    <> "-"
    <>
      suffix
