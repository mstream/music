module Music.View.Controls.Oscillator (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Effect (Effect)
import Effect.Class.Console as Console
import Elmish (ReactElement)
import Elmish.HTML.Events
  ( SelectChangeEvent
  )
import Elmish.HTML.Events (handleEffect, selectSelectedValue) as E
import Elmish.HTML.Styled as H
import Mermaid.DiagramDef.Blocks.BlockId as BlockId
import Music.Message (Message(..))
import Music.Model.AudioNodes.AudioNode (AudioNode(..))
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave (Wave(..))
import Music.Model.AudioNodes.AudioNode.Oscillator.Wave as Wave
import Music.Model.AudioNodes.AudioNodeId (AudioNodeId)
import Music.Model.AudioNodes.AudioNodeId as AudioNodeId
import Music.View.Types (ViewModel)
import Parsing (ParseError)
import Parsing (parseErrorMessage, runParser) as P

type OscillatorControls =
  { conf ∷ Oscillator, id ∷ AudioNodeId, isConnectedToOutput ∷ Boolean }

view ∷ ViewModel OscillatorControls Message
view model dispatch =
  let
    id ∷ String
    id = elementId "controls"
  in
    H.div_ "" { id }
      [ waveElement ]
  where
  waveElement ∷ ReactElement
  waveElement = H.div ""
    [ H.label_ "" { htmlFor: id } "wave shape"
    , H.select_ "" { id, onChange: E.handleEffect onChangeHandler }
        [ option Sine
        , option Square
        ]
    ]
    where
    onChangeHandler ∷ SelectChangeEvent → Effect Unit
    onChangeHandler event = case parsingResult of
      Left parseError →
        Console.error $ "Invalid wave input: " <>
          P.parseErrorMessage parseError
      Right newWave →
        dispatch $ ControlsAdjusted model.id
          { audioNode: Oscillator model.conf { wave = newWave }
          , isConnectedToOutput: model.isConnectedToOutput
          }
      where
      parsingResult ∷ ParseError \/ Wave
      parsingResult = P.runParser
        (E.selectSelectedValue event)
        (Codec.decoder Wave.valueStringCodec)

    option ∷ Wave → ReactElement
    option = H.option "" <<< Codec.encoder Wave.valueStringCodec unit

    id ∷ String
    id = elementId "wave"

  elementId ∷ String → String
  elementId suffix = "oscillator-"
    <> Codec.encoder BlockId.stringCodec unit
      (AudioNodeId.toBlockId model.id)
    <> "-"
    <>
      suffix
