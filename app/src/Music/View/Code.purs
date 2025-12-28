module Music.View.Code (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Elmish ((<|))
import Elmish.HTML.Events (textareaText)
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes.Codec.Code (codec) as Code
import Music.Model.Perspective (Perspective(..))
import Parsing (parseErrorMessage, runParser)
import Music.View.Types (ViewModel)

view ∷ ViewModel String
view s dispatch = H.div ""
  [ H.textarea_ ""
      { defaultValue: s
      , onChange: dispatch <| \e →
          PerspectiveChanged $ Code $ textareaText e
      }
  , H.text case runParser s (Codec.decoder Code.codec) of
      Left parseError →
        "✗ " <> parseErrorMessage parseError
      Right _ →
        "☑"
  ]
