module Music.View.Code (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Elmish ((<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes.Codec.Code (codec) as Code
import Music.Model.Perspective (CodePerspective)
import Music.View.Types (ViewModel)
import Parsing (ParseError, parseErrorMessage, runParser)

view ∷ ViewModel CodePerspective
view { code } dispatch = H.div ""
  [ H.textarea_ ""
      { defaultValue: code
      , onChange: dispatch <| CodeChanged <<< E.textareaText
      }
  , H.text case audioNodesParsingResult of
      Left parseError →
        "✗ " <> parseErrorMessage parseError
      Right _ →
        "☑"
  ]
  where
  audioNodesParsingResult ∷ ParseError \/ AudioNodes
  audioNodesParsingResult = runParser code (Codec.decoder Code.codec)
