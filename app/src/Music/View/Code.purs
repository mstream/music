module Music.View.Code (view) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Elmish (ReactElement, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNodeName (AudioNodeName(..))
import Music.Model.AudioNodes.Codec.Code (codec) as Code
import Music.Model.AudioNodes.Codec.Code.Parameter
  ( class Codeable
  , class Documentable
  , class Typed
  , CodecConf
  , Documentation
  , ValueType(..)
  )
import Music.Model.AudioNodes.Codec.Code.Parameter as Parameter
import Music.Model.Perspective (CodePerspective)
import Music.View.Components.Accordion as Accordion
import Music.View.Types (ViewModel)
import Parsing (ParseError, parseErrorMessage, runParser)
import Type.Proxy (Proxy(..))
import Type.RowList (class RowToList, Cons, Nil, RowList)

view ∷ ViewModel CodePerspective Message
view { code } dispatch = H.div "grid"
  [ H.div ""
      [ H.textarea_ ""
          { defaultValue: code
          , id: "editor"
          , onChange: dispatch <| CodeChanged <<< E.textareaText
          }
      , H.text case audioNodesParsingResult of
          Left parseError →
            "✗ " <> parseErrorMessage parseError
          Right _ →
            "☑"

      ]
  , Accordion.view documentationItems
  ]

  where
  audioNodesParsingResult ∷ ParseError \/ AudioNodes
  audioNodesParsingResult = runParser code (Codec.decoder Code.codec)

  documentationItems ∷ Accordion.Model AudioNodeName
  documentationItems =
    Map.fromFoldable
      [ Oscillator /\ viewDocumentationRecord
          (Proxy ∷ Proxy Oscillator)
      ] <#> \contents → { contents, open: false }

viewDocumentationRecord
  ∷ ∀ r rl
  . RowToList r rl
  ⇒ ViewDocumentationFields rl
  ⇒ Proxy (Record r)
  → ReactElement
viewDocumentationRecord _ = H.table ""
  [ H.thead ""
      [ H.tr ""
          [ H.th "" "Property Name"
          , H.th "" "Description"
          , H.th "" "Value Type"
          ]
      ]
  , H.tbody "" children
  ]
  where
  children ∷ Array ReactElement
  children = Array.fromFoldable
    $ viewDocumentationFields (Proxy ∷ Proxy rl)

class ViewDocumentationFields (rl ∷ RowList Type) where
  viewDocumentationFields ∷ Proxy rl → List ReactElement

instance ViewDocumentationFields Nil where
  viewDocumentationFields _ = Nil

instance
  ( Codeable v i a
  , Documentable v i
  , IsSymbol k
  , Typed i
  , ViewDocumentationFields t
  ) ⇒
  ViewDocumentationFields (Cons k v t) where
  viewDocumentationFields _ = fieldElements <> rest
    where
    fieldElements ∷ List ReactElement
    fieldElements = List.singleton $ H.tr ""
      [ H.th "" codecConf.name
      , H.td "" documentation.description
      , H.td "" (showValueType valueType)
      ]

    showValueType ∷ ValueType → String
    showValueType = case _ of
      Number →
        "number"
      String →
        "string"

    codecConf ∷ CodecConf v i a
    codecConf = Parameter.codecConf @v

    documentation ∷ Documentation i
    documentation = Parameter.documentation @v

    valueType ∷ ValueType
    valueType = Parameter.valueType (Proxy ∷ Proxy i)

    rest ∷ List ReactElement
    rest = viewDocumentationFields (Proxy ∷ Proxy t)
