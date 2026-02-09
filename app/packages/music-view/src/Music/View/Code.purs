module Music.View.Code (view) where

import Prelude

import Data.Array as Array
import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List(..))
import Data.List as List
import Data.LzString as LzString
import Data.Map as Map
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Effect.Uncurried (EffectFn1)
import Elmish (ReactElement, (<|))
import Elmish.HTML.Events (SyntheticEvent)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.AudioNodes as AudioNodes
import Music.Model.AudioNodes.AudioNode.Code.Documentable
  ( class Documentable
  , Documentation
  )
import Music.Model.AudioNodes.AudioNode.Code.Documentable as Documentable
import Music.Model.AudioNodes.AudioNode.Code.Parameter as Parameter
import Music.Model.AudioNodes.AudioNode.Oscillator (Oscillator)
import Music.Model.AudioNodes.AudioNodeName (AudioNodeName(..))
import Music.Model.Perspective (CodePerspective)
import Music.View.Components.Accordion as Accordion
import Music.View.Types (ViewModel)
import Parsing (ParseError, parseErrorMessage, runParser)
import Type.Proxy (Proxy(..))
import Type.RowList (class RowToList, Cons, Nil, RowList)

view ∷ Boolean → ViewModel CodePerspective Message
view isClipboardAvailable { code } dispatch = H.div "grid"
  [ H.div ""
      [ H.textarea_ ""
          { defaultValue: code
          , id: "editor"
          , onChange: dispatch <| CodeChanged <<< E.textareaText
          }
      , codeStatus
      ]
  , Accordion.view showAudioNodeName documentationItems
  ]

  where
  showAudioNodeName ∷ AudioNodeName → String
  showAudioNodeName = case _ of
    Oscillator →
      "Oscillator"

  codeStatus ∷ ReactElement
  codeStatus = case audioNodesParsingResult of
    Left parseError →
      H.text $ "✗ " <> parseErrorMessage parseError
    Right _ →
      H.div ""
        ( [ H.text "☑" ] <>
            case isClipboardAvailable, LzString.encode code of
              true, Right encodedCode →
                [ H.a_ ""
                    { href: "#"
                    , onClick: copyLinkWithCode encodedCode
                    }
                    [ H.img_ "icon"
                        { src:
                            "https://unpkg.com/lucide-static@0.563.0/icons/link.svg"
                        }
                    ]
                ]

              _, _ →
                []
        )

    where
    copyLinkWithCode ∷ String → EffectFn1 SyntheticEvent Unit
    copyLinkWithCode encodedCode = dispatch <|
      CopyLinkToClipboardRequested encodedCode

  audioNodesParsingResult ∷ ParseError \/ AudioNodes
  audioNodesParsingResult = runParser code
    (Codec.decoder AudioNodes.stringCodec)

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
  ( Parameter.Codeable v i a
  , Documentable v i
  , IsSymbol k
  , ViewDocumentationFields t
  ) ⇒
  ViewDocumentationFields (Cons k v t) where
  viewDocumentationFields _ = fieldElements <> rest
    where
    fieldElements ∷ List ReactElement
    fieldElements = List.singleton $ H.tr ""
      [ H.th "" parameterName
      , H.td "" documentation.description
      ]

    parameterName ∷ String
    parameterName = Parameter.name @v @i @a

    documentation ∷ Documentation i
    documentation = Documentable.documentation @v

    rest ∷ List ReactElement
    rest = viewDocumentationFields (Proxy ∷ Proxy t)

