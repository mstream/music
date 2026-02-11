module Test.Elmish.Components.Accordion (def) where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Elmish (ComponentDef', Dispatch, ReactElement, Transition')
import Elmish.Components.Accordion as Accordion
import Elmish.HTML.Styled as H

data Color = Blue | Green | Red

derive instance Eq Color
derive instance Ord Color

type Model = Unit
type Message = Void

def ∷ ∀ m. ComponentDef' m Void Unit
def = { init, view, update }
  where
  init ∷ Transition' m Message Model
  init = pure unit

  view ∷ Model → Dispatch Message → ReactElement
  view _ _ = Accordion.view
    ( case _ of
        Blue → "Blue"
        Green → "Green"
        Red → "Red"
    )
    ( Map.fromFoldable
        [ Blue /\
            { contents: H.div "blue" [ H.text "blue" ]
            , open: false
            }
        , Green /\
            { contents: H.div "green" [ H.text "green" ]
            , open: true
            }
        , Red /\
            { contents: H.div "red" [ H.text "red" ]
            , open: false
            }
        ]
    )

  update ∷ Model → Message → Transition' m Message Model
  update model _ = pure model

