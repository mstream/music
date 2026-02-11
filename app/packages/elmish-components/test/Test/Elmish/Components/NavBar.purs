module Test.Elmish.Components.NavBar (Message, Tab, def) where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Elmish (ComponentDef', Dispatch, ReactElement, Transition')
import Elmish.Components.NavBar (Item(..))
import Elmish.Components.NavBar as NavBar
import Elmish.HTML.Styled as H

data Tab = Tab1 | Tab2 | Tab3

derive instance Eq Tab
derive instance Ord Tab

type Model = Tab
data Message = SelectTab Tab

def ∷ ∀ m. ComponentDef' m Message Model
def = { init, view, update }
  where
  init ∷ Transition' m Message Model
  init = pure Tab2

  view ∷ Model → Dispatch Message → ReactElement
  view model dispatch = H.div ""
    [ NavBar.view
        showTab
        ( Map.fromFoldable
            [ Tab1 /\
                if model == Tab1 then Active
                else Available $ SelectTab Tab1
            , Tab2 /\
                if model == Tab2 then Active
                else Available $ SelectTab Tab2
            , Tab3 /\
                Unavailable "disabled"
            ]
        )
        dispatch
    , H.text $ "this is " <> showTab model
    ]
    where
    showTab ∷ Tab → String
    showTab = case _ of
      Tab1 →
        "Tab1"
      Tab2 →
        "Tab2"
      Tab3 →
        "Tab3"

  update ∷ Model → Message → Transition' m Message Model
  update _ message = pure case message of
    SelectTab tab → tab

