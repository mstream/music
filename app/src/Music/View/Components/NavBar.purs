module Music.View.Components.NavBar (Item(..), Items, view) where

import Prelude

import Data.Array (fromFoldable)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List, singleton)
import Data.Map (Map)
import Elmish (ReactElement)
import Elmish.Dispatch ((<|))
import Elmish.HTML.Styled as H
import Music.Message (Message)
import Music.View.Types (ViewModel)

type Items t = Map t Item
data Item = Active | Available Message | Unavailable String

view ∷ ∀ t. Show t ⇒ ViewModel (Items t)
view itemsByTitle dispatch = H.nav ""
  [ H.ul "" (fromFoldable $ renderItems itemsByTitle) ]
  where
  renderItems ∷ Map t Item → List ReactElement
  renderItems = foldMapWithIndex
    (\title → singleton <<< renderItem title)

  renderItem ∷ t → Item → ReactElement
  renderItem title item = H.li ""
    [ render $ show title ]
    where
    render ∷ String → ReactElement
    render = case item of
      Active →
        H.button "secondary"
      Available message →
        H.a_ ""
          { href: "#"
          , onClick: dispatch <| message
          }
      Unavailable reason →
        H.a_ "secondary" { _data: H._data { tooltip: reason } }
