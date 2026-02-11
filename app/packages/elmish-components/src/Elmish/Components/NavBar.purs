module Elmish.Components.NavBar (Item(..), Model, view) where

import Prelude

import Data.Array (fromFoldable)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List, singleton)
import Data.Map (Map)
import Elmish (Dispatch, ReactElement)
import Elmish.Dispatch ((<|))
import Elmish.HTML.Styled as H

type Model t msg = Map t (Item msg)
data Item msg = Active | Available msg | Unavailable String

view ∷ ∀ msg t. (t → String) → Model t msg → Dispatch msg → ReactElement
view showTitle model dispatch = H.nav ""
  [ H.ul "" (fromFoldable $ renderItems model) ]
  where
  renderItems ∷ Map t (Item msg) → List ReactElement
  renderItems = foldMapWithIndex
    (\title → singleton <<< renderItem title)

  renderItem ∷ t → Item msg → ReactElement
  renderItem title item = H.li ""
    [ render $ showTitle title ]
    where
    render ∷ String → ReactElement
    render = case item of
      Active →
        H.button "secondary"
      Available message →
        H.a_ ""
          { href: "#"
          , onClick: dispatch <| const message
          }
      Unavailable reason →
        H.a_ "secondary" { _data: H._data { tooltip: reason } }
