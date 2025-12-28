module Music.View.Components.Accordion (Item, Items, view) where

import Prelude

import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Music.View.Types (ViewModelPure)

type Items t = Map t Item
type Item = { contents ∷ ReactElement, open ∷ Boolean }

view ∷ ∀ t. Show t ⇒ ViewModelPure (Items t)
view model =
  H.div "" renderedElements
  where
  renderedElements ∷ Array ReactElement
  renderedElements = Array.intersperse (H.hr "")
    (Array.fromFoldable renderedItems)

  renderedItems ∷ List ReactElement
  renderedItems = Map.values $ mapWithIndex renderItem model

  renderItem ∷ t → Item → ReactElement
  renderItem title { contents, open } =
    H.details_ "" { open }
      [ H.summary_ "" { role: "button" } (show title), contents ]
