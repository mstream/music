module Music.View.Components.Accordion (Item, Model, view) where

import Prelude

import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Music.View.Types (ViewModelPure)

type Model t = Map t Item

type Item = { contents ∷ ReactElement, open ∷ Boolean }

view ∷ ∀ t. (t → String) → ViewModelPure (Model t)
view showTitle model =
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
      [ H.summary_
          "contrast outline"
          { role: "button" }
          (showTitle title)
      , contents
      ]
