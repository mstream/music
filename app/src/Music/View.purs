module Music.View (view) where

import Prelude

import Data.Codec as Codec
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Music.Message (Message(..))
import Music.Model (Model)
import Music.Model.AudioNodes.Codec.Code (codec) as Code
import Music.Model.Perspective as Perspective
import Music.Update.Perspective.Controls (init) as Controls
import Music.View.Code (view) as Code
import Music.View.Components.NavBar as NavBar
import Music.View.Controls (view) as Controls
import Music.View.Diagram as Diagram
import Music.View.Types (ViewModel, ViewVoid)
import Parsing (runParser)

data Title = Code | Controls | Diagram

derive instance Eq Title

instance Ord Title where
  compare Code _ = LT
  compare Diagram other = case other of
    Code →
      GT
    Diagram →
      EQ
    Controls →
      LT
  compare Controls _ = GT

instance Show Title where
  show = case _ of
    Code →
      "Code"
    Controls →
      "Controls"
    Diagram →
      "Diagram"

view ∷ ViewModel Model
view model dispatch = H.div ""
  [ H.header "" [ logo, navBar dispatch ]
  , H.hr ""
  , H.main "" [ perspective ]
  , H.hr ""
  , H.footer "" [ githubLink ]
  ]
  where
  logo ∷ ReactElement
  logo = H.div "" [ H.text "MUSIC" ]

  githubLink ∷ ReactElement
  githubLink = H.a_ "" { href: "https://github.com/mstream/music" }
    [ H.img_ ""
        { id: "gh-logo"
        , src:
            "https://github.githubassets.com/assets/GitHub-Mark-ea2971cee799.png"
        }
    ]

  navBar ∷ ViewVoid
  navBar = NavBar.view $ Map.fromFoldable
    [ Code /\ codeNavbarItem
    , Controls /\ controlsNavbarItem
    , Diagram /\ diagramNavbarItem
    ]

  codeNavbarItem ∷ NavBar.Item
  codeNavbarItem = case model.perspective of
    Perspective.Code _ →
      NavBar.Active
    Perspective.Controls controlsModel →
      NavBar.Available
        $ PerspectiveChanged
        $ Perspective.Code
        $ Codec.encoder Code.codec unit controlsModel.audioNodes
    Perspective.Diagram _ →
      NavBar.Unavailable "TODO"

  controlsNavbarItem ∷ NavBar.Item
  controlsNavbarItem = case model.perspective of
    Perspective.Code s →
      case runParser s (Codec.decoder Code.codec) of
        Left _ →
          NavBar.Unavailable "code error"
        Right audioNodes →
          NavBar.Available
            $ PerspectiveChanged
            $ Perspective.Controls
            $ Controls.init audioNodes
    Perspective.Controls _ →
      NavBar.Active
    Perspective.Diagram _ →
      NavBar.Unavailable "TODO"

  diagramNavbarItem ∷ NavBar.Item
  diagramNavbarItem = case model.perspective of
    Perspective.Code _ →
      NavBar.Unavailable "TODO"
    Perspective.Controls _ →
      NavBar.Unavailable "TODO"
    Perspective.Diagram _ →
      NavBar.Unavailable "TODO"

  perspective ∷ ReactElement
  perspective = case model.perspective of
    Perspective.Code s →
      Code.view s dispatch
    Perspective.Controls controlsModel →
      Controls.view controlsModel dispatch
    Perspective.Diagram renderedDiagramHtml →
      Diagram.view renderedDiagramHtml

