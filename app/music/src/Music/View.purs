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
import Music.Model.AudioNodes as AudioNodes
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.View.Code (view) as Code
import Music.View.Components.NavBar as NavBar
import Music.View.Controls (view) as Controls
import Music.View.Diagram as Diagram
import Music.View.Types (ViewModel, ViewVoid)
import Parsing (runParser)

view ∷ ViewModel Model Message
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

  navBar ∷ ViewVoid Message
  navBar = NavBar.view $ Map.fromFoldable
    [ Code /\ codeNavbarItem
    , Controls /\ controlsNavbarItem
    , Diagram /\ diagramNavbarItem
    ]

  codeNavbarItem ∷ NavBar.Item Message
  codeNavbarItem = case model.perspective of
    Perspective.Code _ →
      NavBar.Active
    Perspective.Controls { audioNodes } →
      NavBar.Available $ PerspectiveChanged
        { audioNodes, toPerspective: Code }
    Perspective.Diagram { audioNodes } →
      NavBar.Available $ PerspectiveChanged
        { audioNodes, toPerspective: Code }

  controlsNavbarItem ∷ NavBar.Item Message
  controlsNavbarItem = case model.perspective of
    Perspective.Code { code } →
      case runParser code (Codec.decoder AudioNodes.stringCodec) of
        Left _ →
          NavBar.Unavailable "code error"
        Right audioNodes →
          NavBar.Available $ PerspectiveChanged
            { audioNodes, toPerspective: Controls }
    Perspective.Controls _ →
      NavBar.Active
    Perspective.Diagram { audioNodes } →
      NavBar.Available $ PerspectiveChanged
        { audioNodes, toPerspective: Controls }

  diagramNavbarItem ∷ NavBar.Item Message
  diagramNavbarItem = case model.perspective of
    Perspective.Code { code } →
      case runParser code (Codec.decoder AudioNodes.stringCodec) of
        Left _ →
          NavBar.Unavailable "code error"
        Right audioNodes →
          NavBar.Available $ PerspectiveChanged
            { audioNodes, toPerspective: Diagram }
    Perspective.Controls { audioNodes } →
      NavBar.Available $ PerspectiveChanged
        { audioNodes, toPerspective: Diagram }
    Perspective.Diagram _ →
      NavBar.Active

  perspective ∷ ReactElement
  perspective = case model.perspective of
    Perspective.Code codePerspective →
      Code.view codePerspective dispatch
    Perspective.Controls controlsPerspective →
      Controls.view controlsPerspective dispatch
    Perspective.Diagram diagramPerspective →
      Diagram.view diagramPerspective
