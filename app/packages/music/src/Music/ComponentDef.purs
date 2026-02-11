module Music.ComponentDef (Config, def) where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe, isJust)
import Effect.Aff (Aff)
import Elmish.Component (ComponentDef, ComponentDef')
import Elmish.Component as Component
import Music.Api.Message (Message)
import Music.App (AppM)
import Music.App as App
import Music.Init (init)
import Music.Model (Model)
import Music.Update (update)
import Music.View (view)
import Web.Clipboard (Clipboard)

type Config =
  { codeProcessingResult ∷ String \/ String
  , isDebugModeActive ∷ Boolean
  , mbClipboard ∷ Maybe Clipboard
  }

def ∷ Config → ComponentDef Message Model
def conf =
  Component.nat toAff def'
  where
  toAff ∷ AppM ~> Aff
  toAff = App.runAppM
    { isDebugModeActive: conf.isDebugModeActive
    , mbClipboard: conf.mbClipboard
    }

  def' ∷ ComponentDef' AppM Message Model
  def' =
    { init: init (isJust conf.mbClipboard) conf.codeProcessingResult
    , view
    , update
    }

