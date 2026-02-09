module Utils (getConf) where

import Prelude

import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.LzString as LzString
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (liftEffect)
import Music.ComponentDef (Config)
import Web.Clipboard (Clipboard)
import Web.Clipboard as Clipboard
import Web.HTML (window) as HTML
import Web.HTML.Location (hash, setHash) as HTML
import Web.HTML.Window (location, navigator) as HTML

getConf ∷ Boolean → Effect Config
getConf isDebugModeActive = do
  codeProcessingResult ← processCodeHash
  mbClipboard ← getClipboard
  pure { codeProcessingResult, isDebugModeActive, mbClipboard }

getClipboard ∷ Effect (Maybe Clipboard)
getClipboard = liftEffect do
  window ← HTML.window
  navigator ← HTML.navigator window
  Clipboard.clipboard navigator

processCodeHash ∷ Effect (String \/ String)
processCodeHash = do
  window ← HTML.window
  location ← HTML.location window
  hash ← HTML.hash location
  HTML.setHash "" location
  pure case String.stripPrefix (Pattern "#") hash of
    Nothing →
      Right ""
    Just s →
      if String.null s then Right ""
      else LzString.decode s
