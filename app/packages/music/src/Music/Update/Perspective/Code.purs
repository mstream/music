module Music.Update.Perspective.Code (update) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Elmish as E
import Music.App (Env)
import Music.Init.Perspective.Controls as Controls
import Music.Init.Perspective.Diagram as Diagram
import Music.Init.Types (Init)
import Music.Message (Message(..))
import Music.Model.AudioNodes (AudioNodes)
import Music.Model.Perspective (CodePerspective, Perspective)
import Music.Model.Perspective as Perspective
import Music.Model.Perspective.PerspectiveName (PerspectiveName(..))
import Music.Update.Types (Update)
import Promise as Promise
import Web.Clipboard (Clipboard)
import Web.Clipboard as Clipboard
import Web.HTML (window) as HTML
import Web.HTML.Location (href) as HTML
import Web.HTML.Window (location) as HTML

update
  ∷ ∀ m
  . MonadAff m
  ⇒ MonadAsk Env m
  ⇒ MonadLogger m
  ⇒ Update m CodePerspective Perspective
update codePerspective = case _ of
  CodeChanged newCode →
    handleCodeChanged newCode
  CopyLinkToClipboardRequested encodedCode →
    handleCopyLinkToClipboardRequested encodedCode
  PerspectiveChanged { audioNodes, toPerspective } →
    handlePerspectiveChanged audioNodes toPerspective
  _ →
    noop
  where
  handleCodeChanged ∷ String → Init m Perspective
  handleCodeChanged newCode = pure
    $ Perspective.Code codePerspective { code = newCode }

  handleCopyLinkToClipboardRequested ∷ String → Init m Perspective
  handleCopyLinkToClipboardRequested encodedCode = do
    E.forks \{ dispatch } → do
      { mbClipboard } ← ask
      liftEffect case mbClipboard of
        Just clipboard → do
          baseUrl ← liftEffect getBaseUrl
          copyToClipboard
            (dispatch CopyLinkToClipboardSucceeded)
            (dispatch CopyLinkToClipboardFailed)
            clipboard
            (baseUrl <> "#" <> encodedCode)
        Nothing →
          pure unit

    noop

  handlePerspectiveChanged
    ∷ AudioNodes → PerspectiveName → Init m Perspective
  handlePerspectiveChanged audioNodes = case _ of
    Controls → do
      controlsPerspective ← Controls.init audioNodes
      pure $ Perspective.Controls controlsPerspective
    Diagram → do
      diagramPerspective ← Diagram.init audioNodes
      pure $ Perspective.Diagram diagramPerspective
    _ →
      noop

  noop ∷ Init m Perspective
  noop = pure $ Perspective.Code codePerspective

getBaseUrl ∷ Effect String
getBaseUrl = do
  window ← HTML.window
  location ← HTML.location window
  href ← HTML.href location
  pure case String.stripSuffix (Pattern "#") href of
    Just s →
      s
    Nothing →
      href

copyToClipboard
  ∷ (Effect Unit) → (Effect Unit) → Clipboard → String → Effect Unit
copyToClipboard onSuccess onFailure clipboard text = do
  writePromise ← Clipboard.writeText text clipboard
  void $ Promise.thenOrCatch
    ( \_ → do
        onSuccess
        pure $ Promise.resolve unit
    )
    ( \_ → do
        onFailure
        pure $ Promise.resolve unit
    )
    writePromise

