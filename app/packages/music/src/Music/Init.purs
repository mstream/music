module Music.Init (init) where

import Prelude

import Data.Either (Either(..), fromRight)
import Data.Either.Nested (type (\/))
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Elmish as E
import Music.Init.Perspective.Code (init) as Code
import Music.Init.Types (Init)
import Music.Model (Model)
import Music.Model.Perspective (Perspective(..))

init ∷ ∀ m. MonadEffect m ⇒ Boolean → String \/ String → Init m Model
init isClipboardAvailable codeProcessingResult = do
  E.forkVoid displayWarnings
  perspective ← Code <$> Code.init code
  pure { isClipboardAvailable, perspective }
  where
  code ∷ String
  code = fromRight "" codeProcessingResult

  displayWarnings ∷ m Unit
  displayWarnings = do
    when (not isClipboardAvailable)
      ( Console.warn
          "Clipboard not available - code sharing option is disabled."
      )
    case codeProcessingResult of
      Left errorMessage →
        Console.warn
          $ "Could not processed shared code: " <> errorMessage
      Right _ →
        pure unit

