module Music.ComponentDef (def) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Log.Level (LogLevel(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Elmish.Component (ComponentDef, ComponentDef')
import Elmish.Component as Component
import Music.Init (init)
import Music.Message (Message)
import Music.Model (Model)
import Music.Update (update)
import Music.View (view)

newtype AppM a = AppM (ReaderT Boolean Aff a)

derive newtype instance Applicative AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Functor AppM
derive newtype instance Monad AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk Boolean AppM
derive newtype instance MonadEffect AppM

instance MonadLogger AppM where
  log { level, message } = do
    isDebugModeActive ← ask
    case level, isDebugModeActive of
      Debug, false →
        pure unit
      Debug, true →
        Console.debug message
      Error, _ →
        Console.error message
      Info, _ →
        Console.info message
      Trace, false →
        pure unit
      Trace, true →
        Console.debug message
      Warn, _ →
        Console.warn message

runAppM ∷ Boolean → AppM ~> Aff
runAppM isDebugModeActive (AppM program) =
  runReaderT program isDebugModeActive

def ∷ Boolean → ComponentDef Message Model
def isDebugModeActive =
  Component.nat (runAppM isDebugModeActive) def'
  where
  def' ∷ ComponentDef' AppM Message Model
  def' = { init, view, update }
