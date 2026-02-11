module Music.App (AppM, Env, runAppM) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Log.Level (LogLevel(..))
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Web.Clipboard (Clipboard)

type Env =
  { mbClipboard ∷ Maybe Clipboard, isDebugModeActive ∷ Boolean }

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance Applicative AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Functor AppM
derive newtype instance Monad AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadAsk Env AppM
derive newtype instance MonadEffect AppM

instance MonadLogger AppM where
  log { level, message } = do
    { isDebugModeActive } ← ask
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

runAppM ∷ Env → AppM ~> Aff
runAppM env (AppM program) = runReaderT program env

