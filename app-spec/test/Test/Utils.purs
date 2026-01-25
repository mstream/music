module Test.Utils
  ( assertForeignTrue
  , withBrowser
  , withBrowserPage
  , testClickEvent
  , cwd
  , isNull
  ) where

import Control.Monad.Error.Class (withResource)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Foreign (Foreign, readBoolean)
import Playwright (Browser, Page, Selector(..), URL)
import Playwright as PW
import Prelude (Unit, bind, discard, void, ($), (<>), (==))
import Test.Unit.Assert as Assert

assertForeignTrue ∷ Foreign → Aff Unit
assertForeignTrue value = do
  let
    eiBool = runExcept $ readBoolean value
  Assert.assert "Foreign value is boolean true" (eiBool == Right true)

withBrowser ∷ ∀ a. (Browser → Aff a) → Aff a
withBrowser = withResource acquire release
  where
  acquire = PW.launch PW.chromium {}
  release = PW.close

withBrowserPage ∷ ∀ a. URL → (Page → Aff a) → Aff a
withBrowserPage url action = do
  withBrowser
    \browser → do
      let
        acquire = PW.newPage browser { acceptDownloads: true }
        release = PW.close
      withResource acquire release
        \page → do
          void $ PW.goto page url {}
          action page

testClickEvent
  ∷ String → (Page → Selector → {} → Aff Unit) → Page → Aff Unit
testClickEvent event action page = do
  void $ PW.evaluate page ("setEventTester('" <> event <> "');")
  action page (Selector $ "#event-" <> event) {}
  result ← PW.evaluate page $ "checkEventHappened('" <> event <> "');"
  assertForeignTrue result

foreign import isNull ∷ ∀ a. a → Boolean

foreign import cwd ∷ String
