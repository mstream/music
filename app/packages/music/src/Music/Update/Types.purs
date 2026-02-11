module Music.Update.Types (Update) where

import Music.Init.Types (Init)
import Music.Message (Message)

type Update m inputModel outputModel =
  inputModel → Message → Init m outputModel
