module Music.Update.Types (Update) where

import Music.Api.Message (Message)
import Music.Init.Types (Init)

type Update m inputModel outputModel =
  inputModel → Message → Init m outputModel
