module Music.Init.Types (Init) where

import Elmish (Transition')
import Music.Api.Message (Message)

type Init m model = Transition' m Message model
