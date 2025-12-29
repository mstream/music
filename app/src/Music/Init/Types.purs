module Music.Init.Types (Init) where

import Elmish (Transition)
import Music.Message (Message)

type Init m = Transition Message m
