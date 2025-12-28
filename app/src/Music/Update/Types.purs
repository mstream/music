module Music.Update.Types (Update, UpdateVoid) where

import Elmish (Transition)
import Music.Message (Message)
import Music.Model (Model)

type Update m = m → Message → Transition Message Model
type UpdateVoid = Message → Transition Message Model
