module Update.Types (Update, UpdateVoid) where

import Elmish (Transition)
import Message (Message)
import Model (Model)

type Update m = m → Message → Transition Message Model
type UpdateVoid = Message → Transition Message Model
