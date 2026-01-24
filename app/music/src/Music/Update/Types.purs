module Music.Update.Types (Update, UpdateVoid) where

import Elmish (Transition')
import Music.Message (Message)
import Music.Model (Model)

type Update m model = model → Message → Transition' m Message Model
type UpdateVoid m = Message → Transition' m Message Model
