module Music.View.Types (ViewModel, ViewModelPure, ViewVoid) where

import Elmish (Dispatch, ReactElement)
import Music.Message (Message)

type ViewVoid = Dispatch Message → ReactElement
type ViewModel m = m → Dispatch Message → ReactElement
type ViewModelPure m = m → ReactElement

