module Music.View.Types (ViewModel, ViewModelPure, ViewVoid) where

import Elmish (Dispatch, ReactElement)

type ViewVoid msg = Dispatch msg → ReactElement
type ViewModel mod msg = mod → Dispatch msg → ReactElement
type ViewModelPure mod = mod → ReactElement

