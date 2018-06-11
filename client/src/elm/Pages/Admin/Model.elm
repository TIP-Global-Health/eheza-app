module Pages.Admin.Model exposing (..)

import Backend.Model
import Backend.Session.Form exposing (SessionForm)
import Pages.Page exposing (Page)


type alias Model =
    { createSession : Maybe SessionForm
    }


emptyModel : Model
emptyModel =
    { createSession = Nothing
    }


type Msg
    = ShowCreateSessionForm Bool
    | SaveCreatedSession
    | SetActivePage Page
    | MsgBackend Backend.Model.MsgBackend
