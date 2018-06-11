module Pages.Admin.Model exposing (..)

import Backend.Model
import Backend.Session.Form exposing (SessionForm)
import Form
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
    | MsgCreateSession Form.Msg
    | SetActivePage Page
    | MsgBackend Backend.Model.MsgBackend
