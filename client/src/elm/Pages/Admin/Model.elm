module Pages.Admin.Model exposing (..)

import Backend.Model
import Backend.Session.Form exposing (SessionForm)
import Form
import Pages.Page exposing (Page)
import Utils.Confirmation as Confirmation


type alias Model =
    { createSession : Maybe SessionForm
    , confirmation : Confirmation.Model Msg
    }


emptyModel : Model
emptyModel =
    { createSession = Nothing
    , confirmation = Confirmation.emptyModel
    }


type Msg
    = ResetCreateSessionForm
    | ShowCreateSessionForm Bool
    | MsgCreateSession Form.Msg
    | SetActivePage Page
    | MsgConfirmation (Confirmation.Msg Msg)
    | MsgBackend Backend.Model.MsgBackend
