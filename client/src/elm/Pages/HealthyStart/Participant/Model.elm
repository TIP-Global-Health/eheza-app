module Pages.HealthyStart.Participant.Model exposing (Model, Msg(..), emptyModel)

import Backend.Model
import Pages.Page exposing (Page)


type alias Model =
    { showWarningPopup : Bool
    }


type Msg
    = MsgBackend Backend.Model.MsgIndexedDb
    | SetActivePage Page
    | CloseWarningPopup


emptyModel : Model
emptyModel =
    { showWarningPopup = True }
