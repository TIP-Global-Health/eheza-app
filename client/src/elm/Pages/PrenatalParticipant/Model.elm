module Pages.PrenatalParticipant.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Model
import Date exposing (Date)
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
