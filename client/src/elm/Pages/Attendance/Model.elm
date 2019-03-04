module Pages.Attendance.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { filter : String
    }


emptyModel : Model
emptyModel =
    { filter = ""
    }


type Msg
    = SetActivePage Page
    | SetCheckedIn MotherId Bool
    | SetFilter String
