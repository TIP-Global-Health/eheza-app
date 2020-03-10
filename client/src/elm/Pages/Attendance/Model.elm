module Pages.Attendance.Model exposing (InitialResultsDisplay(..), Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { filter : String
    , initialResultsDisplay : InitialResultsDisplay
    }


emptyModel : Model
emptyModel =
    { filter = ""
    , initialResultsDisplay = InitialResultsHidden
    }


type InitialResultsDisplay
    = InitialResultsHidden
    | InitialResultsShown


type Msg
    = Reset
    | SetActivePage Page
    | SetCheckedIn (Maybe AttendanceId) PersonId Bool
    | SetFilter String
    | ToggleInitialResultsDisplay
