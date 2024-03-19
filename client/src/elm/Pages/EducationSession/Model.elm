module Pages.EducationSession.Model exposing (..)

import Backend.EducationSession.Model exposing (..)
import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : Maybe ViewMode }


emptyModel : Model
emptyModel =
    { viewMode = Nothing }


type ViewMode
    = ModeTopics
    | ModeAttendance


type Msg
    = SetActivePage Page
    | SetViewMode ViewMode
