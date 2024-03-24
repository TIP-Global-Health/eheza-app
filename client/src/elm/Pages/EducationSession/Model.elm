module Pages.EducationSession.Model exposing (..)

import Backend.EducationSession.Model exposing (..)
import Backend.Entities exposing (..)
import Debouncer.Basic as Debouncer exposing (Debouncer, debounce, toDebouncer)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : Maybe ViewMode
    , debouncer : Debouncer Msg Msg
    , search : Maybe String
    , input : String
    }


emptyModel : Model
emptyModel =
    { viewMode = Nothing
    , debouncer = debounce 500 |> toDebouncer
    , search = Nothing
    , input = ""
    }


type ViewMode
    = ModeTopics (EverySet EducationTopic)
    | ModeAttendance (EverySet PersonId)


type Msg
    = SetActivePage Page
    | SetViewMode ViewMode
    | ToggleEducationTopic (EverySet EducationTopic) EducationTopic
    | SaveTopics (EverySet PersonId) (EverySet EducationTopic)
    | MsgDebouncer (Debouncer.Msg Msg)
    | SetInput String
    | SetSearch String
    | ToggleAttendance (EverySet PersonId) PersonId
    | SaveAttendance (EverySet PersonId)
    | EndEncounter