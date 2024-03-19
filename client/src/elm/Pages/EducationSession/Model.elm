module Pages.EducationSession.Model exposing (..)

import Backend.EducationSession.Model exposing (..)
import Backend.Entities exposing (..)
import Debouncer.Basic as Debouncer exposing (Debouncer, debounce, toDebouncer)
import EverySet exposing (EverySet)
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
    | SetEducationTopic (EverySet EducationTopic) EducationTopic
    | SaveTopics EducationSessionId EducationSession
    | MsgDebouncer (Debouncer.Msg Msg)
    | SetInput String
    | SetSearch String
