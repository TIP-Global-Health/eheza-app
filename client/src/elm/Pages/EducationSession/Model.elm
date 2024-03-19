module Pages.EducationSession.Model exposing (..)

import Backend.EducationSession.Model exposing (..)
import Backend.Entities exposing (..)
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : Maybe ViewMode }


emptyModel : Model
emptyModel =
    { viewMode = Nothing
    }


type ViewMode
    = ModeTopics (EverySet EducationTopic)
    | ModeAttendance (EverySet PersonId)


type Msg
    = SetActivePage Page
    | SetViewMode ViewMode
    | SetEducationTopic (EverySet EducationTopic) EducationTopic
    | SaveTopics EducationSessionId EducationSession
