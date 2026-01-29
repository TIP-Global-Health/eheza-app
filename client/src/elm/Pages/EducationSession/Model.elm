module Pages.EducationSession.Model exposing (InitialResultsDisplay(..), Model, Msg(..), ViewMode(..), emptyModel)

import Backend.EducationSession.Model exposing (..)
import Backend.Entities exposing (..)
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : Maybe ViewMode
    , initialResultsDisplay : InitialResultsDisplay
    , filter : String
    }


emptyModel : Model
emptyModel =
    { viewMode = Nothing
    , initialResultsDisplay = InitialResultsHidden
    , filter = ""
    }


type ViewMode
    = ModeTopics (EverySet EducationTopic)
    | ModeAttendance (EverySet PersonId)


type InitialResultsDisplay
    = InitialResultsHidden
    | InitialResultsShown


type Msg
    = SetActivePage Page
    | SetViewMode ViewMode
    | ToggleEducationTopic (EverySet EducationTopic) EducationTopic
    | SaveTopics (EverySet PersonId) (EverySet EducationTopic)
    | SetFilter String
    | ToggleInitialResultsDisplay
    | Reset
    | ToggleAttendance (EverySet PersonId) PersonId
    | SaveAttendance (EverySet PersonId)
    | EndEncounter
