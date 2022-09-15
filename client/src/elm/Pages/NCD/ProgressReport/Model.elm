module Pages.NCD.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.Report.Types exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..))


type alias Model =
    { labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsCurrentMode
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
    }


type Msg
    = NoOp
    | CloseEncounter NCDEncounterId
    | SetActivePage Page
    | SetLabResultsMode (Maybe LabResultsMode)
    | SetEndEncounterDialogState Bool
