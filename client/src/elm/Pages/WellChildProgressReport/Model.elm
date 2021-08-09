module Pages.WellChildProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { diagnosisMode : DiagnosisMode
    , showEndEncounetrDialog : Bool
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , showEndEncounetrDialog = False
    }


type DiagnosisMode
    = ModeActiveDiagnosis
    | ModeCompletedDiagnosis


type Msg
    = CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | SetDiagnosisMode DiagnosisMode
