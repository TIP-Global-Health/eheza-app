module Pages.NCD.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsCurrentMode
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
    }


type NCDRiskFactor
    = RiskFactorSmokeCigarettes
    | RiskFactorConsumeSalt
    | RiskFactorHypertensionHistory
    | RiskFactorHearProblemHistory
    | RiskFactorDiabetesHistory


type Msg
    = NoOp
    | CloseEncounter NCDEncounterId
    | SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetLabResultsMode (Maybe LabResultsMode)
    | SetEndEncounterDialogState Bool
