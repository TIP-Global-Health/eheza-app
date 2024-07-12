module Pages.NCD.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.ReportToWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), LabResultsMode)


type alias Model =
    { diagnosisMode : DiagnosisMode
    , labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsMode
    , showEndEncounterDialog : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentNCD)
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    , components = Nothing
    }


type NCDRiskFactor
    = RiskFactorSmokeCigarettes
    | RiskFactorConsumeSalt
    | RiskFactorHypertensionHistory
    | RiskFactorHearProblemHistory
    | RiskFactorDiabetesHistory


type Msg
    = CloseEncounter NCDEncounterId
    | SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetLabResultsMode (Maybe LabResultsMode)
    | SetEndEncounterDialogState Bool
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.ReportToWhatsAppDialog.Model.ReportComponentsList)
