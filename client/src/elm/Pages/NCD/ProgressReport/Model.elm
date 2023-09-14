module Pages.NCD.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.SendViaWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), LabResultsMode)


type alias Model =
    { diagnosisMode : DiagnosisMode
    , labResultsMode : Maybe LabResultsMode
    , labResultsHistoryOrigin : Maybe LabResultsMode
    , showEndEncounterDialog : Bool
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.SendViaWhatsAppDialog.Model.ReportComponentNCD)
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , labResultsMode = Nothing
    , labResultsHistoryOrigin = Nothing
    , showEndEncounterDialog = False
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    , components = Nothing
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
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.SendViaWhatsAppDialog.Model.ReportComponentsList)
