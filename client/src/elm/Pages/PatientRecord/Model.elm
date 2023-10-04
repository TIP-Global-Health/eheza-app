module Pages.PatientRecord.Model exposing (..)

import Components.SendViaWhatsAppDialog.Model
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , viewMode : ViewMode
    , filter : PatientRecordFilter
    , reportToWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    , spvReportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , viewMode = ViewPatientRecord
    , filter = FilterAcuteIllness
    , reportToWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    , spvReportTab = TabSPVReport
    }


type Msg
    = NoOp
    | SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetSPVReportTab ReportTab
    | SetViewMode ViewMode
    | SetFilter PatientRecordFilter
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)


type ViewMode
    = ViewPatientRecord
    | ViewStartEncounter


type PatientType
    = PatientAdult
    | PatientChild
    | PatientUnknown


type PatientRecordFilter
    = FilterAcuteIllness
    | FilterAntenatal
    | FilterFamilyPlanning
    | FilterDemographics
