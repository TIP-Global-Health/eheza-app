module Pages.PatientRecord.Model exposing (Model, Msg(..), PatientRecordFilter(..), PatientType(..), ViewMode(..), emptyModel)

import Components.ReportToWhatsAppDialog.Model
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , viewMode : ViewMode
    , filter : PatientRecordFilter
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    , spvReportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , viewMode = ViewPatientRecord
    , filter = FilterAcuteIllness
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    , spvReportTab = TabSPVReport
    }


type Msg
    = NoOp
    | SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetSPVReportTab ReportTab
    | SetViewMode ViewMode
    | SetFilter PatientRecordFilter
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)


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
    | FilterDemographics
    | FilterFamilyPlanning
    | FilterGroupEducation
