module Pages.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , viewMode : ViewMode
    , filter : PatientRecordFilter
    , spvReportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , viewMode = ViewPatientRecord
    , filter = FilterAcuteIllness
    , spvReportTab = TabSPVReport
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetSPVReportTab ReportTab
    | SetViewMode ViewMode
    | SetFilter PatientRecordFilter
    | NoOp


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
