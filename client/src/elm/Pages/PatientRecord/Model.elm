module Pages.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Person.Model exposing (Person)
import Pages.Page exposing (Page)
import Pages.WellChild.ProgressReport.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , viewMode : ViewMode
    , filter : PatientRecordFilter
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , viewMode = ViewPatientRecord
    , filter = FilterAcuteIllness
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
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
