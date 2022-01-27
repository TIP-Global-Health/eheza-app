module Pages.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.WellChildProgressReport.Model exposing (DiagnosisMode(..))


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


type PatientRecordFilter
    = FilterAcuteIllness
    | FilterAntenatal
    | FilterDemographics


patientRecordFilters : List PatientRecordFilter
patientRecordFilters =
    [ FilterAcuteIllness
    , FilterAntenatal
    , FilterDemographics
    ]
