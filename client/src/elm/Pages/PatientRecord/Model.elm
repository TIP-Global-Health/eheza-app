module Pages.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.WellChildProgressReport.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , filter : PatientRecordFilter
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , filter = FilterAcuteIllness
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetFilter PatientRecordFilter
    | NoOp


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
