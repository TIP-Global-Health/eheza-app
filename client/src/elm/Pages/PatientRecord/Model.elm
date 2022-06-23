module Pages.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
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


type PatientRecordFilter
    = FilterAcuteIllness
    | FilterAntenatal
    | FilterDemographics


patientRecordFilters : Person -> List PatientRecordFilter
patientRecordFilters person =
    case person.gender of
        Male ->
            [ FilterAcuteIllness
            , FilterDemographics
            ]

        Female ->
            [ FilterAcuteIllness
            , FilterAntenatal
            , FilterDemographics
            ]
