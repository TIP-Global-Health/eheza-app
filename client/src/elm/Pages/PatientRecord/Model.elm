module Pages.PatientRecord.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Person.Model exposing (Person)
import Components.SendViaWhatsAppDialog.Model
import Pages.Page exposing (Page)
import Pages.WellChild.ProgressReport.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , viewMode : ViewMode
    , filter : PatientRecordFilter
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , viewMode = ViewPatientRecord
    , filter = FilterAcuteIllness
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetViewMode ViewMode
    | SetFilter PatientRecordFilter
    | MsgSendViaWhatsAppDialog Components.SendViaWhatsAppDialog.Model.Msg
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
