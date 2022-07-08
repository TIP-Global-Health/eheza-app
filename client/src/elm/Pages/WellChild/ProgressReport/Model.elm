module Pages.WellChild.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Components.SendViaWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type alias Model =
    { diagnosisMode : DiagnosisMode
    , showEndEncounterDialog : Bool
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.SendViaWhatsAppDialog.Model.ReportComponentWellChild)
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , showEndEncounterDialog = False
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    , components = Nothing
    }


type DiagnosisMode
    = ModeActiveDiagnosis
    | ModeCompletedDiagnosis


type PaneEntryStatus
    = StatusOngoing
    | StatusResolved


type ECDStatus
    = StatusOnTrack
    | StatusECDBehind
    | StatusOffTrack
    | NoECDStatus


type WellChildProgressReportInitiator
    = InitiatorWellChild WellChildEncounterId
    | InitiatorNutritionIndividual NutritionEncounterId
    | InitiatorNutritionGroup SessionId PersonId
    | InitiatorPatientRecord PatientRecordInitiator PersonId


type alias BottomActionData msg =
    { showEndEncounterDialog : Bool
    , allowEndEcounter : Bool
    , closeEncounterMsg : msg
    , setEndEncounterDialogStateMsg : Bool -> msg
    , startEncounterMsg : msg
    }


type Msg
    = NoOp
    | CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | SetDiagnosisMode DiagnosisMode
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.SendViaWhatsAppDialog.Model.ReportComponentsList)
