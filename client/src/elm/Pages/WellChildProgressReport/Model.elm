module Pages.WellChildProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Pages.Page exposing (Page)


type alias Model =
    { diagnosisMode : DiagnosisMode
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , showEndEncounterDialog = False
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


type alias EndEncounterData msg =
    { showEndEncounterDialog : Bool
    , allowEndEcounter : Bool
    , closeEncounterMsg : msg
    , setEndEncounterDialogStateMsg : Bool -> msg
    }


type Msg
    = CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | SetDiagnosisMode DiagnosisMode
