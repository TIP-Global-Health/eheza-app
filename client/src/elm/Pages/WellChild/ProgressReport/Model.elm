module Pages.WellChild.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , showEndEncounterDialog = False
    }


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
    , allowEndEncounter : Bool
    , closeEncounterMsg : msg
    , setEndEncounterDialogStateMsg : Bool -> msg
    , startEncounterMsg : msg
    }


type NCDAANCNewbornItem
    = RegularCheckups
    | IronDuringPregnancy


type NCDANutritionBehaviorItem
    = BreastfedSixMonths
    | AppropriateComplementaryFeeding
    | DiverseDiet
    | MealsADay


type NCDAInfrastructureEnvironmentWashItem
    = HasToilets
    | HasCleanWater
    | HasHandwashingFacility
    | HasKitchenGarden
    | InsecticideTreatedBedNets


type NCDACellValue
    = NCDACellValueV
    | NCDACellValueX
    | NCDACellValueDash
    | NCDACellValueEmpty


type Msg
    = NoOp
    | CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | SetDiagnosisMode DiagnosisMode
