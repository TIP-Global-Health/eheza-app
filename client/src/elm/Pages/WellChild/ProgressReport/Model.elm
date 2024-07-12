module Pages.WellChild.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Components.ReportToWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , activeTab : ReportTab
    , showEndEncounterDialog : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentWellChild)
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , activeTab = TabSPVReport
    , showEndEncounterDialog = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    , components = Nothing
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
    | InitiatorChildScoreboard ChildScoreboardEncounterId


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


type NCDATargetedInterventionsItem
    = FBFGiven
    | TreatmentForAcuteMalnutrition
    | TreatmentForDiarrhea
    | SupportChildWithDisability
    | ConditionalCashTransfer
    | ConditionalFoodItems


type NCDAUniversalInterventionsItem
    = Immunization
    | VitaminA
    | Deworming
    | OngeraMNP
    | ECDServices


type NCDAFillTheBlanksItem
    = HeightToAge
    | WeightToAge
    | MuacValue
    | EdemaPresent


type NCDACellValue
    = NCDACellValueV
    | NCDACellValueX
    | NCDACellValueDash
    | NCDACellValueC
    | NCDACellValueH
    | NCDACellValueT
    | NCDACellValueEmpty


type Msg
    = NoOp
    | CloseEncounter WellChildEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | SetDiagnosisMode DiagnosisMode
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.ReportToWhatsAppDialog.Model.ReportComponentsList)
    | SetActiveTab ReportTab
