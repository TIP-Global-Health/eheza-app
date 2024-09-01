module Backend.Completion.Model exposing (..)

import App.Types exposing (Site)
import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias CompletionData =
    { site : Site
    , entityName : String
    , entityType : SelectedEntity
    , acuteIllnessData : List (EncounterData AcuteIllnessActivity)
    , nutritionIndividualData : List (EncounterData NutritionChildActivity)
    , nutritionGroupData : List (NutritionGroupEncounterData NutritionMotherActivity NutritionChildActivity)
    , wellChildData : List (EncounterData WellChildActivity)
    }


type SelectedEntity
    = EntityGlobal
    | EntityHealthCenter


type alias EncounterData activity =
    { startDate : NominalDate
    , takenBy : Maybe TakenBy
    , completion : ActivitiesCompletionData activity
    }


type alias NutritionGroupEncounterData motherActivity childActivity =
    { startDate : NominalDate
    , takenBy : Maybe TakenBy
    , motherData : Maybe (ActivitiesCompletionData motherActivity)
    , childrenData : List (ActivitiesCompletionData childActivity)
    }


type alias ActivitiesCompletionData activity =
    { expectedActivities : List activity
    , completedActivities : List activity
    }


type AcuteIllnessActivity
    = AcuteIllnessAcuteFindings
    | AcuteIllnessContactsTracing
    | AcuteIllnessCoreExam
    | AcuteIllnessDangerSigns
    | AcuteIllnessFollowUp
    | AcuteIllnessMUAC
    | AcuteIllnessNutrition
    | AcuteIllnessVitals
    | AcuteIllnessCall114
    | AcuteIllnessCOVIDTesting
    | AcuteIllnessExposure
    | AcuteIllnessContactHC
    | AcuteIllnessHealthEducation
    | AcuteIllnessIsolation
    | AcuteIllnessMalariaTesting
    | AcuteIllnessMedicationDistribution
    | AcuteIllnessSendToHC
    | AcuteIllnessSymptomsGeneral
    | AcuteIllnessSymptomsGI
    | AcuteIllnessSymptomsRespiratory
    | AcuteIllnessTravelHistory
    | AcuteIllnessPriorTreatment
    | AcuteIllnessOngoingTreatment


type NutritionChildActivity
    = NutritionHeight
    | NutritionNutrition
    | NutritionPhoto
    | NutritionWeight
    | NutritionMUAC
    | NutritionContributingFactors
    | NutritionFollowUp
    | NutritionHealthEducation
    | NutritionSendToHC
    | NutritionNCDA
    | NutritionChildFbf


type NutritionMotherActivity
    = NutritionFamilyPlanning
    | NutritionLactation
    | NutritionMotherFbf


type WellChildActivity
    = WellChildAlbendazole
    | WellChildBCGImmunisation
    | WellChildCaring
    | WellChildContributingFactors
    | WellChildDTPImmunisation
    | WellChildECD
    | WellChildFeeding
    | WellChildFollowUp
    | WellChildFoodSecurity
    | WellChildHeadCircumference
    | WellChildHealthEducation
    | WellChildHeight
    | WellChildHygiene
    | WellChildIPVImmunisation
    | WellChildMebendezole
    | WellChildMRImmunisation
    | WellChildMUAC
    | WellChildNCDA
    | WellChildNextVisit
    | WellChildNutrition
    | WellChildOPVImmunisation
    | WellChildPCV13Immunisation
    | WellChildPhoto
    | WellChildPregnancySummary
    | WellChildRotarixImmunisation
    | WellChildSendToHC
    | WellChildSymptomsReview
    | WellChildVitals
    | WellChildVitaminA
    | WellChildWeight
    | WellChildHPVImmunisation
    | WellChildDTPSAImmunisation


type TakenBy
    = TakenByNurse
    | TakenByCHW
    | TakenByUnknown


type Msg
    = SetData Value
