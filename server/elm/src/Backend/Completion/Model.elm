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
    , childScoreboardData : List (EncounterData ChildScoreboardActivity)
    , hivData : List (EncounterData HIVActivity)
    , homeVisitData : List (EncounterData HomeVisitActivity)
    , ncdData : List (EncounterData NCDActivity)
    , nutritionIndividualData : List (EncounterData NutritionChildActivity)
    , nutritionGroupData : List (NutritionGroupEncounterData NutritionMotherActivity NutritionChildActivity)
    , tuberculosisData : List (EncounterData TuberculosisActivity)
    , wellChildData : List WellChildEncounterData
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


type alias WellChildEncounterData =
    { startDate : NominalDate
    , encounterType : WellChildEncounterType
    , completion : ActivitiesCompletionData WellChildActivity
    }


type WellChildEncounterType
    = PediatricCare
    | PediatricCareChw
    | NewbornExam


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


type ChildScoreboardActivity
    = ChildScoreboardNCDA
    | ChildScoreboardBCGImmunisation
    | ChildScoreboardDTPImmunisation
    | ChildScoreboardDTPSAImmunisation
    | ChildScoreboardIPVImmunisation
    | ChildScoreboardMRImmunisation
    | ChildScoreboardOPVImmunisation
    | ChildScoreboardPCV13Immunisation
    | ChildScoreboardRotarixImmunisation


type HomeVisitActivity
    = HomeVisitCaring
    | HomeVisitFeeding
    | HomeVisitFoodSecurity
    | HomeVisitHygiene


type NCDActivity
    = NCDCoreExam
    | NCDCoMorbidities
    | NCDCreatinineTest
    | NCDDangerSigns
    | NCDFamilyHistory
    | NCDFamilyPlanning
    | NCDHba1cTest
    | NCDHealthEducation
    | NCDHIVTest
    | NCDLipidPanelTest
    | NCDLiverFunctionTest
    | NCDMedicationDistribution
    | NCDMedicationHistory
    | NCDOutsideCare
    | NCDPregnancyTest
    | NCDRandomBloodSugarTest
    | NCDReferral
    | NCDSocialHistory
    | NCDSymptomReview
    | NCDUrineDipstickTest
    | NCDVitals
    | NCDCreatinineTestResult
    | NCDLipidPanelTestResult
    | NCDLiverFunctionTestResult
    | NCDRandomBloodSugarTestResult
    | NCDUrineDipstickTestResult


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
    | WellChildDTPSAImmunisation
    | WellChildECD
    | WellChildFeeding
    | WellChildFollowUp
    | WellChildFoodSecurity
    | WellChildHeadCircumference
    | WellChildHealthEducation
    | WellChildHeight
    | WellChildHPVImmunisation
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


type HIVActivity
    = HIVDiagnostics
    | HIVFollowUp
    | HIVHealthEducation
    | HIVMedication
    | HIVReferral
    | HIVSymptomReview
    | HIVTreatmentReview


type TuberculosisActivity
    = TuberculosisDiagnostics
    | TuberculosisDOT
    | TuberculosisFollowUp
    | TuberculosisHealthEducation
    | TuberculosisMedication
    | TuberculosisReferral
    | TuberculosisSymptomReview
    | TuberculosisTreatmentReview


type TakenBy
    = TakenByNurse
    | TakenByCHW
    | TakenByUnknown


type Msg
    = SetData Value
