module Pages.PrenatalActivity.Model exposing
    ( BreastExamForm
    , CSectionReason(..)
    , CorePhysicalExamForm
    , DangerSign(..)
    , DangerSignsData
    , ExaminationData
    , ExaminationTask(..)
    , FamilyPlanningData
    , FetalPresentation(..)
    , HistoryData
    , HistoryTask(..)
    , LmpRange(..)
    , MedicalHistoryForm
    , MedicationForm
    , Model
    , Msg(..)
    , NutritionAssessmentForm
    , ObstetricFormFirstStep
    , ObstetricFormSecondStep
    , ObstetricHistoryFormType(..)
    , ObstetricalExamForm
    , PatientProvisionsData
    , PatientProvisionsTask(..)
    , PregnancyDatingData
    , PregnancyDatingForm
    , PreviousDeliveryPeriod(..)
    , ResourcesForm
    , SocialHistoryForm
    , VitalsForm
    , decodeLmpRange
    , emptyModel
    , emptyObstetricFormSecondStep
    , encodeLmpRange
    )

import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
      -- PregnancyDatingMsgs
    | ToggleDateSelector
    | SetLmpDate Date
    | SetLmpDateConfident Bool
    | SetLmpRange String
      -- HistoryMsgs
    | SetActiveHistoryTask HistoryTask
    | SetHistoryTaskCompleted
      -- HistoryMsgs, OB, Step 1
    | SetOBFirstStepCompleted
    | SetCurrentlyPregnant Bool
    | SetOBIntInput (Maybe Int -> ObstetricFormFirstStep -> ObstetricFormFirstStep) String
      -- HistoryMsgs, OB, Step 2
    | SetCSectionReason CSectionReason
    | SetNumberOfCSections String
    | SetOBBoolInput (Bool -> ObstetricFormSecondStep -> ObstetricFormSecondStep) Bool
    | SetPreviousDeliveryPeriod PreviousDeliveryPeriod
      -- HistoryMsgs, Medical
    | SetMedicalBoolInput (Bool -> MedicalHistoryForm -> MedicalHistoryForm) Bool
      -- HistoryMsgs, Social
    | SetSocialBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool
      -- ExaminationMsgs
    | SetActiveExaminationTask ExaminationTask
    | SetExaminationTaskCompleted
      -- ExaminationMsgs, Vitals
    | SetVitalsMeasurement (Maybe Float -> VitalsForm -> VitalsForm) String
      -- ExaminationMsgs, Nutrition Assessment
    | SetNutritionAssessmentMeasurement (Maybe Float -> NutritionAssessmentForm -> NutritionAssessmentForm) String
      -- ExaminationMsgs, Core Physical Exam
    | SetCorePhysicalExamBoolInput (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) Bool
    | SetCorePhysicalExamNeck NeckCPESign
    | SetCorePhysicalExamLungs LungsCPESign
    | SetCorePhysicalExamAbdomen AbdomenCPESign
    | SetCorePhysicalExamHands HandsCPESign
    | SetCorePhysicalExamLegs LegsCPESign
      -- ExaminationMsgs, Obstetrical Exam
    | SetObstetricalExamBoolInput (Bool -> ObstetricalExamForm -> ObstetricalExamForm) Bool
    | SetObstetricalExamMeasurement (Maybe Float -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFetalPresentation FetalPresentation
      -- ExaminationMsgs, Breast Exam
    | SetBreastExamBoolInput (Bool -> BreastExamForm -> BreastExamForm) Bool
    | SetBreastExamBreast BreastExamSign
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
      -- PatientProvisionsMsgs
    | SetActivePatientProvisionsTask PatientProvisionsTask
    | SetPatientProvisionsTaskCompleted
      -- PatientProvisionsMsgs, Medication
    | SetMedicationBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
      -- PatientProvisionsMsgs, Resources
    | SetResourcesBoolInput (Bool -> ResourcesForm -> ResourcesForm) Bool
      -- DangerSignsMsgs
    | SetDangerSign DangerSign


type alias Model =
    { pregnancyDatingData : PregnancyDatingData
    , historyData : HistoryData
    , examinationData : ExaminationData
    , familyPlanningData : FamilyPlanningData
    , patientProvisionsData : PatientProvisionsData
    , dangerSignsData : DangerSignsData
    }


emptyModel : Model
emptyModel =
    { pregnancyDatingData = emptyPregnancyDatingData
    , historyData = emptyHistoryData
    , examinationData = emptyExaminationData
    , familyPlanningData = emptyFamilyPlanningData
    , patientProvisionsData = emptyPatientProvisionsData
    , dangerSignsData = emptyDangerSignsData
    }


type alias PregnancyDatingData =
    { form : PregnancyDatingForm
    }


emptyPregnancyDatingData : PregnancyDatingData
emptyPregnancyDatingData =
    { form = emptyPregnancyDatingForm
    }


type alias HistoryData =
    { obstetricForm : ObstetricHistoryFormType
    , medicalForm : MedicalHistoryForm
    , socialForm : SocialHistoryForm
    , activeTask : HistoryTask
    , completedTasks : List HistoryTask
    }


emptyHistoryData : HistoryData
emptyHistoryData =
    { obstetricForm = FirstStep emptyObstetricFormFirstStep
    , medicalForm = emptyMedicalHistoryForm
    , socialForm = emptySocialHistoryForm
    , activeTask = Obstetric
    , completedTasks = []
    }


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , nutritionAssessmentForm : NutritionAssessmentForm
    , corePhysicalExamForm : CorePhysicalExamForm
    , obstetricalExamForm : ObstetricalExamForm
    , breastExamForm : BreastExamForm
    , activeTask : ExaminationTask
    , completedTasks : List ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , nutritionAssessmentForm = emptyNutritionAssessmentForm
    , corePhysicalExamForm = emptyCorePhysicalExamForm
    , obstetricalExamForm = emptyObstetricalExamForm
    , breastExamForm = emptyBreastExamForm
    , activeTask = Vitals
    , completedTasks = []
    }


type alias FamilyPlanningData =
    { form : FamilyPlanningForm
    }


emptyFamilyPlanningData : FamilyPlanningData
emptyFamilyPlanningData =
    { form = emptyFamilyPlanningForm
    }


type alias PatientProvisionsData =
    { medicationForm : MedicationForm
    , resourcesForm : ResourcesForm
    , activeTask : PatientProvisionsTask
    , completedTasks : List PatientProvisionsTask
    }


emptyPatientProvisionsData : PatientProvisionsData
emptyPatientProvisionsData =
    { medicationForm = emptyMedicationForm
    , resourcesForm = emptyResourcesForm
    , activeTask = Medication
    , completedTasks = []
    }


type alias DangerSignsData =
    { form : DangerSignsForm
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { form = emptyDangerSignsForm
    }


type ObstetricHistoryFormType
    = FirstStep ObstetricFormFirstStep
    | SecondStep ObstetricFormSecondStep


type HistoryTask
    = Obstetric
    | Medical
    | Social


type alias PregnancyDatingForm =
    { lmpRange : Maybe LmpRange
    , lmpDate : Maybe Date
    , lmpDateConfident : Maybe Bool
    , isDateSelectorOpen : Bool
    }


emptyPregnancyDatingForm : PregnancyDatingForm
emptyPregnancyDatingForm =
    PregnancyDatingForm Nothing Nothing Nothing False


type alias ObstetricFormFirstStep =
    { currentlyPregnant : Maybe Bool
    , termPregnancy : Maybe Int
    , preTermPregnancy : Maybe Int
    , stillbirthsAtTerm : Maybe Int
    , stillbirthsPreTerm : Maybe Int
    , abortions : Maybe Int
    , liveChildren : Maybe Int
    }


emptyObstetricFormFirstStep : ObstetricFormFirstStep
emptyObstetricFormFirstStep =
    { currentlyPregnant = Nothing
    , termPregnancy = Nothing
    , preTermPregnancy = Nothing
    , stillbirthsAtTerm = Nothing
    , stillbirthsPreTerm = Nothing
    , abortions = Nothing
    , liveChildren = Nothing
    }


type alias ObstetricFormSecondStep =
    { cSections : Maybe Int
    , cSectionInPreviousDelivery : Maybe Bool
    , reasonForCSection : Maybe CSectionReason
    , previousDeliveryPeriod : Maybe PreviousDeliveryPeriod
    , successiveAbortions : Maybe Bool
    , successivePrimatureDeliveries : Maybe Bool
    , stillbornPreviousDelivery : Maybe Bool
    , babyDiedOnDayOfBirthPreviousDelivery : Maybe Bool
    , partialPlacentaPreviousDelivery : Maybe Bool
    , severeHemorrhagingPreviousDelivery : Maybe Bool
    , preeclampsiaPreviousPregnancy : Maybe Bool
    , convulsionsPreviousDelivery : Maybe Bool
    , convulsionsAndUnconciousPreviousDelivery : Maybe Bool
    , gestatipnalDiabetesPreviousPregnancy : Maybe Bool
    , incompleteCervixPreviousPregnancy : Maybe Bool
    , rhNegative : Maybe Bool
    }


emptyObstetricFormSecondStep : ObstetricFormSecondStep
emptyObstetricFormSecondStep =
    { cSections = Nothing
    , cSectionInPreviousDelivery = Nothing
    , reasonForCSection = Nothing
    , previousDeliveryPeriod = Nothing
    , successiveAbortions = Nothing
    , successivePrimatureDeliveries = Nothing
    , stillbornPreviousDelivery = Nothing
    , babyDiedOnDayOfBirthPreviousDelivery = Nothing
    , partialPlacentaPreviousDelivery = Nothing
    , severeHemorrhagingPreviousDelivery = Nothing
    , preeclampsiaPreviousPregnancy = Nothing
    , convulsionsPreviousDelivery = Nothing
    , convulsionsAndUnconciousPreviousDelivery = Nothing
    , gestatipnalDiabetesPreviousPregnancy = Nothing
    , incompleteCervixPreviousPregnancy = Nothing
    , rhNegative = Nothing
    }


type alias MedicalHistoryForm =
    { uterineMyoma : Maybe Bool
    , diabetes : Maybe Bool
    , cardiacDisease : Maybe Bool
    , renalDisease : Maybe Bool
    , hypertensionBeforePregnancy : Maybe Bool
    , tuberculosisPast : Maybe Bool
    , tuberculosisPresent : Maybe Bool
    , asthma : Maybe Bool
    , bowedLegs : Maybe Bool
    , hiv : Maybe Bool
    }


emptyMedicalHistoryForm : MedicalHistoryForm
emptyMedicalHistoryForm =
    { uterineMyoma = Nothing
    , diabetes = Nothing
    , cardiacDisease = Nothing
    , renalDisease = Nothing
    , hypertensionBeforePregnancy = Nothing
    , tuberculosisPast = Nothing
    , tuberculosisPresent = Nothing
    , asthma = Nothing
    , bowedLegs = Nothing
    , hiv = Nothing
    }


type alias SocialHistoryForm =
    { accompaniedByPartner : Maybe Bool
    , partnerReceivedCounseling : Maybe Bool
    , mentalHealthHistory : Maybe Bool
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    SocialHistoryForm Nothing Nothing Nothing


type CSectionReason
    = Breech
    | Emergency
    | FailureToProgress
    | None
    | Other


type PreviousDeliveryPeriod
    = LessThan18Month
    | MoreThan5Years
    | Neither


type LmpRange
    = OneMonth
    | ThreeMonth
    | SixMonth


encodeLmpRange : LmpRange -> String
encodeLmpRange range =
    case range of
        OneMonth ->
            "one-month"

        ThreeMonth ->
            "three-month"

        SixMonth ->
            "six-month"


decodeLmpRange : String -> Maybe LmpRange
decodeLmpRange s =
    case s of
        "one-month" ->
            Just OneMonth

        "three-month" ->
            Just ThreeMonth

        "six-month" ->
            Just SixMonth

        _ ->
            Nothing


type ExaminationTask
    = BreastExam
    | CorePhysicalExam
    | NutritionAssessment
    | ObstetricalExam
    | Vitals


type alias VitalsForm =
    { sysBloodPressure : Maybe Float
    , diaBloodPressure : Maybe Float
    , heartRate : Maybe Float
    , respiratoryRate : Maybe Float
    , bodyTemperature : Maybe Float
    }


emptyVitalsForm : VitalsForm
emptyVitalsForm =
    { sysBloodPressure = Nothing
    , diaBloodPressure = Nothing
    , heartRate = Nothing
    , respiratoryRate = Nothing
    , bodyTemperature = Nothing
    }


type alias NutritionAssessmentForm =
    { height : Maybe Float
    , weight : Maybe Float
    , bmi : Maybe Float
    , muac : Maybe Float
    }


emptyNutritionAssessmentForm : NutritionAssessmentForm
emptyNutritionAssessmentForm =
    { height = Nothing
    , weight = Nothing
    , bmi = Nothing
    , muac = Nothing
    }


type alias CorePhysicalExamForm =
    { brittleHair : Maybe Bool
    , paleConjuctiva : Maybe Bool
    , neck : Maybe NeckCPESign
    , abnormalHeart : Maybe Bool
    , lungs : Maybe LungsCPESign
    , abdomen : Maybe AbdomenCPESign
    , hands : Maybe HandsCPESign
    , legs : Maybe LegsCPESign
    }


emptyCorePhysicalExamForm : CorePhysicalExamForm
emptyCorePhysicalExamForm =
    { brittleHair = Nothing
    , paleConjuctiva = Nothing
    , neck = Nothing
    , abnormalHeart = Nothing
    , lungs = Nothing
    , abdomen = Nothing
    , hands = Nothing
    , legs = Nothing
    }


type alias ObstetricalExamForm =
    { fundalHeight : Maybe Float
    , fetalPresentation : Maybe FetalPresentation
    , fetalMovement : Maybe Bool
    , fetalHeartRate : Maybe Float
    , cSectionScar : Maybe Bool
    }


emptyObstetricalExamForm : ObstetricalExamForm
emptyObstetricalExamForm =
    { fundalHeight = Nothing
    , fetalPresentation = Nothing
    , fetalMovement = Nothing
    , fetalHeartRate = Nothing
    , cSectionScar = Nothing
    }


type FetalPresentation
    = Transverse
    | Breach
    | Cephalic


type alias BreastExamForm =
    { breast : Maybe BreastExamSign
    , selfGuidance : Maybe Bool
    }


emptyBreastExamForm : BreastExamForm
emptyBreastExamForm =
    BreastExamForm Nothing Nothing


type alias FamilyPlanningForm =
    { signs : Maybe (List FamilyPlanningSign)
    }


emptyFamilyPlanningForm : FamilyPlanningForm
emptyFamilyPlanningForm =
    FamilyPlanningForm Nothing


type PatientProvisionsTask
    = Medication
    | Resources


type alias MedicationForm =
    { receivedIronFolicAcid : Maybe Bool
    , receivedDewormingPill : Maybe Bool
    }


emptyMedicationForm : MedicationForm
emptyMedicationForm =
    MedicationForm Nothing Nothing


type alias ResourcesForm =
    { receivedMosquitoNet : Maybe Bool
    }


emptyResourcesForm : ResourcesForm
emptyResourcesForm =
    ResourcesForm Nothing


type alias DangerSignsForm =
    { signs : Maybe (List DangerSign)
    }


emptyDangerSignsForm : DangerSignsForm
emptyDangerSignsForm =
    DangerSignsForm Nothing


type DangerSign
    = VaginalBleeding
    | HeadacheBlurredVision
    | Convulsions
    | AbdominalPain
    | DificultyBreathing
    | Fever
    | ExtremeWeakness
    | NoDangerSign
