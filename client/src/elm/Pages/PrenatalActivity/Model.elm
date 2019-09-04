module Pages.PrenatalActivity.Model exposing
    ( BreastExamForm
    , CorePhysicalExamForm
    , DangerSignsData
    , DangerSignsForm
    , ExaminationData
    , ExaminationTask(..)
    , FamilyPlanningData
    , FamilyPlanningForm
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
    , ObstetricHistoryStep(..)
    , ObstetricalExamForm
    , PatientProvisionsData
    , PatientProvisionsTask(..)
    , PregnancyDatingData
    , PregnancyDatingForm
    , ResourcesForm
    , SocialHistoryForm
    , VitalsForm
    , decodeLmpRange
    , emptyModel
    , emptyObstetricFormFirstStep
    , emptyObstetricFormSecondStep
    , encodeLmpRange
    )

import Backend.Entities exposing (..)
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
    | SavePregnancyDating PrenatalEncounterId PersonId (Maybe ( LastMenstrualPeriodId, LastMenstrualPeriod ))
      -- HistoryMsgs
    | SetActiveHistoryTask HistoryTask
      -- HistoryMsgs, OB, Step 1
    | SetCurrentlyPregnant Bool
    | SetOBIntInput (Maybe Int -> ObstetricFormFirstStep -> ObstetricFormFirstStep) String
    | SaveOBHistoryStep1 PrenatalEncounterId PersonId (Maybe ( ObstetricHistoryId, ObstetricHistory ))
      -- HistoryMsgs, OB, Step 2
    | SetCSectionReason CSectionReason
    | SetNumberOfCSections String
    | SetOBBoolInput (Bool -> ObstetricFormSecondStep -> ObstetricFormSecondStep) Bool
    | SetPreviousDeliveryPeriod PreviousDeliveryPeriod
    | BackToOBHistoryStep1
    | SaveOBHistoryStep2 PrenatalEncounterId PersonId (Maybe ( ObstetricHistoryStep2Id, ObstetricHistoryStep2 ))
      -- HistoryMsgs, Medical
    | SetMedicalBoolInput (Bool -> MedicalHistoryForm -> MedicalHistoryForm) Bool
    | SaveMedicalHistory PrenatalEncounterId PersonId (Maybe ( MedicalHistoryId, MedicalHistory ))
      -- HistoryMsgs, Social
    | SetSocialBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool
    | SaveSocialHistory PrenatalEncounterId PersonId (Maybe ( SocialHistoryId, SocialHistory ))
      -- ExaminationMsgs
    | SetActiveExaminationTask ExaminationTask
      -- ExaminationMsgs, Vitals
    | SetVitalsIntMeasurement (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatMeasurement (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PrenatalEncounterId PersonId (Maybe ( VitalsId, Vitals ))
      -- ExaminationMsgs, Nutrition Assessment
    | SetNutritionAssessmentMeasurement (Maybe Float -> NutritionAssessmentForm -> NutritionAssessmentForm) String
    | SaveNutritionAssessment PrenatalEncounterId PersonId (Maybe ( PrenatalNutritionId, PrenatalNutrition ))
      -- ExaminationMsgs, Core Physical Exam
    | SetCorePhysicalExamBoolInput (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) Bool
    | SetCorePhysicalExamNeck NeckCPESign
    | SetCorePhysicalExamLungs LungsCPESign
    | SetCorePhysicalExamAbdomen AbdomenCPESign
    | SetCorePhysicalExamHands HandsCPESign
    | SetCorePhysicalExamLegs LegsCPESign
    | SaveCorePhysicalExam PrenatalEncounterId PersonId (Maybe ( CorePhysicalExamId, CorePhysicalExam ))
      -- ExaminationMsgs, Obstetrical Exam
    | SetObstetricalExamBoolInput (Bool -> ObstetricalExamForm -> ObstetricalExamForm) Bool
    | SetObstetricalExamIntMeasurement (Maybe Int -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFloatMeasurement (Maybe Float -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFetalPresentation FetalPresentation
    | SaveObstetricalExam PrenatalEncounterId PersonId (Maybe ( ObstetricalExamId, ObstetricalExam ))
      -- ExaminationMsgs, Breast Exam
    | SetBreastExamBoolInput (Bool -> BreastExamForm -> BreastExamForm) Bool
    | SetBreastExamBreast BreastExamSign
    | SaveBreastExam PrenatalEncounterId PersonId (Maybe ( BreastExamId, BreastExam ))
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
    | SaveFamilyPlanning PrenatalEncounterId PersonId (Maybe ( PrenatalFamilyPlanningId, PrenatalFamilyPlanning ))
      -- PatientProvisionsMsgs
    | SetActivePatientProvisionsTask PatientProvisionsTask
      -- PatientProvisionsMsgs, Medication
    | SetMedicationBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
    | SaveMedication PrenatalEncounterId PersonId (Maybe ( MedicationId, Medication ))
      -- PatientProvisionsMsgs, Resources
    | SetResourcesBoolInput (Bool -> ResourcesForm -> ResourcesForm) Bool
    | SaveResources PrenatalEncounterId PersonId (Maybe ( ResourceId, Resource ))
      -- DangerSignsMsgs
    | SetDangerSign DangerSign
    | SaveDangerSigns PrenatalEncounterId PersonId (Maybe ( DangerSignsId, DangerSigns ))


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
    { obstetricFormFirstStep : ObstetricFormFirstStep
    , obstetricFormSecondStep : ObstetricFormSecondStep
    , obstetricHistoryStep : ObstetricHistoryStep
    , medicalForm : MedicalHistoryForm
    , socialForm : SocialHistoryForm
    , activeTask : HistoryTask
    }


emptyHistoryData : HistoryData
emptyHistoryData =
    { obstetricFormFirstStep = emptyObstetricFormFirstStep
    , obstetricFormSecondStep = emptyObstetricFormSecondStep
    , obstetricHistoryStep = ObstetricHistoryFirstStep
    , medicalForm = emptyMedicalHistoryForm
    , socialForm = emptySocialHistoryForm
    , activeTask = Obstetric
    }


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , nutritionAssessmentForm : NutritionAssessmentForm
    , corePhysicalExamForm : CorePhysicalExamForm
    , obstetricalExamForm : ObstetricalExamForm
    , breastExamForm : BreastExamForm
    , activeTask : ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , nutritionAssessmentForm = emptyNutritionAssessmentForm
    , corePhysicalExamForm = emptyCorePhysicalExamForm
    , obstetricalExamForm = emptyObstetricalExamForm
    , breastExamForm = emptyBreastExamForm
    , activeTask = Vitals
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
    }


emptyPatientProvisionsData : PatientProvisionsData
emptyPatientProvisionsData =
    { medicationForm = emptyMedicationForm
    , resourcesForm = emptyResourcesForm
    , activeTask = Medication
    }


type alias DangerSignsData =
    { form : DangerSignsForm
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { form = emptyDangerSignsForm
    }


type ObstetricHistoryStep
    = ObstetricHistoryFirstStep
    | ObstetricHistorySecondStep


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
    , cSectionReason : Maybe CSectionReason
    , previousDeliveryPeriod : Maybe PreviousDeliveryPeriod
    , successiveAbortions : Maybe Bool
    , successivePrematureDeliveries : Maybe Bool
    , stillbornPreviousDelivery : Maybe Bool
    , babyDiedOnDayOfBirthPreviousDelivery : Maybe Bool
    , partialPlacentaPreviousDelivery : Maybe Bool
    , severeHemorrhagingPreviousDelivery : Maybe Bool
    , preeclampsiaPreviousPregnancy : Maybe Bool
    , convulsionsPreviousDelivery : Maybe Bool
    , convulsionsAndUnconsciousPreviousDelivery : Maybe Bool
    , gestationalDiabetesPreviousPregnancy : Maybe Bool
    , incompleteCervixPreviousPregnancy : Maybe Bool
    , rhNegative : Maybe Bool
    }


emptyObstetricFormSecondStep : ObstetricFormSecondStep
emptyObstetricFormSecondStep =
    { cSections = Nothing
    , cSectionInPreviousDelivery = Nothing
    , cSectionReason = Nothing
    , previousDeliveryPeriod = Nothing
    , successiveAbortions = Nothing
    , successivePrematureDeliveries = Nothing
    , stillbornPreviousDelivery = Nothing
    , babyDiedOnDayOfBirthPreviousDelivery = Nothing
    , partialPlacentaPreviousDelivery = Nothing
    , severeHemorrhagingPreviousDelivery = Nothing
    , preeclampsiaPreviousPregnancy = Nothing
    , convulsionsPreviousDelivery = Nothing
    , convulsionsAndUnconsciousPreviousDelivery = Nothing
    , gestationalDiabetesPreviousPregnancy = Nothing
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
    , heartRate : Maybe Int
    , respiratoryRate : Maybe Int
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
    , muac : Maybe Float
    }


emptyNutritionAssessmentForm : NutritionAssessmentForm
emptyNutritionAssessmentForm =
    { height = Nothing
    , weight = Nothing
    , muac = Nothing
    }


type alias CorePhysicalExamForm =
    -- Needs to be redefined to use EverySet to allow multiple signs.
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
    , fetalHeartRate : Maybe Int
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


type alias BreastExamForm =
    -- Should be EverySet, since you can have more than one sign.
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
    -- Should be EverySet
    { signs : Maybe (List DangerSign)
    }


emptyDangerSignsForm : DangerSignsForm
emptyDangerSignsForm =
    DangerSignsForm Nothing
