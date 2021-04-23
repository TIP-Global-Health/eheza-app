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
    , LaboratoryData
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
    , PregnancyTestingForm
    , PrenatalPhotoData
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
import Measurement.Model exposing (DropZoneFile)
import Pages.Page exposing (Page)


type Msg
    = DropZoneComplete DropZoneFile
    | SetActivePage Page
    | SetAlertsDialogState Bool
      -- PregnancyDatingMsgs
    | ToggleDateSelector
    | SetLmpDate Date
    | SetLmpDateConfident Bool
    | SetLmpRange String
    | SavePregnancyDating IndividualEncounterParticipantId PersonId (Maybe ( LastMenstrualPeriodId, LastMenstrualPeriod ))
      -- HistoryMsgs
    | SetActiveHistoryTask HistoryTask
      -- HistoryMsgs, OB, Step 1
    | SetCurrentlyPregnant Bool
    | SetOBIntInput (Maybe Int -> ObstetricFormFirstStep -> ObstetricFormFirstStep) String
    | SaveOBHistoryStep1 PersonId (Maybe ( ObstetricHistoryId, ObstetricHistory ))
      -- HistoryMsgs, OB, Step 2
    | SetCSectionReason CSectionReason
    | SetNumberOfCSections String
    | SetOBBoolInput (Bool -> ObstetricFormSecondStep -> ObstetricFormSecondStep) Bool
    | SetPreviousDeliveryPeriod PreviousDeliveryPeriod
    | BackToOBHistoryStep1
    | SaveOBHistoryStep2 PersonId (Maybe ( ObstetricHistoryStep2Id, ObstetricHistoryStep2 )) (Maybe HistoryTask)
      -- HistoryMsgs, Medical
    | SetMedicalBoolInput (Bool -> MedicalHistoryForm -> MedicalHistoryForm) Bool
    | SaveMedicalHistory PersonId (Maybe ( MedicalHistoryId, MedicalHistory )) (Maybe HistoryTask)
      -- HistoryMsgs, Social
    | SetSocialBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool
    | SetSocialHivTestingResult String
    | SaveSocialHistory PersonId (Maybe ( SocialHistoryId, SocialHistory )) (Maybe HistoryTask)
      -- ExaminationMsgs
    | SetActiveExaminationTask ExaminationTask
      -- ExaminationMsgs, Vitals
    | SetVitalsIntMeasurement (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatMeasurement (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PersonId (Maybe ( VitalsId, Vitals )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Nutrition Assessment
    | SetNutritionAssessmentMeasurement (Maybe Float -> NutritionAssessmentForm -> NutritionAssessmentForm) String
    | SaveNutritionAssessment PersonId (Maybe ( PrenatalNutritionId, PrenatalNutrition )) (Maybe Float) (Maybe ExaminationTask)
      -- ExaminationMsgs, Core Physical Exam
    | SetCorePhysicalExamBoolInput (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) Bool
    | SetCorePhysicalExamHeart HeartCPESign
    | SetCorePhysicalExamNeck NeckCPESign
    | SetCorePhysicalExamLungs LungsCPESign
    | SetCorePhysicalExamAbdomen AbdomenCPESign
    | SetCorePhysicalExamHands HandsCPESign
    | SetCorePhysicalExamLegs LegsCPESign
    | SaveCorePhysicalExam PersonId (Maybe ( CorePhysicalExamId, CorePhysicalExam )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Obstetrical Exam
    | SetObstetricalExamBoolInput (Bool -> ObstetricalExamForm -> ObstetricalExamForm) Bool
    | SetObstetricalExamIntMeasurement (Maybe Int -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFloatMeasurement (Maybe Float -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFetalPresentation FetalPresentation
    | SetObstetricalExamCSectionScar CSectionScar
    | SaveObstetricalExam PersonId (Maybe ( ObstetricalExamId, ObstetricalExam )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Breast Exam
    | SetBreastExamBoolInput (Bool -> BreastExamForm -> BreastExamForm) Bool
    | SetBreastExamBreast BreastExamSign
    | SaveBreastExam PersonId (Maybe ( BreastExamId, BreastExam )) (Maybe ExaminationTask)
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
    | SaveFamilyPlanning PersonId (Maybe ( PrenatalFamilyPlanningId, PrenatalFamilyPlanning ))
      -- PatientProvisionsMsgs
    | SetActivePatientProvisionsTask PatientProvisionsTask
      -- PatientProvisionsMsgs, Medication
    | SetMedicationBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
    | SaveMedication PersonId (Maybe ( MedicationId, Medication )) (Maybe PatientProvisionsTask)
      -- PatientProvisionsMsgs, Resources
    | SetResourcesBoolInput (Bool -> ResourcesForm -> ResourcesForm) Bool
    | SaveResources PersonId (Maybe ( ResourceId, Resource )) (Maybe PatientProvisionsTask)
      -- DangerSignsMsgs
    | SetDangerSign DangerSign
    | SaveDangerSigns PersonId (Maybe ( DangerSignsId, DangerSigns ))
      -- PrenatalPhotoMsgs
    | SavePrenatalPhoto PersonId (Maybe PrenatalPhotoId) PhotoUrl
      -- LABORATORYMsgs
    | SetActiveLaboratoryTask LaboratoryTask
    | SetPregnancyTestResult String
    | SavePregnancyTesting PersonId (Maybe ( PregnancyTestId, PregnancyTest ))


type alias Model =
    { pregnancyDatingData : PregnancyDatingData
    , historyData : HistoryData
    , examinationData : ExaminationData
    , familyPlanningData : FamilyPlanningData
    , patientProvisionsData : PatientProvisionsData
    , dangerSignsData : DangerSignsData
    , prenatalPhotoData : PrenatalPhotoData
    , laboratoryData : LaboratoryData
    , showAlertsDialog : Bool
    }


emptyModel : Model
emptyModel =
    { pregnancyDatingData = emptyPregnancyDatingData
    , historyData = emptyHistoryData
    , examinationData = emptyExaminationData
    , familyPlanningData = emptyFamilyPlanningData
    , patientProvisionsData = emptyPatientProvisionsData
    , dangerSignsData = emptyDangerSignsData
    , prenatalPhotoData = emptyPrenatalPhotoData
    , laboratoryData = emptyLaboratoryData
    , showAlertsDialog = False
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


type alias LaboratoryData =
    { pregnancyTestingForm : PregnancyTestingForm
    , activeTask : LaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { pregnancyTestingForm = PregnancyTestingForm Nothing
    , activeTask = LaboratoryPregnancyTesting
    }


type LaboratoryTask
    = LaboratoryPregnancyTesting


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
    , termPregnancyDirty : Bool
    , preTermPregnancy : Maybe Int
    , preTermPregnancyDirty : Bool
    , stillbirthsAtTerm : Maybe Int
    , stillbirthsAtTermDirty : Bool
    , stillbirthsPreTerm : Maybe Int
    , stillbirthsPreTermDirty : Bool
    , abortions : Maybe Int
    , abortionsDirty : Bool
    , liveChildren : Maybe Int
    , liveChildrenDirty : Bool
    }


emptyObstetricFormFirstStep : ObstetricFormFirstStep
emptyObstetricFormFirstStep =
    { currentlyPregnant = Nothing
    , termPregnancy = Nothing
    , termPregnancyDirty = False
    , preTermPregnancy = Nothing
    , preTermPregnancyDirty = False
    , stillbirthsAtTerm = Nothing
    , stillbirthsAtTermDirty = False
    , stillbirthsPreTerm = Nothing
    , stillbirthsPreTermDirty = False
    , abortions = Nothing
    , abortionsDirty = False
    , liveChildren = Nothing
    , liveChildrenDirty = False
    }


type alias ObstetricFormSecondStep =
    { cSections : Maybe Int
    , cSectionsDirty : Bool
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
    , cSectionsDirty = False
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
    , mentalHealthHistory : Maybe Bool
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
    , mentalHealthHistory = Nothing
    }


type alias SocialHistoryForm =
    { accompaniedByPartner : Maybe Bool
    , partnerReceivedCounseling : Maybe Bool
    , partnerReceivedTesting : Maybe Bool
    , partnerTestingResult : Maybe SocialHistoryHivTestingResult
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    SocialHistoryForm Nothing Nothing Nothing Nothing


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
    , sysBloodPressureDirty : Bool
    , diaBloodPressure : Maybe Float
    , diaBloodPressureDirty : Bool
    , heartRate : Maybe Int
    , heartRateDirty : Bool
    , respiratoryRate : Maybe Int
    , respiratoryRateDirty : Bool
    , bodyTemperature : Maybe Float
    , bodyTemperatureDirty : Bool
    }


emptyVitalsForm : VitalsForm
emptyVitalsForm =
    { sysBloodPressure = Nothing
    , sysBloodPressureDirty = False
    , diaBloodPressure = Nothing
    , diaBloodPressureDirty = False
    , heartRate = Nothing
    , heartRateDirty = False
    , respiratoryRate = Nothing
    , respiratoryRateDirty = False
    , bodyTemperature = Nothing
    , bodyTemperatureDirty = False
    }


type alias NutritionAssessmentForm =
    { height : Maybe Float
    , heightDirty : Bool
    , weight : Maybe Float
    , weightDirty : Bool
    , muac : Maybe Float
    , muacDirty : Bool
    }


emptyNutritionAssessmentForm : NutritionAssessmentForm
emptyNutritionAssessmentForm =
    { height = Nothing
    , heightDirty = False
    , weight = Nothing
    , weightDirty = False
    , muac = Nothing
    , muacDirty = False
    }


type alias CorePhysicalExamForm =
    -- Needs to be redefined to use EverySet to allow multiple signs.
    { brittleHair : Maybe Bool
    , paleConjuctiva : Maybe Bool
    , neck : Maybe (List NeckCPESign)
    , heart : Maybe HeartCPESign
    , heartMurmur : Maybe Bool
    , lungs : Maybe (List LungsCPESign)
    , abdomen : Maybe (List AbdomenCPESign)
    , hands : Maybe (List HandsCPESign)
    , legs : Maybe (List LegsCPESign)
    }


emptyCorePhysicalExamForm : CorePhysicalExamForm
emptyCorePhysicalExamForm =
    { brittleHair = Nothing
    , paleConjuctiva = Nothing
    , neck = Nothing
    , heart = Nothing
    , heartMurmur = Nothing
    , lungs = Nothing
    , abdomen = Nothing
    , hands = Nothing
    , legs = Nothing
    }


type alias ObstetricalExamForm =
    { fundalHeight : Maybe Float
    , fundalHeightDirty : Bool
    , fetalPresentation : Maybe FetalPresentation
    , fetalMovement : Maybe Bool
    , fetalHeartRate : Maybe Int
    , fetalHeartRateDirty : Bool
    , cSectionScar : Maybe CSectionScar
    }


emptyObstetricalExamForm : ObstetricalExamForm
emptyObstetricalExamForm =
    { fundalHeight = Nothing
    , fundalHeightDirty = False
    , fetalPresentation = Nothing
    , fetalMovement = Nothing
    , fetalHeartRate = Nothing
    , fetalHeartRateDirty = False
    , cSectionScar = Nothing
    }


type alias BreastExamForm =
    -- Should be EverySet, since you can have more than one sign.
    { breast : Maybe (List BreastExamSign)
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


type alias PrenatalPhotoData =
    { url : Maybe PhotoUrl }


emptyPrenatalPhotoData : PrenatalPhotoData
emptyPrenatalPhotoData =
    { url = Nothing }


type alias PregnancyTestingForm =
    { pregnancyTestResult : Maybe PregnancyTestResult
    }
