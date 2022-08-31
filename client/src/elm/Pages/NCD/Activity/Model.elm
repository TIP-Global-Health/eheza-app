module Pages.NCD.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Measurement.Model
    exposing
        ( CorePhysicalExamForm
        , FamilyPlanningForm
        , VitalsForm
        , emptyCorePhysicalExamForm
        , emptyFamilyPlanningForm
        , emptyVitalsForm
        )
import Pages.NCD.Activity.Types exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
      -- DangerSignsMsgs
    | SetDangerSign NCDDangerSign
    | SaveDangerSigns PersonId (Maybe ( NCDDangerSignsId, NCDDangerSigns ))
      -- SymptomReviewMsgs
    | SetGroup1Symptom NCDGroup1Symptom
    | SetGroup2Symptom NCDGroup2Symptom
    | SetPainSymptom NCDPainSymptom
    | SaveSymptomReview PersonId (Maybe ( NCDSymptomReviewId, NCDSymptomReview ))
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
    | SaveFamilyPlanning PersonId (Maybe ( NCDFamilyPlanningId, NCDFamilyPlanning ))
      -- ExaminationMsgs
    | SetActiveExaminationTask ExaminationTask
      -- ExaminationMsgs, Vitals
    | SetVitalsIntInput (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatInput (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PersonId (Maybe ( NCDVitalsId, NCDVitals )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Core Exam
    | SetCoreExamBoolInput (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) Bool
    | SetCoreExamHeart HeartCPESign
    | SetCoreExamNeck NeckCPESign
    | SetCoreExamLungs LungsCPESign
    | SetCoreExamAbdomen AbdomenCPESign
    | SetCoreExamHands HandsCPESign
    | SetCoreExamLegs LegsCPESign
    | SaveCoreExam PersonId (Maybe ( NCDCoreExamId, NCDCoreExam )) (Maybe ExaminationTask)
      -- MedicalHistoryMsgs
    | SetActiveMedicalHistoryTask MedicalHistoryTask
    | SetMedicalCondition MedicalCondition
    | SaveCoMorbidities PersonId (Maybe ( NCDCoMorbiditiesId, NCDCoMorbidities )) (Maybe MedicalHistoryTask)
    | SetMedicationCausingHypertension MedicationCausingHypertension
    | SetMedicationTreatingHypertension MedicationTreatingHypertension
    | SetMedicationTreatingDiabetes MedicationTreatingDiabetes
    | SaveMedicationHistory PersonId (Maybe ( NCDMedicationHistoryId, NCDMedicationHistory )) (Maybe MedicalHistoryTask)
    | SaveSocialHistory PersonId (Maybe ( NCDSocialHistoryId, NCDSocialHistory )) (Maybe MedicalHistoryTask)
    | SaveFamilyHistory PersonId (Maybe ( NCDFamilyHistoryId, NCDFamilyHistory )) (Maybe MedicalHistoryTask)
    | SaveOutsideCare PersonId (Maybe ( NCDOutsideCareId, NCDOutsideCare )) (Maybe MedicalHistoryTask)


type alias Model =
    { dangerSignsData : DangerSignsData
    , symptomReviewData : SymptomReviewData
    , examinationData : ExaminationData
    , familyPlanningData : FamilyPlanningData
    , medicalHistoryData : MedicalHistoryData
    }


emptyModel : Model
emptyModel =
    { dangerSignsData = emptyDangerSignsData
    , symptomReviewData = emptySymptomReviewData
    , examinationData = emptyExaminationData
    , familyPlanningData = emptyFamilyPlanningData
    , medicalHistoryData = emptyMedicalHistoryData
    }


type alias DangerSignsData =
    { form : DangerSignsForm
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { form = emptyDangerSignsForm
    }


type alias DangerSignsForm =
    { signs : Maybe (List NCDDangerSign)
    }


emptyDangerSignsForm : DangerSignsForm
emptyDangerSignsForm =
    DangerSignsForm Nothing


type alias SymptomReviewData =
    { form : SymptomReviewForm
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    }


type alias SymptomReviewForm =
    { group1Symptoms : Maybe (List NCDGroup1Symptom)
    , group2Symptoms : Maybe (List NCDGroup2Symptom)
    , painSymptoms : Maybe (List NCDPainSymptom)
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    SymptomReviewForm Nothing Nothing Nothing


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , coreExamForm : CorePhysicalExamForm
    , activeTask : Maybe ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , coreExamForm = emptyCorePhysicalExamForm
    , activeTask = Nothing
    }


type alias FamilyPlanningData =
    { form : FamilyPlanningForm
    }


emptyFamilyPlanningData : FamilyPlanningData
emptyFamilyPlanningData =
    { form = emptyFamilyPlanningForm
    }


type alias MedicalHistoryData =
    { coMorbiditiesForm : CoMorbiditiesForm
    , familyHistoryForm : FamilyHistoryForm
    , medicationHistoryForm : MedicationHistoryForm
    , socialHistoryForm : SocialHistoryForm
    , activeTask : Maybe MedicalHistoryTask
    }


emptyMedicalHistoryData : MedicalHistoryData
emptyMedicalHistoryData =
    { coMorbiditiesForm = emptyCoMorbiditiesForm
    , familyHistoryForm = emptyFamilyHistoryForm
    , medicationHistoryForm = emptyMedicationHistoryForm
    , socialHistoryForm = emptySocialHistoryForm
    , activeTask = Nothing
    }


type alias CoMorbiditiesForm =
    { conditions : Maybe (List MedicalCondition)
    }


emptyCoMorbiditiesForm : CoMorbiditiesForm
emptyCoMorbiditiesForm =
    CoMorbiditiesForm Nothing


type alias FamilyHistoryForm =
    { hypertensionInFamily : Maybe Bool
    , heartProblemInFamily : Maybe Bool
    , diabetesInFamily : Maybe Bool
    , hypertensionPredecessors : Maybe (List Predecessor)
    , hypertensionPredecessorsDirty : Bool
    , heartProblemPredecessors : Maybe (List Predecessor)
    , heartProblemPredecessorsDirty : Bool
    , diabetesPredecessors : Maybe (List Predecessor)
    , diabetesPredecessorsDirty : Bool
    }


emptyFamilyHistoryForm : FamilyHistoryForm
emptyFamilyHistoryForm =
    { hypertensionInFamily = Nothing
    , heartProblemInFamily = Nothing
    , diabetesInFamily = Nothing
    , hypertensionPredecessors = Nothing
    , hypertensionPredecessorsDirty = False
    , heartProblemPredecessors = Nothing
    , heartProblemPredecessorsDirty = False
    , diabetesPredecessors = Nothing
    , diabetesPredecessorsDirty = False
    }


type alias MedicationHistoryForm =
    { medicationsCausingHypertension : Maybe (List MedicationCausingHypertension)
    , medicationsTreatingHypertension : Maybe (List MedicationTreatingHypertension)
    , medicationsTreatingDiabetes : Maybe (List MedicationTreatingDiabetes)
    }


emptyMedicationHistoryForm : MedicationHistoryForm
emptyMedicationHistoryForm =
    MedicationHistoryForm Nothing Nothing Nothing


type alias SocialHistoryForm =
    { alcohol : Maybe Bool
    , cigarettes : Maybe Bool
    , salt : Maybe Bool
    , difficult4Times : Maybe Bool
    , helpAtHome : Maybe Bool
    , foodGroup : Maybe FoodGroup
    , beveragesPerWeek : Maybe Int
    , beveragesPerWeekDirty : Bool
    , cigarettesPerWeek : Maybe Int
    , cigarettesPerWeekDirty : Bool
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    { alcohol = Nothing
    , cigarettes = Nothing
    , salt = Nothing
    , difficult4Times = Nothing
    , helpAtHome = Nothing
    , foodGroup = Nothing
    , beveragesPerWeek = Nothing
    , beveragesPerWeekDirty = False
    , cigarettesPerWeek = Nothing
    , cigarettesPerWeekDirty = False
    }
