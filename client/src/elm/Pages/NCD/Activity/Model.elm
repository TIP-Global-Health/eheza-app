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


type alias Model =
    { dangerSignsData : DangerSignsData
    , symptomReviewData : SymptomReviewData
    , examinationData : ExaminationData
    , familyPlanningData : FamilyPlanningData
    }


emptyModel : Model
emptyModel =
    { dangerSignsData = emptyDangerSignsData
    , symptomReviewData = emptySymptomReviewData
    , examinationData = emptyExaminationData
    , familyPlanningData = emptyFamilyPlanningData
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
