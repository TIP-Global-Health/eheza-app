module Pages.NCD.Activity.Types exposing (..)


type ExaminationTask
    = TaskCoreExam
    | TaskVitals


type MedicalHistoryTask
    = TaskCoMorbidities
    | TaskMedicationHistory
    | TaskSocialHistory
    | TaskFamilyHistory
    | TaskOutsideCare


type LaboratoryTask
    = TaskRandomBloodSugarTest
    | TaskCreatinineTest
    | TaskUrineDipstickTest
    | TaskHIVTest
    | TaskPregnancyTest
    | TaskLiverFunctionTest


type NextStepsTask
    = TaskHealthEducation
    | TaskMedicationDistribution
    | TaskReferral
