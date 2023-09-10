module Pages.NCD.Activity.Types exposing (ExaminationTask(..), MedicalHistoryTask(..), NextStepsTask(..))


type ExaminationTask
    = TaskCoreExam
    | TaskVitals


type MedicalHistoryTask
    = TaskCoMorbidities
    | TaskMedicationHistory
    | TaskSocialHistory
    | TaskFamilyHistory
    | TaskOutsideCare


type NextStepsTask
    = TaskHealthEducation
    | TaskMedicationDistribution
    | TaskReferral
