module Pages.Prenatal.Activity.Types exposing (..)


type ExaminationTask
    = BreastExam
    | CorePhysicalExam
    | NutritionAssessment
    | ObstetricalExam
    | Vitals


type HistoryTask
    = Obstetric
    | Medical
    | Social


type LmpRange
    = OneMonth
    | ThreeMonth
    | SixMonth


type NextStepsTask
    = NextStepsAppointmentConfirmation
    | NextStepsFollowUp
    | NextStepsSendToHC
    | NextStepsHealthEducation
    | NextStepsNewbornEnrolment
    | NextStepsMedicationDistribution
    | NextStepsRecommendedTreatment
    | NextStepsWait


type PatientProvisionsTask
    = Medication
    | Resources


type LaboratoryTask
    = TaskBloodGpRsTest
    | TaskHemoglobinTest
    | TaskHepatitisBTest
    | TaskHIVTest
    | TaskMalariaTest
    | TaskRandomBloodSugarTest
    | TaskSyphilisTest
    | TaskUrineDipstickTest
