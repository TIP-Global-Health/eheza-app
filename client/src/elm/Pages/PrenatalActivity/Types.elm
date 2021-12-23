module Pages.PrenatalActivity.Types exposing (..)


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


type PatientProvisionsTask
    = Medication
    | Resources
