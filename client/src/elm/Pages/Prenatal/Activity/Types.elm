module Pages.Prenatal.Activity.Types exposing (..)


type ExaminationTask
    = BreastExam
    | CorePhysicalExam
    | NutritionAssessment
    | ObstetricalExam
    | Vitals
    | GUExam


type HistoryTask
    = Obstetric
    | Medical
    | Social
    | OutsideCare


type LmpRange
    = OneMonth
    | ThreeMonths
    | SixMonthsOrMore


type NextStepsTask
    = NextStepsAppointmentConfirmation
    | NextStepsFollowUp
    | NextStepsSendToHC
    | NextStepsHealthEducation
    | NextStepsNewbornEnrolment
    | NextStepsMedicationDistribution
    | NextStepsWait


type SymptomReviewStep
    = SymptomReviewStepSymptoms
    | SymptomReviewStepQuestions


type TreatmentReviewTask
    = TreatmentReviewPrenatalMedication
    | TreatmentReviewHIV
    | TreatmentReviewHypertension
    | TreatmentReviewMalaria
    | TreatmentReviewAnemia
    | TreatmentReviewSyphilis


type ImmunisationTask
    = TaskTetanus


type WarningPopupType msg
    = WarningPopupUrgent ( String, String )
    | WarningPopupRegular
    | WarningPopupTuberculosis
    | WarningPopupMentalHealth msg
    | WarningPopupTreatmentReview msg
    | WarningPopupVitaminA msg


type ObstetricHistoryStep
    = ObstetricHistoryFirstStep
    | ObstetricHistorySecondStep
