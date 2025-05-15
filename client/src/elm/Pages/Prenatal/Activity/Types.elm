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


type MedicationTask
    = TaskCalcium
    | TaskFolate
    | TaskIron
    | TaskMMS
    | TaskMebendazole


type PrePregnancyClassification
    = PrePregnancySevereUnderWeight
    | PrePregnancyUnderWeight
    | PrePregnancyNormal
    | PrePregnancyOverweight
    | PrePregnancyObesity


type GWGClassification
    = GWGSeverelyInadequate
    | GWGInadequate
    | GWGAdequate
    | GWGExcessive
