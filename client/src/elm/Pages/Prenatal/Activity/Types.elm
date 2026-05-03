module Pages.Prenatal.Activity.Types exposing (ExaminationTask(..), GWGClassification(..), HistoryTask(..), ImmunisationTask(..), MedicationTask(..), NextStepsTask(..), ObstetricHistoryStep(..), PrePregnancyClassification(..), SymptomReviewStep(..), TreatmentReviewTask(..), WarningPopupType(..))


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
    | NextStepsNextVisit


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
    | TaskOverview


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
    | TaskFefol
    | TaskFolate
    | TaskIron
    | TaskMebendazole
    | TaskMMS


type PrePregnancyClassification
    = PrePregnancyUnderWeight
    | PrePregnancyNormal
    | PrePregnancyOverweight
    | PrePregnancyObesity


type GWGClassification
    = GWGSeverelyInadequate
    | GWGInadequate
    | GWGAdequate
    | GWGExcessive
