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


type PatientProvisionsTask
    = Medication
    | Resources


type SymptomReviewStep
    = SymptomReviewStepSymptoms
    | SymptomReviewStepQuestions


type LegCrampsReliefMethod
    = ReliefMethodMuscleStretching
    | ReliefMethodDorsiflexion
    | ReliefMethodRelaxation
    | ReliefMethodSleepWithPillowBetweenLegs
    | ReliefMethodHeatTherapy
    | ReliefMethodMassage


type HeartburnReliefMethod
    = ReliefMethodAvoidLargeMeals
    | ReliefMethodCeaseSmoking
    | ReliefMethodAvoidAlcohom
    | ReliefMethodSleepWithHeadRaised


type EarlyMastitisOrEngorgmentReliefMethod
    = ReliefMethodBreastMassage
    | ReliefMethodIncreaseFluid
    | ReliefMethodBreastfeedingOrHandExpression


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
