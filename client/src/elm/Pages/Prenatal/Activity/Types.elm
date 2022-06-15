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
    | OutsideCare


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
    | TaskHIVPCRTest


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


type TreatmentReviewTask
    = TreatmentReviewPrenatalMedication
    | TreatmentReviewHIV
    | TreatmentReviewHypertension
    | TreatmentReviewMalaria
    | TreatmentReviewAnemia
    | TreatmentReviewSyphilis


type OutsideCareStep
    = OutsideCareStepDiagnoses
    | OutsideCareStepMedications


type ImmunisationTask
    = TaskTetanus
