module Pages.AcuteIllness.Activity.Types exposing (AILaboratoryTask(..), DangerSignsTask(..), ExposureTask(..), NextStepsTask(..), OngoingTreatmentTask(..), PhysicalExamTask(..), PriorTreatmentTask(..), SymptomReliefType(..), SymptomsTask(..))


type SymptomReliefType
    = SymptomReliefParacetamol
    | SymptomReliefVitaminC
    | SymptomReliefPaidoterineSyrup
    | SymptomReliefCoughMixture


type SymptomsTask
    = SymptomsGeneral
    | SymptomsRespiratory
    | SymptomsGI



-- PHYSICAL EXAM


type PhysicalExamTask
    = PhysicalExamVitals
    | PhysicalExamCoreExam
    | PhysicalExamMuac
    | PhysicalExamAcuteFindings
    | PhysicalExamNutrition



-- LABORATORY


type AILaboratoryTask
    = LaboratoryMalariaTesting
    | LaboratoryCovidTesting



-- EXPOSURE


type ExposureTask
    = ExposureTravel
    | ExposureExposure



-- PRIOR TREATMENT


type PriorTreatmentTask
    = TreatmentReview



-- NEXT STEPS


type NextStepsTask
    = NextStepsMedicationDistribution
    | NextStepsSendToHC
    | NextStepsHealthEducation
    | NextStepsFollowUp
    | NextStepsContactTracing
    | NextStepsSymptomsReliefGuidance



-- ONGOING TREATMENT


type OngoingTreatmentTask
    = OngoingTreatmentReview



-- DANGER SIGNS


type DangerSignsTask
    = ReviewDangerSigns
