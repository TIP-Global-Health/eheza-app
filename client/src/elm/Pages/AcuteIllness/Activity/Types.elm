module Pages.AcuteIllness.Activity.Types exposing (..)


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
    = NextStepsIsolation
    | NextStepsContactHC
    | NextStepsCall114
    | NextStepsMedicationDistribution
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
