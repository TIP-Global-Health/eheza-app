module Pages.Prenatal.RecurrentActivity.Types exposing (ExaminationTask(..), NextStepsTask(..))


type NextStepsTask
    = NextStepsSendToHC
    | NextStepsMedicationDistribution
    | NextStepsHealthEducation


type ExaminationTask
    = ExaminationVitals
