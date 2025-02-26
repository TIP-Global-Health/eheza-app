module Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))

{-| This module provides types relating to the UI for presenting
nutrition activities.
-}


type AcuteIllnessActivity
    = AcuteIllnessSymptoms
    | AcuteIllnessPhysicalExam
    | AcuteIllnessPriorTreatment
    | AcuteIllnessLaboratory
    | AcuteIllnessNextSteps
    | AcuteIllnessOngoingTreatment
    | AcuteIllnessDangerSigns
