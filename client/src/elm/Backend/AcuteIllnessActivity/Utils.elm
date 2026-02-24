module Backend.AcuteIllnessActivity.Utils exposing (activityFromString, activityToString, getActivityIcon, getAllActivities)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Backend.AcuteIllnessActivity.Model exposing (..)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
activityToString : AcuteIllnessActivity -> String
activityToString activity =
    case activity of
        AcuteIllnessSymptoms ->
            "symptoms"

        AcuteIllnessPhysicalExam ->
            "physical-exam"

        AcuteIllnessPriorTreatment ->
            "prior-treatment"

        AcuteIllnessLaboratory ->
            "laboratory"

        AcuteIllnessNextSteps ->
            "next-steps"

        AcuteIllnessOngoingTreatment ->
            "ongoing-treatment"

        AcuteIllnessDangerSigns ->
            "danger-signs"


{-| The inverse of encodeActivityTypeAsString
-}
activityFromString : String -> Maybe AcuteIllnessActivity
activityFromString s =
    case s of
        "symptoms" ->
            Just AcuteIllnessSymptoms

        "physical-exam" ->
            Just AcuteIllnessPhysicalExam

        "prior-treatment" ->
            Just AcuteIllnessPriorTreatment

        "laboratory" ->
            Just AcuteIllnessLaboratory

        "next-steps" ->
            Just AcuteIllnessNextSteps

        "ongoing-treatment" ->
            Just AcuteIllnessOngoingTreatment

        "danger-signs" ->
            Just AcuteIllnessDangerSigns

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : AcuteIllnessActivity -> String
getActivityIcon activity =
    activityToString activity


getAllActivities : Bool -> List AcuteIllnessActivity
getAllActivities isFirstEncounter =
    if isFirstEncounter then
        [ AcuteIllnessSymptoms, AcuteIllnessPriorTreatment, AcuteIllnessPhysicalExam, AcuteIllnessLaboratory, AcuteIllnessNextSteps ]

    else
        [ AcuteIllnessDangerSigns, AcuteIllnessPhysicalExam, AcuteIllnessOngoingTreatment, AcuteIllnessLaboratory, AcuteIllnessNextSteps ]
