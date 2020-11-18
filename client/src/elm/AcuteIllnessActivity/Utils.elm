module AcuteIllnessActivity.Utils exposing (decodeActivityFromString, defaultActivity, encodeActivityAsString, getActivityIcon, getAllActivities)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import AcuteIllnessActivity.Model exposing (..)
import AssocList as Dict exposing (Dict)
import Translate exposing (Language, translate)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
encodeActivityAsString : AcuteIllnessActivity -> String
encodeActivityAsString activity =
    case activity of
        AcuteIllnessSymptoms ->
            "symptoms"

        AcuteIllnessPhysicalExam ->
            "physical-exam"

        AcuteIllnessPriorTreatment ->
            "prior-treatment"

        AcuteIllnessLaboratory ->
            "laboratory"

        AcuteIllnessExposure ->
            "exposure"

        AcuteIllnessNextSteps ->
            "next-steps"

        AcuteIllnessOngoingTreatment ->
            "ongoing-treatment"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe AcuteIllnessActivity
decodeActivityFromString s =
    case s of
        "symptoms" ->
            Just AcuteIllnessSymptoms

        "physical-exam" ->
            Just AcuteIllnessPhysicalExam

        "prior-treatment" ->
            Just AcuteIllnessPriorTreatment

        "laboratory" ->
            Just AcuteIllnessLaboratory

        "exposure" ->
            Just AcuteIllnessExposure

        "next-steps" ->
            Just AcuteIllnessNextSteps

        "ongoing-treatment" ->
            Just AcuteIllnessOngoingTreatment

        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivity : AcuteIllnessActivity
defaultActivity =
    AcuteIllnessSymptoms


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : AcuteIllnessActivity -> String
getActivityIcon activity =
    encodeActivityAsString activity


getAllActivities : Bool -> List AcuteIllnessActivity
getAllActivities isFirstEncounter =
    if isFirstEncounter then
        [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPriorTreatment, AcuteIllnessPhysicalExam, AcuteIllnessLaboratory, AcuteIllnessNextSteps ]

    else
        [ AcuteIllnessSymptoms, AcuteIllnessOngoingTreatment, AcuteIllnessPhysicalExam ]
