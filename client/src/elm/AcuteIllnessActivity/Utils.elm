module AcuteIllnessActivity.Utils exposing (decodeActivityFromString, encodeActivityAsString, getActivityIcon, getAllActivities)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import AcuteIllnessActivity.Model exposing (..)
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

        AcuteIllnessLaboratory ->
            "laboratory"

        AcuteIllnessExposure ->
            "exposure"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe AcuteIllnessActivity
decodeActivityFromString s =
    case s of
        "symptoms" ->
            Just AcuteIllnessSymptoms

        "physical-exam" ->
            Just AcuteIllnessPhysicalExam

        "laboratory" ->
            Just AcuteIllnessLaboratory

        "exposure" ->
            Just AcuteIllnessExposure

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


getAllActivities : List AcuteIllnessActivity
getAllActivities =
    [ AcuteIllnessSymptoms, AcuteIllnessPhysicalExam, AcuteIllnessLaboratory, AcuteIllnessExposure ]
