module PrenatalActivity.Utils exposing
    ( decodeActivityFromString
    , defaultActivity
    , encodeActivityAsString
    , getActivityIcon
    , getAllActivities
    )

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Backend.Entities exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import EverySet
import Gizra.NominalDate exposing (diffDays)
import Maybe.Extra exposing (isJust, isNothing)
import PrenatalActivity.Model exposing (..)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
encodeActivityAsString : PrenatalActivity -> String
encodeActivityAsString activity =
    case activity of
        _ ->
            ""


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe PrenatalActivity
decodeActivityFromString s =
    case s of
        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivity : PrenatalActivity
defaultActivity =
    PregnancyDating


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : PrenatalActivity -> String
getActivityIcon activity =
    case activity of
        DangerSigns ->
            "danger-signs"

        Examination ->
            "examination"

        FamilyPlanning ->
            "planning"

        History ->
            "history"

        PatientProvisions ->
            "patient-provisions"

        PregnancyDating ->
            "pregnancy-dating"


getAllActivities : List PrenatalActivity
getAllActivities =
    [ PregnancyDating, History, Examination, FamilyPlanning, PatientProvisions, DangerSigns ]
