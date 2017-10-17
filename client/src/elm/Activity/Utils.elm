module Activity.Utils
    exposing
        ( getActivityList
        , getActivityTypeList
        , getActivityIdentity
        , getTotalsNumberPerActivity
        , participantHasPendingActivity
        , hasAnyPendingChildActivity
        , hasAnyPendingMotherActivity
        , hasCompletedChildActivity
        , hasCompletedMotherActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Backend.Child.Model exposing (Child)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getEditableChildMeasurements, getEditableMotherMeasurements)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (..)
import Dict exposing (Dict)
import EveryDict
import EveryDictList
import Maybe.Extra exposing (isNothing)
import Participant.Model exposing (Participant(..), ParticipantId(..), ParticipantTypeFilter(..))
import StorageKey exposing (StorageKey, isNew)


getActivityTypeList : ParticipantTypeFilter -> List ActivityType
getActivityTypeList participantTypeFilter =
    let
        childrenActivities =
            [ Activity.Model.Child ChildPicture
            , Activity.Model.Child Weight
            , Activity.Model.Child Height
            , Activity.Model.Child Muac
            , Activity.Model.Child NutritionSigns
            ]

        mothersActivities =
            [ Activity.Model.Mother FamilyPlanning
            ]
    in
        case participantTypeFilter of
            All ->
                childrenActivities ++ mothersActivities

            Children ->
                childrenActivities

            Mothers ->
                mothersActivities


{-| Get the pending and completed activities.

-}
getActivityList : ParticipantTypeFilter -> EditableSession -> List ActivityListItem
getActivityList participantTypeFilter session =
    List.map
        (\activityType ->
            { activity = getActivityIdentity activityType
            , totals = getTotalsNumberPerActivity activityType session
            }
        )
        (getActivityTypeList participantTypeFilter)


getActivityIdentity : ActivityType -> ActivityIdentity
getActivityIdentity activityType =
    let
        identityVal =
            case activityType of
                Child childActivityType ->
                    case childActivityType of
                        ChildPicture ->
                            ActivityIdentity "Photo" "photo"

                        Height ->
                            ActivityIdentity "Height" "height"

                        Weight ->
                            ActivityIdentity "Weight" "weight"

                        Muac ->
                            ActivityIdentity "MUAC" "muac"

                        NutritionSigns ->
                            ActivityIdentity "Nutrition" "nutrition"

                        ProgressReport ->
                            ActivityIdentity "Progress reports" "bar chart"

                Mother motherActivityType ->
                    case motherActivityType of
                        FamilyPlanning ->
                            ActivityIdentity "Family planning" "planning"
    in
        identityVal activityType


getAllChildActivities : List ChildActivityType
getAllChildActivities =
    [ ChildPicture, Height, Muac, NutritionSigns, ProgressReport, Weight ]


getAllMotherActivities : List MotherActivityType
getAllMotherActivities =
    [ FamilyPlanning ]


{-| Given an activity, how many of those measurements should we expect, and how
many are still pending?

TODO: We'll need to modify this to take into account which people are actually present,
once we've got that in the data model.
-}
getTotalsNumberPerActivity : ActivityType -> EditableSession -> { pending : Int, total : Int }
getTotalsNumberPerActivity activityType session =
    case activityType of
        Child childType ->
            let
                -- Until we have data about who is actually present, the total would be
                -- everyone who is in the session. (Eventually, we may filter this).
                total =
                    EveryDict.size session.offlineSession.children

                -- It's actually eaiser to count the completed ones, so we do that and
                -- just subtract to get pending.
                completed =
                    session.editableMeasurements.children
                        |> EveryDict.filter (always (hasCompletedChildActivity childType))
                        |> EveryDict.size
            in
                { pending = total - completed
                , total = total
                }

        Mother motherType ->
            let
                -- Until we have data about who is actually present, the total would be
                -- everyone who is in the session. (Eventually, we may filter this).
                total =
                    EveryDictList.size session.offlineSession.mothers

                -- It's actually eaiser to count the completed ones, so we do that and
                -- just subtract to get pending.
                completed =
                    session.editableMeasurements.mothers
                        |> EveryDict.filter (always (hasCompletedMotherActivity motherType))
                        |> EveryDict.size
            in
                { pending = total - completed
                , total = total
                }


hasCompletedChildActivity : ChildActivityType -> EditableChildMeasurements -> Bool
hasCompletedChildActivity activityType measurements =
    case activityType of
        ChildPicture ->
            isCompleted measurements.photo

        Height ->
            isCompleted measurements.height

        Weight ->
            isCompleted measurements.weight

        Muac ->
            isCompleted measurements.muac

        NutritionSigns ->
            isCompleted measurements.nutrition

        ProgressReport ->
            -- Hmm. This isn't really a measurement, so if we get it, we'll say
            -- it's not "completed".
            --
            -- TODO: I suppose that if we're tracking "activities" for UI purposes,
            -- perhaps the activity here is just looking at the progress report?
            -- So, that would imply some local data that tracks whether we've looked
            -- at the progress report?
            False


hasCompletedMotherActivity : MotherActivityType -> EditableMotherMeasurements -> Bool
hasCompletedMotherActivity activityType measurements =
    case activityType of
        FamilyPlanning ->
            isCompleted measurements.familyPlanning


{-| Should some measurement be considered completed? Note that this means that it has
been entered locally, not that it has been saved to the backend.
-}
isCompleted : EditableEntity key value -> Bool
isCompleted entity =
    case entity of
        NotFound ->
            False

        New _ ->
            True

        Unedited _ _ ->
            True

        Edited _ _ _ ->
            True

        Deleted _ _ ->
            False


hasAnyPendingMotherActivity : EditableMotherMeasurements -> Bool
hasAnyPendingMotherActivity measurements =
    getAllMotherActivities
        |> List.any ((flip hasCompletedMotherActivity) measurements >> not)


hasAnyPendingChildActivity : EditableChildMeasurements -> Bool
hasAnyPendingChildActivity measurements =
    getAllChildActivities
        |> List.any ((flip hasCompletedChildActivity) measurements >> not)


{-| Just looking at the single examination for the moment
Will need to parameterize when we have more than one.
-}
participantHasPendingActivity : ParticipantId -> EditableMeasurements -> Bool
participantHasPendingActivity participantId measurements =
    case participantId of
        ParticipantChildId childId ->
            getEditableChildMeasurements childId measurements
                |> hasAnyPendingChildActivity

        ParticipantMotherId motherId ->
            getEditableMotherMeasurements motherId measurements
                |> hasAnyPendingMotherActivity
