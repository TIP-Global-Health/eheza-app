module Activity.Utils
    exposing
        ( getActivityList
        , getActivityTypeList
        , getActivityIdentity
        , getTotalsNumberPerActivity
        , hasAnyPendingChildActivity
        , hasAnyPendingMotherActivity
        , hasCompletedChildActivity
        , hasCompletedMotherActivity
        , motherOrAnyChildHasAnyPendingActivity
        , participantHasPendingActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (applyEdit)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (..)
import Backend.Session.Utils exposing (getMother, getMotherMeasurementData, getChildMeasurementData)
import Dict exposing (Dict)
import EveryDict
import EveryDictList
import Maybe.Extra exposing (isJust, isNothing)
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

                completed =
                    session.offlineSession.children
                        |> EveryDict.filter (\childId _ -> hasCompletedChildActivity childType (getChildMeasurementData childId session))
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
                    session.offlineSession.mothers
                        |> EveryDictList.filter (\motherId _ -> hasCompletedMotherActivity motherType (getMotherMeasurementData motherId session))
                        |> EveryDictList.size
            in
                { pending = total - completed
                , total = total
                }


hasCompletedChildActivity : ChildActivityType -> MeasurementData ChildMeasurements ChildEdits -> Bool
hasCompletedChildActivity activityType measurements =
    case activityType of
        ChildPicture ->
            isCompleted measurements.edits.photo (Maybe.map Tuple.second measurements.current.photo)

        Height ->
            isCompleted measurements.edits.height (Maybe.map Tuple.second measurements.current.height)

        Weight ->
            isCompleted measurements.edits.weight (Maybe.map Tuple.second measurements.current.weight)

        Muac ->
            isCompleted measurements.edits.muac (Maybe.map Tuple.second measurements.current.muac)

        NutritionSigns ->
            isCompleted measurements.edits.nutrition (Maybe.map Tuple.second measurements.current.nutrition)

        ProgressReport ->
            -- Hmm. This isn't really a measurement, so if we get it, we'll say
            -- it's not "completed".
            --
            -- TODO: I suppose that if we're tracking "activities" for UI purposes,
            -- perhaps the activity here is just looking at the progress report?
            -- So, that would imply some local data that tracks whether we've looked
            -- at the progress report?
            False


hasCompletedMotherActivity : MotherActivityType -> MeasurementData MotherMeasurements MotherEdits -> Bool
hasCompletedMotherActivity activityType measurements =
    case activityType of
        FamilyPlanning ->
            isCompleted measurements.edits.familyPlanning (Maybe.map Tuple.second measurements.current.familyPlanning)


{-| Should some measurement be considered completed? Note that this means that it has
been entered locally, not that it has been saved to the backend.
-}
isCompleted : Edit value -> Maybe value -> Bool
isCompleted edit =
    applyEdit edit >> isJust


isPending : Edit value -> Maybe value -> Bool
isPending edit =
    applyEdit edit >> isNothing


hasAnyPendingMotherActivity : MeasurementData MotherMeasurements MotherEdits -> Bool
hasAnyPendingMotherActivity measurements =
    getAllMotherActivities
        |> List.any ((flip hasCompletedMotherActivity) measurements >> not)


hasAnyPendingChildActivity : MeasurementData ChildMeasurements ChildEdits -> Bool
hasAnyPendingChildActivity measurements =
    getAllChildActivities
        |> List.any ((flip hasCompletedChildActivity) measurements >> not)


{-| See whether either the mother, or any of her children, has a pending activity.

If we can't find the mother, we return False.

-}
motherOrAnyChildHasAnyPendingActivity : MotherId -> EditableSession -> Bool
motherOrAnyChildHasAnyPendingActivity motherId session =
    let
        motherHasOne =
            motherHasAnyPendingActivity motherId session

        anyChildHasOne =
            getMother motherId session.offlineSession
                |> Maybe.map
                    (\mother ->
                        mother.children
                            |> List.any (\childId -> childHasAnyPendingActivity childId session)
                    )
                |> Maybe.withDefault False
    in
        motherHasOne || anyChildHasOne


{-| Does the mother herself have any pending activity?
-}
motherHasAnyPendingActivity : MotherId -> EditableSession -> Bool
motherHasAnyPendingActivity motherId session =
    getMotherMeasurementData motherId session
        |> hasAnyPendingMotherActivity


{-| Does the child have any pending activity?
-}
childHasAnyPendingActivity : ChildId -> EditableSession -> Bool
childHasAnyPendingActivity childId session =
    getChildMeasurementData childId session
        |> hasAnyPendingChildActivity


participantHasPendingActivity : ParticipantId -> EditableSession -> Bool
participantHasPendingActivity participantId =
    case participantId of
        ParticipantChildId childId ->
            childHasAnyPendingActivity childId

        ParticipantMotherId motherId ->
            motherHasAnyPendingActivity motherId
