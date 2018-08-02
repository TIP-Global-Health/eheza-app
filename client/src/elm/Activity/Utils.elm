module Activity.Utils
    exposing
        ( childHasAnyPendingActivity
        , childHasCompletedActivity
        , childHasPendingActivity
        , decodeActivityTypeFromString
        , defaultActivityType
        , encodeActivityTypeAsString
        , expectedMotherActivities
        , getActivityIcon
        , getActivityList
        , getActivityTypeList
        , getAllChildActivities
        , getAllMotherActivities
        , hasAnyCompletedActivity
        , hasAnyPendingChildActivity
        , hasAnyPendingMotherActivity
        , hasCompletedChildActivity
        , hasCompletedMotherActivity
        , isCheckedIn
        , motherHasAnyPendingActivity
        , motherHasCompletedActivity
        , motherHasPendingActivity
        , motherOrAnyChildHasAnyCompletedActivity
        , motherOrAnyChildHasAnyPendingActivity
        , onlyCheckedIn
        , setCheckedIn
        )

import Activity.Model exposing (ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (applyEdit)
import Backend.Mother.Model exposing (ChildrenRelationType(..), Mother)
import Backend.Session.Model exposing (..)
import Backend.Session.Utils exposing (getChildMeasurementData, getMother, getMotherMeasurementData, getMyMother, mapMotherEdits)
import EveryDict
import EveryDictList
import Maybe.Extra exposing (isJust, isNothing)


{-| Used for URL etc., not for display in the normal UI
(since we'd translate for that).
-}
encodeActivityTypeAsString : ActivityType -> String
encodeActivityTypeAsString activityType =
    case activityType of
        ChildActivity activity ->
            case activity of
                ChildPicture ->
                    "picture"

                Height ->
                    "height"

                Muac ->
                    "muac"

                NutritionSigns ->
                    "nutrition"

                ProgressReport ->
                    "progress"

                Weight ->
                    "weight"

        MotherActivity activity ->
            case activity of
                FamilyPlanning ->
                    "family_planning"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityTypeFromString : String -> Maybe ActivityType
decodeActivityTypeFromString s =
    case s of
        "picture" ->
            Just <| ChildActivity ChildPicture

        "height" ->
            Just <| ChildActivity Height

        "muac" ->
            Just <| ChildActivity Muac

        "nutrition" ->
            Just <| ChildActivity NutritionSigns

        "progress" ->
            Just <| ChildActivity ProgressReport

        "weight" ->
            Just <| ChildActivity Weight

        "family_planning" ->
            Just <| MotherActivity FamilyPlanning

        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivityType : ActivityType
defaultActivityType =
    ChildActivity Height


{-| Note that `ProgressReport` isn't included for now, as it is
handled specially in the UI.
-}
getActivityTypeList : List ActivityType
getActivityTypeList =
    let
        childrenActivities =
            List.map ChildActivity getAllChildActivities

        mothersActivities =
            List.map MotherActivity getAllMotherActivities
    in
    childrenActivities ++ mothersActivities


{-| Get the pending and completed activities.
-}
getActivityList : EditableSession -> List ActivityListItem
getActivityList session =
    List.map
        (\activityType ->
            { activityType = activityType
            , totals = getTotalsNumberPerActivity activityType session
            }
        )
        getActivityTypeList


{-| Returns a string representing an icon for the activity, for use in a "class" attribute.
-}
getActivityIcon : ActivityType -> String
getActivityIcon activityType =
    case activityType of
        ChildActivity childActivityType ->
            case childActivityType of
                ChildPicture ->
                    "photo"

                Height ->
                    "height"

                Weight ->
                    "weight"

                Muac ->
                    "muac"

                NutritionSigns ->
                    "nutrition"

                ProgressReport ->
                    "bar chart"

        MotherActivity motherActivityType ->
            case motherActivityType of
                FamilyPlanning ->
                    "planning"


{-| Note that, for now, we're leaving out `ProgressReport` because that is handled
specially in the UI at the moment ... that may change in future.
-}
getAllChildActivities : List ChildActivityType
getAllChildActivities =
    [ Height, Muac, NutritionSigns, Weight, ChildPicture ]


getAllMotherActivities : List MotherActivityType
getAllMotherActivities =
    [ FamilyPlanning ]


getAllCaregiverActivities : List MotherActivityType
getAllCaregiverActivities =
    []


expectedMotherActivities : Mother -> List MotherActivityType
expectedMotherActivities mother =
    case mother.relation of
        MotherRelation ->
            getAllMotherActivities

        CaregiverRelation ->
            getAllCaregiverActivities


expectMotherActivity : MotherActivityType -> Mother -> Bool
expectMotherActivity activityType mother =
    List.member activityType (expectedMotherActivities mother)


{-| Given an activity, how many of those measurements should we expect, and how
many are still pending?

Both the pending and the total leave out anyone who is not in attendance (that is,
either marked in attendance, or has at least one activity completed).

-}
getTotalsNumberPerActivity : ActivityType -> EditableSession -> { pending : Int, total : Int }
getTotalsNumberPerActivity activityType session =
    case activityType of
        ChildActivity childType ->
            let
                childrenInAttendance =
                    session.offlineSession.children
                        |> EveryDict.filter (\childId _ -> childIsCheckedIn childId session)

                total =
                    EveryDict.size childrenInAttendance

                completed =
                    childrenInAttendance
                        |> EveryDict.filter (\childId _ -> hasCompletedChildActivity childType (getChildMeasurementData childId session))
                        |> EveryDict.size
            in
            { pending = total - completed
            , total = total
            }

        MotherActivity motherType ->
            let
                expectedMothers =
                    session.offlineSession.mothers
                        |> EveryDictList.filter
                            (\motherId mother ->
                                isCheckedIn motherId session && expectMotherActivity motherType mother
                            )

                total =
                    EveryDictList.size expectedMothers

                -- It's actually eaiser to count the completed ones, so we do that and
                -- just subtract to get pending.
                completed =
                    expectedMothers
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


childHasCompletedActivity : ChildId -> ChildActivityType -> EditableSession -> Bool
childHasCompletedActivity childId activityType session =
    getChildMeasurementData childId session
        |> hasCompletedChildActivity activityType


childHasPendingActivity : ChildId -> ChildActivityType -> EditableSession -> Bool
childHasPendingActivity childId activityType session =
    childHasCompletedActivity childId activityType session
        |> not


hasCompletedMotherActivity : MotherActivityType -> MeasurementData MotherMeasurements MotherEdits -> Bool
hasCompletedMotherActivity activityType measurements =
    case activityType of
        FamilyPlanning ->
            isCompleted measurements.edits.familyPlanning (Maybe.map Tuple.second measurements.current.familyPlanning)


motherHasCompletedActivity : MotherId -> MotherActivityType -> EditableSession -> Bool
motherHasCompletedActivity motherId activityType session =
    getMotherMeasurementData motherId session
        |> hasCompletedMotherActivity activityType


motherHasPendingActivity : MotherId -> MotherActivityType -> EditableSession -> Bool
motherHasPendingActivity motherId activityType session =
    motherHasCompletedActivity motherId activityType session
        |> not


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
        |> List.any (flip hasCompletedMotherActivity measurements >> not)


hasAnyCompletedMotherActivity : MeasurementData MotherMeasurements MotherEdits -> Bool
hasAnyCompletedMotherActivity measurements =
    getAllMotherActivities
        |> List.any (flip hasCompletedMotherActivity measurements)


hasAnyPendingChildActivity : MeasurementData ChildMeasurements ChildEdits -> Bool
hasAnyPendingChildActivity measurements =
    getAllChildActivities
        |> List.any (flip hasCompletedChildActivity measurements >> not)


hasAnyCompletedChildActivity : MeasurementData ChildMeasurements ChildEdits -> Bool
hasAnyCompletedChildActivity measurements =
    getAllChildActivities
        |> List.any (flip hasCompletedChildActivity measurements)


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


{-| See whether either the mother, or any of her children, has any completed activity.

If we can't find the mother, we return False.

-}
motherOrAnyChildHasAnyCompletedActivity : MotherId -> EditableSession -> Bool
motherOrAnyChildHasAnyCompletedActivity motherId session =
    let
        motherHasOne =
            motherHasAnyCompletedActivity motherId session

        anyChildHasOne =
            getMother motherId session.offlineSession
                |> Maybe.map
                    (\mother ->
                        mother.children
                            |> List.any (\childId -> childHasAnyCompletedActivity childId session)
                    )
                |> Maybe.withDefault False
    in
    motherHasOne || anyChildHasOne


{-| Has the mother been marked as checked in?

We'll return true if the mother has been explicitly checked-in in the UI, or
has a completed activity ... that way, we can freely change the explicit
check-in (and activities) without worrying about synchronizing the two.

-}
isCheckedIn : MotherId -> EditableSession -> Bool
isCheckedIn motherId session =
    let
        explicitlyCheckedIn =
            getMotherMeasurementData motherId session
                |> (.edits >> .explicitlyCheckedIn)

        hasCompletedActivity =
            motherOrAnyChildHasAnyCompletedActivity motherId session
    in
    explicitlyCheckedIn || hasCompletedActivity


setCheckedIn : Bool -> MotherId -> EditableSession -> EditableSession
setCheckedIn checkedIn =
    mapMotherEdits (\edits -> { edits | explicitlyCheckedIn = checkedIn })


childIsCheckedIn : ChildId -> EditableSession -> Bool
childIsCheckedIn childId session =
    getMyMother childId session.offlineSession
        |> Maybe.map Tuple.first
        |> Maybe.map (\motherId -> isCheckedIn motherId session)
        |> Maybe.withDefault False


{-| Filters the mothers and children in an editable session to only
include those who are marked in attendance.
-}
onlyCheckedIn : EditableSession -> EditableSession
onlyCheckedIn session =
    let
        mothers =
            session.offlineSession.mothers
                |> EveryDictList.filter (\motherId _ -> isCheckedIn motherId session)

        children =
            -- TODO: This could be done more efficiently, given that we've got the mothers
            session.offlineSession.children
                |> EveryDict.filter (\childId _ -> childIsCheckedIn childId session)

        offlineSession =
            (\offline ->
                { offline
                    | mothers = mothers
                    , children = children
                }
            )
                session.offlineSession
    in
    -- For now, at least, we don't bother filtering the measurements ... we just
    -- filter the mothers and children.
    { session | offlineSession = offlineSession }


{-| Does the mother herself have any pending activity?
-}
motherHasAnyPendingActivity : MotherId -> EditableSession -> Bool
motherHasAnyPendingActivity motherId session =
    getMotherMeasurementData motherId session
        |> hasAnyPendingMotherActivity


{-| Does the mother herself have any completed activity?
-}
motherHasAnyCompletedActivity : MotherId -> EditableSession -> Bool
motherHasAnyCompletedActivity motherId session =
    getMotherMeasurementData motherId session
        |> hasAnyCompletedMotherActivity


{-| Does the child have any pending activity?
-}
childHasAnyPendingActivity : ChildId -> EditableSession -> Bool
childHasAnyPendingActivity childId session =
    getChildMeasurementData childId session
        |> hasAnyPendingChildActivity


{-| Does the child have any completed activity?
-}
childHasAnyCompletedActivity : ChildId -> EditableSession -> Bool
childHasAnyCompletedActivity childId session =
    getChildMeasurementData childId session
        |> hasAnyCompletedChildActivity


{-| Is there any completed activity of any kind?
-}
hasAnyCompletedActivity : EditableSession -> Bool
hasAnyCompletedActivity session =
    let
        forChildren =
            session.offlineSession.children
                |> EveryDict.toList
                |> List.any (\( id, _ ) -> childHasAnyCompletedActivity id session)

        forMothers =
            session.offlineSession.mothers
                |> EveryDictList.toList
                |> List.any (\( id, _ ) -> motherHasAnyCompletedActivity id session)
    in
    forChildren || forMothers
