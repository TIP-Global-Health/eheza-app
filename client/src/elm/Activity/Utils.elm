module Activity.Utils
    exposing
        ( decodeActivityFromString
        , defaultActivity
        , encodeActivityAsString
        , getActivityCountForMother
        , getActivityIcon
        , getAllActivities
        , getCheckedIn
        , getParticipantCountForActivity
        , motherIsCheckedIn
        , setCheckedIn
        , summarizeByActivity
        , summarizeByParticipant
        , summarizeChildActivity
        , summarizeChildParticipant
        , summarizeMotherActivity
        , summarizeMotherParticipant
        )

{-| Various utilities that deal with "activities". An activity represents the need
for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Activity.Model exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (applyEdit)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (..)
import Backend.Session.Utils exposing (getChildMeasurementData, getMother, getMotherMeasurementData, getMyMother, mapMotherEdits)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Maybe.Extra exposing (isJust, isNothing)


{-| Used for URL etc., not for display in the normal UI
(since we'd translate for that).
-}
encodeActivityAsString : Activity -> String
encodeActivityAsString activity =
    case activity of
        ChildActivity childActivity ->
            case childActivity of
                ChildPicture ->
                    "picture"

                Height ->
                    "height"

                Muac ->
                    "muac"

                NutritionSigns ->
                    "nutrition"

                Weight ->
                    "weight"

        MotherActivity motherActivity ->
            case motherActivity of
                FamilyPlanning ->
                    "family_planning"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe Activity
decodeActivityFromString s =
    case s of
        "picture" ->
            Just <| ChildActivity ChildPicture

        "height" ->
            Just <| ChildActivity Height

        "muac" ->
            Just <| ChildActivity Muac

        "nutrition" ->
            Just <| ChildActivity NutritionSigns

        "weight" ->
            Just <| ChildActivity Weight

        "family_planning" ->
            Just <| MotherActivity FamilyPlanning

        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivity : Activity
defaultActivity =
    ChildActivity Height


{-| Returns a string representing an icon for the activity, for use in a "class" attribute.
-}
getActivityIcon : Activity -> String
getActivityIcon activity =
    case activity of
        ChildActivity childActivity ->
            case childActivity of
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

        MotherActivity motherActivity ->
            case motherActivity of
                FamilyPlanning ->
                    "planning"


getAllActivities : List Activity
getAllActivities =
    List.concat
        [ List.map ChildActivity getAllChildActivities
        , List.map MotherActivity getAllMotherActivities
        ]


getAllChildActivities : List ChildActivity
getAllChildActivities =
    [ Height, Muac, NutritionSigns, Weight, ChildPicture ]


getAllMotherActivities : List MotherActivity
getAllMotherActivities =
    [ FamilyPlanning ]


{-| Do we expect this activity to be performed in this session for this child?
Note that we don't consider whether the child is checked in here -- just whether
we would expect to perform this action if checked in.
-}
expectChildActivity : EditableSession -> ChildId -> ChildActivity -> Bool
expectChildActivity session childId activity =
    -- For now, we always expect all child activities ... this is here for
    -- when we add a `Counseling` activity, which we won't expect for all
    -- children in all sessions
    True


{-| Do we expect this activity to be performed in this session for this mother?
Note that we don't consider whether the mother is checked in here -- just whether
we would expect to perform this action if checked in.
-}
expectMotherActivity : EditableSession -> MotherId -> MotherActivity -> Bool
expectMotherActivity session motherId activity =
    -- Always True until we have the caregiver code
    True


{-| For a particular child activity, figure out which children have completed
the activity and have the activity pending. (This may not add up to all the
children, because we only consider a child "pending" if they are checked in and
the activity is expected.
-}
summarizeChildActivity : ChildActivity -> EditableSession -> CompletedAndPending (EveryDictList ChildId Child)
summarizeChildActivity activity session =
    getCheckedIn session
        |> .children
        |> EveryDictList.filter (\childId _ -> expectChildActivity session childId activity)
        |> EveryDictList.partition (\childId _ -> childHasCompletedActivity childId activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| For a particular mother activity, figure out which mothers have completed
the activity and have the activity pending. (This may not add up to all the
mothers, because we only consider a mother "pending" if they are checked in and
the activity is expected.
-}
summarizeMotherActivity : MotherActivity -> EditableSession -> CompletedAndPending (EveryDictList MotherId Mother)
summarizeMotherActivity activity session =
    getCheckedIn session
        |> .mothers
        |> EveryDictList.filter (\motherId _ -> expectMotherActivity session motherId activity)
        |> EveryDictList.partition (\motherId _ -> motherHasCompletedActivity motherId activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| Summarize our data for the editable session in a way that is useful
for our UI, when we're focused on activities. This only considers children &
mothers who are checked in to the session.
-}
summarizeByActivity : EditableSession -> SummaryByActivity
summarizeByActivity session =
    let
        children =
            getAllChildActivities
                |> List.map
                    (\activity ->
                        ( activity
                        , summarizeChildActivity activity session
                        )
                    )
                |> EveryDict.fromList

        mothers =
            getAllMotherActivities
                |> List.map
                    (\activity ->
                        ( activity
                        , summarizeMotherActivity activity session
                        )
                    )
                |> EveryDict.fromList
    in
    { children = children
    , mothers = mothers
    }


getParticipantCountForActivity : SummaryByActivity -> Activity -> CompletedAndPending Int
getParticipantCountForActivity summary activity =
    case activity of
        ChildActivity childActivity ->
            summary.children
                |> EveryDict.get childActivity
                |> Maybe.map
                    (\{ completed, pending } ->
                        { completed = EveryDictList.size completed
                        , pending = EveryDictList.size pending
                        }
                    )
                |> Maybe.withDefault
                    { completed = 0
                    , pending = 0
                    }

        MotherActivity motherActivity ->
            summary.mothers
                |> EveryDict.get motherActivity
                |> Maybe.map
                    (\{ completed, pending } ->
                        { completed = EveryDictList.size completed
                        , pending = EveryDictList.size pending
                        }
                    )
                |> Maybe.withDefault
                    { completed = 0
                    , pending = 0
                    }


{-| For a particular child, figure out which activities are completed
and which are pending. (This may not add up to all the activities, because some
activities may not be expected for this child).
-}
summarizeChildParticipant : ChildId -> EditableSession -> CompletedAndPending (List ChildActivity)
summarizeChildParticipant id session =
    getAllChildActivities
        |> List.filter (expectChildActivity session id)
        |> List.partition (\activity -> childHasCompletedActivity id activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| For a particular mother, figure out which activities are completed
and which are pending. (This may not add up to all the activities, because some
activities may not be expected for this mother).
-}
summarizeMotherParticipant : MotherId -> EditableSession -> CompletedAndPending (List MotherActivity)
summarizeMotherParticipant id session =
    getAllMotherActivities
        |> List.filter (expectMotherActivity session id)
        |> List.partition (\activity -> motherHasCompletedActivity id activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| Summarize our data for the editable session in a way that is useful
for our UI, when we're focused on participants. This only considers children &
mothers who are checked in to the session.
-}
summarizeByParticipant : EditableSession -> SummaryByParticipant
summarizeByParticipant session =
    let
        checkedIn =
            getCheckedIn session

        children =
            EveryDictList.map
                (\childId _ -> summarizeChildParticipant childId session)
                checkedIn.children

        mothers =
            EveryDictList.map
                (\motherId _ -> summarizeMotherParticipant motherId session)
                checkedIn.mothers
    in
    { children = children
    , mothers = mothers
    }


{-| This includes activities for children of the mother.
-}
getActivityCountForMother : MotherId -> Mother -> SummaryByParticipant -> CompletedAndPending Int
getActivityCountForMother id mother summary =
    let
        motherCount =
            EveryDictList.get id summary.mothers
                |> Maybe.map
                    (\activities ->
                        { pending = List.length activities.pending
                        , completed = List.length activities.completed
                        }
                    )
                |> Maybe.withDefault
                    { pending = 0
                    , completed = 0
                    }
    in
    List.foldl
        (\childId accum ->
            EveryDictList.get childId summary.children
                |> Maybe.map
                    (\activities ->
                        { pending = accum.pending + List.length activities.pending
                        , completed = accum.completed + List.length activities.completed
                        }
                    )
                |> Maybe.withDefault accum
        )
        motherCount
        mother.children


hasCompletedChildActivity : ChildActivity -> MeasurementData ChildMeasurements ChildEdits -> Bool
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


childHasCompletedActivity : ChildId -> ChildActivity -> EditableSession -> Bool
childHasCompletedActivity childId activityType session =
    getChildMeasurementData childId session
        |> hasCompletedChildActivity activityType


hasCompletedMotherActivity : MotherActivity -> MeasurementData MotherMeasurements MotherEdits -> Bool
hasCompletedMotherActivity activityType measurements =
    case activityType of
        FamilyPlanning ->
            isCompleted measurements.edits.familyPlanning (Maybe.map Tuple.second measurements.current.familyPlanning)


motherHasCompletedActivity : MotherId -> MotherActivity -> EditableSession -> Bool
motherHasCompletedActivity motherId activityType session =
    getMotherMeasurementData motherId session
        |> hasCompletedMotherActivity activityType


{-| Should some measurement be considered completed? Note that this means that it has
been entered locally, not that it has been saved to the backend.
-}
isCompleted : Edit value -> Maybe value -> Bool
isCompleted edit =
    applyEdit edit >> isJust


hasAnyCompletedMotherActivity : MeasurementData MotherMeasurements MotherEdits -> Bool
hasAnyCompletedMotherActivity measurements =
    getAllMotherActivities
        |> List.any (flip hasCompletedMotherActivity measurements)


hasAnyCompletedChildActivity : MeasurementData ChildMeasurements ChildEdits -> Bool
hasAnyCompletedChildActivity measurements =
    getAllChildActivities
        |> List.any (flip hasCompletedChildActivity measurements)


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
motherIsCheckedIn : MotherId -> EditableSession -> Bool
motherIsCheckedIn motherId session =
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
        |> Maybe.map (\motherId -> motherIsCheckedIn motherId session)
        |> Maybe.withDefault False


{-| Who is checked in, considering both explicit check in and anyone who has
any completed activity?
-}
getCheckedIn : EditableSession -> { mothers : EveryDictList MotherId Mother, children : EveryDictList ChildId Child }
getCheckedIn session =
    let
        -- Which mothers have been explicitly checked in?
        explicitlyCheckedInMothers =
            EveryDict.filter (\_ edits -> edits.explicitlyCheckedIn)
                session.edits.mothers

        -- A mother is checked in if explicitly checked in or has any completed activites.
        mothers =
            EveryDictList.filter
                (\motherId _ ->
                    EveryDict.member motherId explicitlyCheckedInMothers
                        || motherOrAnyChildHasAnyCompletedActivity motherId session
                )
                session.offlineSession.mothers

        -- A child is checked in if the mother is checked in.
        children =
            EveryDictList.filter
                (\_ child ->
                    child.motherId
                        |> Maybe.map (\motherId -> EveryDictList.member motherId mothers)
                        |> Maybe.withDefault False
                )
                session.offlineSession.children
    in
    { mothers = mothers
    , children = children
    }


{-| Does the mother herself have any completed activity?
-}
motherHasAnyCompletedActivity : MotherId -> EditableSession -> Bool
motherHasAnyCompletedActivity motherId session =
    getMotherMeasurementData motherId session
        |> hasAnyCompletedMotherActivity


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
                |> EveryDictList.toList
                |> List.any (\( id, _ ) -> childHasAnyCompletedActivity id session)

        forMothers =
            session.offlineSession.mothers
                |> EveryDictList.toList
                |> List.any (\( id, _ ) -> motherHasAnyCompletedActivity id session)
    in
    forChildren || forMothers
