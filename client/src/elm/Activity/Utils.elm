module Activity.Utils
    exposing
        ( decodeActivityFromString
        , defaultActivity
        , encodeActivityAsString
        , expectCounselingActivity
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

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Activity.Model exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Counseling.Model exposing (CounselingTiming(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (applyEdit, currentValue, mapMeasurementData)
import Backend.Mother.Model exposing (ChildrenRelationType(..), Mother)
import Backend.Session.Model exposing (..)
import Backend.Session.Utils exposing (getChild, getChildHistoricalMeasurements, getChildMeasurementData, getMother, getMotherMeasurementData, getMyMother, mapMotherEdits)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (diffDays)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
encodeActivityAsString : Activity -> String
encodeActivityAsString activity =
    case activity of
        ChildActivity childActivity ->
            case childActivity of
                ChildPicture ->
                    "picture"

                Counseling ->
                    "counseling"

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

        "counseling" ->
            Just <| ChildActivity Counseling

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


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : Activity -> String
getActivityIcon activity =
    case activity of
        ChildActivity childActivity ->
            case childActivity of
                ChildPicture ->
                    "photo"

                Counseling ->
                    "counseling"

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
    [ Counseling, Height, Muac, NutritionSigns, Weight, ChildPicture ]


getAllMotherActivities : List MotherActivity
getAllMotherActivities =
    [ FamilyPlanning ]


{-| Do we expect this activity to be performed in this session for this child?
Note that we don't consider whether the child is checked in here -- just
whether we would expect to perform this action if checked in.
-}
expectChildActivity : EditableSession -> ChildId -> ChildActivity -> Bool
expectChildActivity session childId activity =
    case activity of
        Counseling ->
            Maybe.Extra.isJust <|
                expectCounselingActivity session childId

        _ ->
            -- In all other cases, we expect each ativity each time.
            True


{-| Whether to expect a counseling activity is not just a yes/no question,
since we'd also like to know **which** sort of counseling activity to expect.
I suppose we could parameterize the `Counseling` activity by
`CounselingTiming`. However, that would be awkward in its own way, since we
also don't want more than one in each session.

So, we'll try it this way for now. We'll return `Nothing` if no kind of
counseling activity is expected, and `Just CounselingTiming` if one is
expected.

-}
expectCounselingActivity : EditableSession -> ChildId -> Maybe CounselingTiming
expectCounselingActivity session childId =
    let
        -- First, we check our current value. If we have a counseling session
        -- stored in the backend, or we've already got a local edit, then we
        -- use that.  This has two benefits. First, its a kind of optimization,
        -- since we're basically caching our conclusion about whether to
        -- showing the counseling activity or not. Second, it provides some UI
        -- stability ...  once we show the counseling activity and the user
        -- checks some boxes, it ensures that we'll definitely keep showing
        -- that one, and not switch to something else.
        cachedTiming =
            getChildMeasurementData childId session
                |> mapMeasurementData .counselingSession .counseling
                |> currentValue
                |> Maybe.map (.value >> Tuple.first)

        -- All the counseling session records from the past
        historical =
            getChildHistoricalMeasurements childId session.offlineSession
                |> .counselingSessions

        -- Have we ever completed a counseling session of the specified type?
        completed timing =
            List.any
                (\( _, counseling ) -> Tuple.first counseling.value == timing)
                historical

        -- How long ago did we complete a session of the specified type?
        completedDaysAgo timing =
            historical
                |> List.Extra.find (\( _, counseling ) -> Tuple.first counseling.value == timing)
                |> Maybe.map (\( _, counseling ) -> diffDays counseling.dateMeasured session.offlineSession.session.scheduledDate.start)

        -- How old will the child be as of the scheduled date of the session?
        -- (All of our date calculations are in days here).
        --
        -- It simplifies the rest of the calculation if we avoid making this a
        -- `Maybe`. We've got bigger problems if the session doesn't actually
        -- contain the child, so it should be safe to default the age to 0.
        age =
            getChild childId session.offlineSession
                |> Maybe.map (\child -> diffDays child.birthDate session.offlineSession.session.scheduledDate.start)
                |> Maybe.withDefault 0

        -- We don't necessarily know when the next session will be scheduled,
        -- so we work on the assumption that it will be no more than 6 weeks
        -- from this session (so, 42 days).
        maximumSessionGap =
            42

        -- For the reminder, which isn't as critical, we apply the normal
        -- session gap of 32 days. This reduces the frequence of cases where we
        -- issue the reminder super-early, at the cost of some cases where we
        -- might issue no reminder (which is less serious).
        normalSessionGap =
            32

        -- To compute a two-month gap, we use one normal and one maximum
        twoMonthGap =
            normalSessionGap + maximumSessionGap

        -- To compute a three month gap, we use two normals and one maximum
        threeMonthGap =
            (normalSessionGap * 2) + maximumSessionGap

        -- In how many days (from the session date) will the child be 2 years
        -- old?
        daysUntilTwoYearsOld =
            (365 * 2) - age

        -- In how many days (from the session date) will the child be 1 year
        -- old?
        daysUntilOneYearOld =
            365 - age

        -- If we don't have a value already, we apply our basic logic, but
        -- lazily, so we make this a function. Here's a summary of our design
        -- goals, which end up having a number of parts.
        --
        -- - Definitely show the counseling activity before the relevant
        --   anniversary, using the assumption that the next session will be no
        --   more than 6 weeks away.
        --
        -- - Try to avoid showing counseling activities with no reminders, but
        --   do it without a reminder if necessary.
        --
        -- - Once we show a reminder, always show the counseling activity in
        --   the next session, even if it now seems a bit early (to avoid double
        --   reminders).
        --
        -- - Always show the entry counseling if it hasn't been done, unless
        --   we've already reached exit counseling.
        --
        -- - Make sure that there is a bit of a delay between entry counseling
        --   and midpoint counseling (for cases where a baby starts late).
        checkTiming _ =
            if completed Exit then
                -- If exit counseling has been done, then we need no more
                -- counseling
                Nothing
            else if completed BeforeExit then
                -- If we've given the exit reminder, then show the exit
                -- counseling now, even if it seems a bit early.
                Just Exit
            else if daysUntilTwoYearsOld < maximumSessionGap then
                -- If we can't be sure we'll have another session before the
                -- baby is two, then show the exit counseling
                Just Exit
            else if not (completed Entry) then
                -- If we haven't done entry counseling, then we always need to
                -- do it
                Just Entry
            else if completed MidPoint then
                -- If we have already done the MidPoint counseling, then the
                -- only thing left to consider is whether to show the Exit
                -- reminder
                if daysUntilTwoYearsOld < twoMonthGap then
                    Just BeforeExit
                else
                    Nothing
            else if completed BeforeMidpoint then
                -- If we've given the midpoint warning, then show it, even if
                -- it seems a bit early now.
                Just MidPoint
            else if daysUntilOneYearOld < maximumSessionGap then
                -- If we can't be sure we'll have another session before the
                -- baby is one year old, we show the exit counseling. Except,
                -- we also check to see whether we've done entry counseling
                -- recently ...  so that we'll always have a bit of a gap.
                case completedDaysAgo Entry of
                    Just daysAgo ->
                        if daysAgo < threeMonthGap then
                            -- We're forcing the midpoint counseling to be
                            -- roungly 3 months after the entry counseling. So,
                            -- the ideal sequence would be:
                            --
                            -- entry -> Nothing -> Rminder MidPoint -> MidPoint
                            if daysAgo < twoMonthGap then
                                Nothing
                            else
                                Just BeforeMidpoint
                        else
                            Just MidPoint

                    Nothing ->
                        Just MidPoint
            else if daysUntilOneYearOld < twoMonthGap then
                -- If we think we'll do the midpoint counseling at the next
                -- session, show the reminder. Except, again, we try to force a
                -- bit of separation between Entry and the Midpoint.
                case completedDaysAgo Entry of
                    Just daysAgo ->
                        if daysAgo < twoMonthGap then
                            -- We're forcing the reminder for midpoint
                            -- counseling to be roughtly 2 months after the
                            -- entry counseling.
                            Nothing
                        else
                            Just BeforeMidpoint

                    Nothing ->
                        Just BeforeMidpoint
            else
                Nothing
    in
    cachedTiming
        |> Maybe.Extra.orElseLazy checkTiming


{-| Do we expect this activity to be performed in this session for this mother?
Note that we don't consider whether the mother is checked in here -- just
whether we would expect to perform this action if checked in.
-}
expectMotherActivity : EditableSession -> MotherId -> MotherActivity -> Bool
expectMotherActivity session motherId activity =
    -- The activity is unused for now, but will probably be used later, so I've
    -- structured it this way from the beginning.  For now, all activities are
    -- expected for "mothers" and none for "caregivers"
    getMother motherId session.offlineSession
        |> Maybe.map
            (\mother ->
                case mother.relation of
                    MotherRelation ->
                        True

                    CaregiverRelation ->
                        False
            )
        |> Maybe.withDefault False


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


{-| This summarizes our summary, by counting, for the given activity, how many
participants are completed or pending.
-}
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


{-| This summarizes our summary, by counting how many activities have been
completed for the given mother.

It includes ativities for children of the mother, since we navigate from mother
to child.

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

        Counseling ->
            isCompleted measurements.edits.counseling (Maybe.map Tuple.second measurements.current.counselingSession)

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

        -- A mother is checked in if explicitly checked in or has any completed
        -- activites.
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
