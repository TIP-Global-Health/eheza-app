module Activity.Utils exposing (..)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Activity.Model exposing (..)
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (CounselingTiming(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (currentValue, currentValues, getMeasurementValueFunc, mapMeasurementData, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person, Ubudehe(..))
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.Session.Model exposing (..)
import Backend.Session.Utils exposing (getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildMeasurementData2, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMotherMeasurementData2, getMyMother)
import EverySet
import Gizra.NominalDate exposing (NominalDate, diffDays, diffMonths)
import LocalData
import Maybe.Extra exposing (isJust, isNothing)
import RemoteData exposing (RemoteData(..))
import ZScore.Model
import ZScore.Utils exposing (zScoreWeightForAge)


generateNutritionAssessment : NominalDate -> ZScore.Model.Model -> PersonId -> ModelIndexedDb -> OfflineSession -> List NutritionAssessment
generateNutritionAssessment currentDate zscores childId db offlineSession =
    let
        measurements =
            LocalData.unwrap
                Nothing
                (\measurements_ -> Dict.get childId measurements_.current.children)
                offlineSession.measurements

        muacValue =
            Maybe.andThen (.muac >> getMeasurementValueFunc) measurements

        nutritionValue =
            Maybe.andThen (.nutrition >> getMeasurementValueFunc) measurements
                |> Maybe.map .signs

        weightValue =
            Maybe.andThen (.weight >> Maybe.map (Tuple.second >> .value >> weightValueFunc)) measurements
    in
    Backend.NutritionEncounter.Utils.generateNutritionAssessment currentDate zscores childId muacValue nutritionValue weightValue True db


{-| Used for URL etc., not for display in the normal UI (since we'd translatefor that).
-}
encodeActivityAsString : Activity -> String
encodeActivityAsString activity =
    case activity of
        ChildActivity childActivity ->
            case childActivity of
                ChildFbf ->
                    "child_fbf"

                ChildPicture ->
                    "picture"

                ContributingFactors ->
                    "contributing_factors"

                -- Counseling ->
                --   "counseling"
                FollowUp ->
                    "follow_up"

                Activity.Model.HealthEducation ->
                    "group_health_education"

                Height ->
                    "height"

                Muac ->
                    "muac"

                NutritionSigns ->
                    "nutrition"

                Activity.Model.SendToHC ->
                    "group_send_to_hc"

                Weight ->
                    "weight"

        MotherActivity motherActivity ->
            case motherActivity of
                FamilyPlanning ->
                    "family_planning"

                Lactation ->
                    "lactation"

                MotherFbf ->
                    "mother_fbf"

                ParticipantConsent ->
                    "participants_consent"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe Activity
decodeActivityFromString s =
    case s of
        "child_fbf" ->
            Just <| ChildActivity ChildFbf

        "picture" ->
            Just <| ChildActivity ChildPicture

        "contributing_factors" ->
            Just <| ChildActivity ContributingFactors

        "follow_up" ->
            Just <| ChildActivity FollowUp

        "group_health_education" ->
            Just <| ChildActivity Activity.Model.HealthEducation

        -- "counseling" ->
        --  Just <| ChildActivity Counseling
        "height" ->
            Just <| ChildActivity Height

        "muac" ->
            Just <| ChildActivity Muac

        "nutrition" ->
            Just <| ChildActivity NutritionSigns

        "group_send_to_hc" ->
            Just <| ChildActivity Activity.Model.SendToHC

        "weight" ->
            Just <| ChildActivity Weight

        "family_planning" ->
            Just <| MotherActivity FamilyPlanning

        "lactation" ->
            Just <| MotherActivity Lactation

        "mother_fbf" ->
            Just <| MotherActivity MotherFbf

        "participants_consent" ->
            Just <| MotherActivity ParticipantConsent

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : Activity -> String
getActivityIcon activity =
    case activity of
        ChildActivity childActivity ->
            case childActivity of
                ChildFbf ->
                    "fbf"

                ChildPicture ->
                    "photo"

                ContributingFactors ->
                    "contributing-factors"

                FollowUp ->
                    "follow-up"

                Activity.Model.HealthEducation ->
                    "health-education"

                -- Counseling ->
                --    "counseling"
                Height ->
                    "height"

                Muac ->
                    "muac"

                Activity.Model.SendToHC ->
                    "send-to-hc"

                Weight ->
                    "weight"

                NutritionSigns ->
                    "nutrition"

        MotherActivity motherActivity ->
            case motherActivity of
                FamilyPlanning ->
                    "planning"

                Lactation ->
                    "lactation"

                MotherFbf ->
                    "fbf"

                ParticipantConsent ->
                    "forms"


getAllActivities : OfflineSession -> List Activity
getAllActivities offlineSession =
    List.concat
        [ List.map ChildActivity (getAllChildActivities offlineSession)
        , List.map MotherActivity (getAllMotherActivities offlineSession)
        ]


getAllChildActivitiesExcludingNextSteps : OfflineSession -> List Activity
getAllChildActivitiesExcludingNextSteps offlineSession =
    List.concat
        [ List.map ChildActivity (getAllChildActivitiesWithExclusion offlineSession nextStepsActivities)
        , List.map MotherActivity (getAllMotherActivities offlineSession)
        ]


getAllChildActivities : OfflineSession -> List ChildActivity
getAllChildActivities offlineSession =
    getAllChildActivitiesWithExclusion offlineSession []


getAllChildActivitiesWithExclusion : OfflineSession -> List ChildActivity -> List ChildActivity
getAllChildActivitiesWithExclusion offlineSession exclusionList =
    let
        forAllGroupTypesMandatory =
            [ {- Counseling, -} Height
            , Muac
            , NutritionSigns
            , Weight
            ]

        forAllGroupTypesOptional =
            [ ChildPicture ]

        forFbf =
            case offlineSession.session.clinicType of
                Fbf ->
                    [ ChildFbf ]

                Achi ->
                    [ ChildFbf ]

                _ ->
                    []
    in
    forAllGroupTypesMandatory
        ++ nextStepsActivities
        ++ forFbf
        ++ forAllGroupTypesOptional
        |> List.filter (\activity -> not <| List.member activity exclusionList)


getAllMotherActivities : OfflineSession -> List MotherActivity
getAllMotherActivities offlineSession =
    case offlineSession.session.clinicType of
        Achi ->
            -- No mother activities at Achi groups.
            []

        _ ->
            let
                forAllGroupTypes =
                    [ FamilyPlanning

                    --, ParticipantConsent
                    ]
            in
            if offlineSession.session.clinicType == Fbf then
                forAllGroupTypes ++ [ Lactation, MotherFbf ]

            else
                forAllGroupTypes


nextStepsActivities : List ChildActivity
nextStepsActivities =
    [ ContributingFactors
    , Activity.Model.HealthEducation
    , Activity.Model.SendToHC
    , FollowUp
    ]


{-| Do we expect this activity to be performed in this session for this child?
Note that we don't consider whether the child is checked in here -- just
whether we would expect to perform this action if checked in.
-}
expectChildActivity : NominalDate -> ZScore.Model.Model -> OfflineSession -> PersonId -> Bool -> ModelIndexedDb -> ChildActivity -> Bool
expectChildActivity currentDate zscores offlineSession childId isChw db activity =
    case activity of
        Muac ->
            Dict.get childId offlineSession.children
                |> Maybe.andThen .birthDate
                |> Maybe.map
                    (\birthDate ->
                        if diffMonths birthDate currentDate < 6 then
                            False

                        else
                            True
                    )
                |> Maybe.withDefault False

        {- Counseling ->
           Maybe.Extra.isJust <|
               expectCounselingActivity session childId
        -}
        ChildFbf ->
            List.member offlineSession.session.clinicType [ Achi, Fbf ]

        ContributingFactors ->
            mandatoryActivitiesCompleted currentDate zscores offlineSession childId isChw db
                && (generateNutritionAssessment currentDate zscores childId db offlineSession
                        |> List.isEmpty
                        |> not
                   )

        FollowUp ->
            expectChildActivity currentDate zscores offlineSession childId isChw db ContributingFactors

        Activity.Model.HealthEducation ->
            expectChildActivity currentDate zscores offlineSession childId isChw db ContributingFactors

        Activity.Model.SendToHC ->
            expectChildActivity currentDate zscores offlineSession childId isChw db ContributingFactors

        _ ->
            -- In all other cases, we always view the ativity.
            True


mandatoryActivitiesCompleted : NominalDate -> ZScore.Model.Model -> OfflineSession -> PersonId -> Bool -> ModelIndexedDb -> Bool
mandatoryActivitiesCompleted currentDate zscores offlineSession childId isChw db =
    childActivitiesCompleted currentDate zscores offlineSession childId isChw db allMandatoryActivities


childActivitiesCompleted : NominalDate -> ZScore.Model.Model -> OfflineSession -> PersonId -> Bool -> ModelIndexedDb -> List ChildActivity -> Bool
childActivitiesCompleted currentDate zscores offlineSession childId isChw db activities =
    List.all (childActivityCompleted currentDate zscores offlineSession childId isChw db) activities


childActivityCompleted : NominalDate -> ZScore.Model.Model -> OfflineSession -> PersonId -> Bool -> ModelIndexedDb -> ChildActivity -> Bool
childActivityCompleted currentDate zscores offlineSession childId isChw db activity =
    (not <| expectChildActivity currentDate zscores offlineSession childId isChw db activity)
        || childHasCompletedActivity childId activity offlineSession


allMandatoryActivities : List ChildActivity
allMandatoryActivities =
    [ Muac, NutritionSigns, Weight ]


{-| Whether to expect a counseling activity is not just a yes/no question,
since we'd also like to know **which** sort of counseling activity to expect.
I suppose we could parameterize the `Counseling` activity by
`CounselingTiming`. However, that would be awkward in its own way, since we
also don't want more than one in each session.

So, we'll try it this way for now. We'll return `Nothing` if no kind of
counseling activity is expected, and `Just CounselingTiming` if one is
expected.

-}
expectCounselingActivity : EditableSession -> PersonId -> Maybe CounselingTiming
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
                |> LocalData.toMaybe
                |> Maybe.andThen
                    (mapMeasurementData .counselingSession
                        >> currentValue
                        >> Maybe.map (.value >> Tuple.first)
                    )

        -- All the counseling session records from the past
        historical =
            getChildHistoricalMeasurements childId session.offlineSession
                |> LocalData.map .counselingSessions
                |> LocalData.withDefault Dict.empty

        -- Have we ever completed a counseling session of the specified type?
        completed timing =
            historical
                |> Dict.toList
                |> List.any
                    (\( _, counseling ) -> Tuple.first counseling.value == timing)

        -- How long ago did we complete a session of the specified type?
        completedDaysAgo timing =
            historical
                |> Dict.filter (\_ counseling -> Tuple.first counseling.value == timing)
                |> Dict.toList
                |> List.head
                |> Maybe.map (\( _, counseling ) -> diffDays counseling.dateMeasured session.offlineSession.session.startDate)

        -- How old will the child be as of the scheduled date of the session?
        -- (All of our date calculations are in days here).
        --
        -- It simplifies the rest of the calculation if we avoid making this a
        -- `Maybe`. We've got bigger problems if the session doesn't actually
        -- contain the child, so it should be safe to default the age to 0.
        age =
            getChild childId session.offlineSession
                |> Maybe.andThen
                    (\child ->
                        Maybe.map
                            (\birthDate -> diffDays birthDate session.offlineSession.session.startDate)
                            child.birthDate
                    )
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
expectMotherActivity : NominalDate -> OfflineSession -> PersonId -> MotherActivity -> Bool
expectMotherActivity currentDate offlineSession motherId activity =
    Dict.get motherId offlineSession.participants.byMotherId
        |> Maybe.withDefault []
        |> List.any
            (\participant ->
                case activity of
                    FamilyPlanning ->
                        case participant.adultActivities of
                            MotherActivities ->
                                True

                            CaregiverActivities ->
                                False

                    Lactation ->
                        case participant.adultActivities of
                            MotherActivities ->
                                List.member offlineSession.session.clinicType [ Achi, Fbf ]

                            CaregiverActivities ->
                                False

                    -- Show at FBF groups, for mothers that are breastfeeding.
                    MotherFbf ->
                        case participant.adultActivities of
                            MotherActivities ->
                                if List.member offlineSession.session.clinicType [ Achi, Fbf ] then
                                    let
                                        isBreastfeeding =
                                            getMotherMeasurementData2 motherId offlineSession
                                                |> LocalData.map
                                                    (.current
                                                        >> .lactation
                                                        >> Maybe.map (Tuple.second >> .value >> EverySet.member Breastfeeding)
                                                        >> Maybe.withDefault False
                                                    )
                                                |> LocalData.withDefault False

                                        entitledByUbudehe =
                                            Dict.get motherId offlineSession.mothers
                                                |> Maybe.map
                                                    (\mother ->
                                                        case mother.ubudehe of
                                                            Just Ubudehe1 ->
                                                                True

                                                            Just Ubudehe2 ->
                                                                True

                                                            _ ->
                                                                False
                                                    )
                                                |> Maybe.withDefault False
                                    in
                                    isBreastfeeding && entitledByUbudehe

                                else
                                    False

                            CaregiverActivities ->
                                False

                    ParticipantConsent ->
                        case offlineSession.session.clinicType of
                            Pmtct ->
                                expectParticipantConsent offlineSession motherId
                                    |> Dict.isEmpty
                                    |> not

                            _ ->
                                False
            )


{-| Which participant forms would we expect this mother to consent to in this session?
-}
expectParticipantConsent : OfflineSession -> PersonId -> Dict ParticipantFormId ParticipantForm
expectParticipantConsent session motherId =
    let
        previouslyConsented =
            getMotherHistoricalMeasurements motherId session
                |> LocalData.map
                    (.consents
                        >> Dict.map (\_ consent -> consent.value.formId)
                        >> Dict.values
                        >> EverySet.fromList
                    )
                |> LocalData.withDefault EverySet.empty

        consentedAtCurrentSession =
            getMotherMeasurementData2 motherId session
                |> LocalData.map
                    (.current
                        >> .consent
                        >> Dict.map (\_ consent -> consent.value.formId)
                        >> Dict.values
                        >> EverySet.fromList
                    )
                |> LocalData.withDefault EverySet.empty

        consentedAtPreviousSessions =
            EverySet.diff previouslyConsented consentedAtCurrentSession
    in
    session.allParticipantForms
        |> Dict.filter (\id _ -> not (EverySet.member id consentedAtPreviousSessions))


{-| For a particular child activity, figure out which children have completed
the activity and have the activity pending. (This may not add up to all the
children, because we only consider a child "pending" if they are checked in and
the activity is expected.
-}
summarizeChildActivity : NominalDate -> ZScore.Model.Model -> ChildActivity -> OfflineSession -> Bool -> ModelIndexedDb -> CheckedIn -> CompletedAndPending (Dict PersonId Person)
summarizeChildActivity currentDate zscores activity session isChw db checkedIn =
    checkedIn.children
        |> Dict.filter (\childId _ -> expectChildActivity currentDate zscores session childId isChw db activity)
        |> Dict.partition (\childId _ -> childHasCompletedActivity childId activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| For a particular mother activity, figure out which mothers have completed
the activity and have the activity pending. (This may not add up to all the
mothers, because we only consider a mother "pending" if they are checked in and
the activity is expected.
-}
summarizeMotherActivity : NominalDate -> ZScore.Model.Model -> MotherActivity -> OfflineSession -> Bool -> ModelIndexedDb -> CheckedIn -> CompletedAndPending (Dict PersonId Person)
summarizeMotherActivity currentDate zscores activity session isChw db checkedIn =
    -- For participant consent, we only consider the activity to be completed once
    -- all expected consents have been saved.
    checkedIn.mothers
        |> Dict.filter (\motherId _ -> expectMotherActivity currentDate session motherId activity)
        |> Dict.partition (\motherId _ -> motherHasCompletedActivity motherId activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| This summarizes our summary, by counting, for the given activity, how many
participants are completed or pending.
-}
getParticipantCountForActivity : SummaryByActivity -> Activity -> CompletedAndPending Int
getParticipantCountForActivity summary activity =
    case activity of
        ChildActivity childActivity ->
            summary.children
                |> Dict.get childActivity
                |> Maybe.map
                    (\{ completed, pending } ->
                        { completed = Dict.size completed
                        , pending = Dict.size pending
                        }
                    )
                |> Maybe.withDefault
                    { completed = 0
                    , pending = 0
                    }

        MotherActivity motherActivity ->
            summary.mothers
                |> Dict.get motherActivity
                |> Maybe.map
                    (\{ completed, pending } ->
                        { completed = Dict.size completed
                        , pending = Dict.size pending
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
summarizeChildParticipant : NominalDate -> ZScore.Model.Model -> PersonId -> OfflineSession -> Bool -> ModelIndexedDb -> CompletedAndPending (List ChildActivity)
summarizeChildParticipant currentDate zscores id session isChw db =
    getAllChildActivities session
        |> List.filter (expectChildActivity currentDate zscores session id isChw db)
        |> List.partition (\activity -> childHasCompletedActivity id activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| For a particular mother, figure out which activities are completed
and which are pending. (This may not add up to all the activities, because some
activities may not be expected for this mother).
-}
summarizeMotherParticipant : NominalDate -> ZScore.Model.Model -> PersonId -> OfflineSession -> Bool -> ModelIndexedDb -> CompletedAndPending (List MotherActivity)
summarizeMotherParticipant currentDate zscores id session isChw db =
    getAllMotherActivities session
        |> List.filter (expectMotherActivity currentDate session id)
        |> List.partition (\activity -> motherHasCompletedActivity id activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| This summarizes our summary, by counting how many activities have been
completed for the given mother.

It includes ativities for children of the mother, since we navigate from mother
to child.

-}
getActivityCountForMother : EditableSession -> PersonId -> Person -> SummaryByParticipant -> CompletedAndPending Int
getActivityCountForMother session id mother summary =
    let
        motherCount =
            Dict.get id summary.mothers
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
        (\( childId, _ ) accum ->
            Dict.get childId summary.children
                |> Maybe.map
                    (\activities ->
                        { pending = accum.pending + List.length activities.pending
                        , completed = accum.completed + List.length activities.completed
                        }
                    )
                |> Maybe.withDefault accum
        )
        motherCount
        (getChildren id session.offlineSession)


hasCompletedChildActivity : ChildActivity -> MeasurementData ChildMeasurements -> Bool
hasCompletedChildActivity activityType measurements =
    case activityType of
        ChildFbf ->
            isCompleted (Maybe.map Tuple.second measurements.current.fbf)

        ChildPicture ->
            isCompleted (Maybe.map Tuple.second measurements.current.photo)

        -- Counseling ->
        --    isCompleted (Maybe.map Tuple.second measurements.current.counselingSession)
        Height ->
            isCompleted (Maybe.map Tuple.second measurements.current.height)

        Weight ->
            isCompleted (Maybe.map Tuple.second measurements.current.weight)

        Muac ->
            isCompleted (Maybe.map Tuple.second measurements.current.muac)

        NutritionSigns ->
            isCompleted (Maybe.map Tuple.second measurements.current.nutrition)

        ContributingFactors ->
            isCompleted (Maybe.map Tuple.second measurements.current.contributingFactors)

        FollowUp ->
            isCompleted (Maybe.map Tuple.second measurements.current.followUp)

        Activity.Model.HealthEducation ->
            isCompleted (Maybe.map Tuple.second measurements.current.healthEducation)

        Activity.Model.SendToHC ->
            isCompleted (Maybe.map Tuple.second measurements.current.sendToHC)


childHasCompletedActivity : PersonId -> ChildActivity -> OfflineSession -> Bool
childHasCompletedActivity childId activityType session =
    getChildMeasurementData2 childId session
        |> LocalData.map (hasCompletedChildActivity activityType)
        |> LocalData.withDefault False


hasCompletedMotherActivity : OfflineSession -> PersonId -> MotherActivity -> MeasurementData MotherMeasurements -> Bool
hasCompletedMotherActivity session motherId activityType measurements =
    case activityType of
        FamilyPlanning ->
            isCompleted (Maybe.map Tuple.second measurements.current.familyPlanning)

        Lactation ->
            isCompleted (Maybe.map Tuple.second measurements.current.lactation)

        MotherFbf ->
            isCompleted (Maybe.map Tuple.second measurements.current.fbf)

        ParticipantConsent ->
            -- We only consider this activity completed if all expected
            -- consents have been saved.
            let
                current =
                    mapMeasurementData .consent measurements
                        |> currentValues
                        |> List.map (Tuple.second >> .value >> .formId)
                        |> EverySet.fromList

                expected =
                    expectParticipantConsent session motherId
            in
            (Dict.isEmpty expected |> not)
                && (expected
                        |> Dict.toList
                        |> List.all (\( id, _ ) -> EverySet.member id current)
                   )


motherHasCompletedActivity : PersonId -> MotherActivity -> OfflineSession -> Bool
motherHasCompletedActivity motherId activityType session =
    getMotherMeasurementData2 motherId session
        |> LocalData.map (hasCompletedMotherActivity session motherId activityType)
        |> LocalData.withDefault False


{-| Should some measurement be considered completed? Note that this means that it has
been entered locally, not that it has been saved to the backend.
-}
isCompleted : Maybe value -> Bool
isCompleted =
    isJust


hasAnyCompletedMotherActivity : OfflineSession -> PersonId -> MeasurementData MotherMeasurements -> Bool
hasAnyCompletedMotherActivity session motherId measurements =
    getAllMotherActivities session
        |> List.any (\activity -> hasCompletedMotherActivity session motherId activity measurements)


hasAnyCompletedChildActivity : OfflineSession -> MeasurementData ChildMeasurements -> Bool
hasAnyCompletedChildActivity session measurements =
    getAllChildActivities session
        |> List.any (\a -> hasCompletedChildActivity a measurements)


{-| See whether either the mother, or any of her children, has any completed activity.

If we can't find the mother, we return False.

-}
motherOrAnyChildHasAnyCompletedActivity : PersonId -> OfflineSession -> Bool
motherOrAnyChildHasAnyCompletedActivity motherId session =
    let
        motherHasOne =
            motherHasAnyCompletedActivity motherId session

        anyChildHasOne =
            getChildren motherId session
                |> List.any (\( childId, _ ) -> childHasAnyCompletedActivity childId session)
    in
    motherHasOne || anyChildHasOne


{-| Has the mother been marked as checked in?

We'll return true if the mother has been explicitly checked-in in the UI, or
has a completed activity ... that way, we can freely change the explicit
check-in (and activities) without worrying about synchronizing the two.

-}
motherIsCheckedIn : PersonId -> OfflineSession -> Bool
motherIsCheckedIn motherId session =
    let
        explicitlyCheckedIn =
            getMotherMeasurementData2 motherId session
                |> LocalData.map (.current >> .attendance >> getMeasurementValueFunc >> (==) (Just True))
                |> LocalData.withDefault False

        hasCompletedActivity =
            motherOrAnyChildHasAnyCompletedActivity motherId session
    in
    explicitlyCheckedIn || hasCompletedActivity


childIsCheckedIn : PersonId -> OfflineSession -> Bool
childIsCheckedIn childId session =
    getMyMother childId session
        |> Maybe.map Tuple.first
        |> Maybe.map (\motherId -> motherIsCheckedIn motherId session)
        |> Maybe.withDefault False


{-| Does the mother herself have any completed activity?
-}
motherHasAnyCompletedActivity : PersonId -> OfflineSession -> Bool
motherHasAnyCompletedActivity motherId session =
    getMotherMeasurementData2 motherId session
        |> LocalData.map (hasAnyCompletedMotherActivity session motherId)
        |> LocalData.withDefault False


{-| Does the child have any completed activity?
-}
childHasAnyCompletedActivity : PersonId -> OfflineSession -> Bool
childHasAnyCompletedActivity childId session =
    getChildMeasurementData2 childId session
        |> LocalData.map (hasAnyCompletedChildActivity session)
        |> LocalData.withDefault False
