module Activity.Utils exposing (decodeActivityFromString, encodeActivityAsString, generateNutritionAssessment, getActivityCountForMother, getActivityIcon, getAllChildActivities, getAllChildActivitiesExcludingNextSteps, getAllMotherActivities, getParticipantCountForActivity, isCaregiver, mandatoryActivitiesCompleted, motherIsCheckedIn, summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Activity.Model exposing (Activity(..), ChildActivity(..), CompletedAndPending, MotherActivity(..), SummaryByActivity, SummaryByParticipant)
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (currentValues, expectNCDAActivity, getMeasurementValueFunc, mapMeasurementData, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession)
import Backend.Session.Utils exposing (getChildMeasurementData2, getChildren, getMotherMeasurementData2)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import LocalData
import Maybe.Extra exposing (isJust)
import Measurement.Utils exposing (expectParticipantConsent)
import SyncManager.Model exposing (SiteFeature)
import ZScore.Model


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
                    "child-fbf"

                ChildPicture ->
                    "picture"

                ContributingFactors ->
                    "contributing-factors"

                -- Counseling ->
                --   "counseling"
                FollowUp ->
                    "follow-up"

                Activity.Model.HealthEducation ->
                    "group-health-education"

                Height ->
                    "height"

                Muac ->
                    "muac"

                NutritionSigns ->
                    "nutrition"

                Activity.Model.SendToHC ->
                    "group-send-to-hc"

                Weight ->
                    "weight"

                NCDA ->
                    "ncda"

        MotherActivity motherActivity ->
            case motherActivity of
                FamilyPlanning ->
                    "family-planning"

                Lactation ->
                    "lactation"

                MotherFbf ->
                    "mother-fbf"

                ParticipantConsent ->
                    "participants-consent"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe Activity
decodeActivityFromString s =
    case s of
        "child-fbf" ->
            Just <| ChildActivity ChildFbf

        "picture" ->
            Just <| ChildActivity ChildPicture

        "contributing-factors" ->
            Just <| ChildActivity ContributingFactors

        "follow-up" ->
            Just <| ChildActivity FollowUp

        "group-health-education" ->
            Just <| ChildActivity Activity.Model.HealthEducation

        -- "counseling" ->
        --  Just <| ChildActivity Counseling
        "height" ->
            Just <| ChildActivity Height

        "muac" ->
            Just <| ChildActivity Muac

        "nutrition" ->
            Just <| ChildActivity NutritionSigns

        "group-send-to-hc" ->
            Just <| ChildActivity Activity.Model.SendToHC

        "weight" ->
            Just <| ChildActivity Weight

        "ncda" ->
            Just <| ChildActivity NCDA

        "family-planning" ->
            Just <| MotherActivity FamilyPlanning

        "lactation" ->
            Just <| MotherActivity Lactation

        "mother-fbf" ->
            Just <| MotherActivity MotherFbf

        "participants-consent" ->
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

                NCDA ->
                    "history"

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

        forFbfOptional =
            case offlineSession.session.clinicType of
                Fbf ->
                    [ NCDA ]

                _ ->
                    []
    in
    forAllGroupTypesMandatory
        ++ nextStepsActivities
        ++ forFbf
        ++ forAllGroupTypesOptional
        ++ forFbfOptional
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
expectChildActivity :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> OfflineSession
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> ChildActivity
    -> Bool
expectChildActivity currentDate zscores features offlineSession childId isChw db activity =
    case activity of
        Muac ->
            Dict.get childId offlineSession.children
                |> Maybe.andThen .birthDate
                |> Maybe.map
                    (\birthDate ->
                        diffMonths birthDate currentDate >= 6
                    )
                |> Maybe.withDefault False

        {- Counseling ->
           Maybe.Extra.isJust <|
               expectCounselingActivity session childId
        -}
        ChildFbf ->
            List.member offlineSession.session.clinicType [ Achi, Fbf ]

        ContributingFactors ->
            mandatoryActivitiesCompleted currentDate zscores features offlineSession childId isChw db
                && (generateNutritionAssessment currentDate zscores childId db offlineSession
                        |> List.isEmpty
                        |> not
                   )

        FollowUp ->
            expectChildActivity currentDate zscores features offlineSession childId isChw db ContributingFactors

        Activity.Model.HealthEducation ->
            expectChildActivity currentDate zscores features offlineSession childId isChw db ContributingFactors

        Activity.Model.SendToHC ->
            expectChildActivity currentDate zscores features offlineSession childId isChw db ContributingFactors

        Activity.Model.NCDA ->
            -- For nurses only, show if child is bellow age of 24 months.
            Dict.get childId offlineSession.children
                |> Maybe.map (expectNCDAActivity currentDate features isChw)
                |> Maybe.withDefault False

        _ ->
            -- In all other cases, we always view the ativity.
            True


mandatoryActivitiesCompleted :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> OfflineSession
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> Bool
mandatoryActivitiesCompleted currentDate zscores features offlineSession childId isChw db =
    childActivitiesCompleted currentDate zscores features offlineSession childId isChw db allMandatoryActivities


childActivitiesCompleted :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> OfflineSession
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> List ChildActivity
    -> Bool
childActivitiesCompleted currentDate zscores features offlineSession childId isChw db activities =
    List.all (childActivityCompleted currentDate zscores features offlineSession childId isChw db) activities


childActivityCompleted :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> OfflineSession
    -> PersonId
    -> Bool
    -> ModelIndexedDb
    -> ChildActivity
    -> Bool
childActivityCompleted currentDate zscores features offlineSession childId isChw db activity =
    (not <| expectChildActivity currentDate zscores features offlineSession childId isChw db activity)
        || childHasCompletedActivity childId activity offlineSession


allMandatoryActivities : List ChildActivity
allMandatoryActivities =
    [ Muac, NutritionSigns, Weight ]


{-| Do we expect this activity to be performed in this session for this mother?
Note that we don't consider whether the mother is checked in here -- just
whether we would expect to perform this action if checked in.
-}
expectMotherActivity : OfflineSession -> PersonId -> MotherActivity -> Bool
expectMotherActivity offlineSession motherId activity =
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
                                    getMotherMeasurementData2 motherId offlineSession
                                        |> LocalData.map
                                            (.current
                                                >> .lactation
                                                >> Maybe.map (Tuple.second >> .value >> EverySet.member Breastfeeding)
                                                >> Maybe.withDefault False
                                            )
                                        |> LocalData.withDefault False

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


{-| For a particular child activity, figure out which children have completed
the activity and have the activity pending. (This may not add up to all the
children, because we only consider a child "pending" if they are checked in and
the activity is expected.
-}
summarizeChildActivity :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> ChildActivity
    -> OfflineSession
    -> Bool
    -> ModelIndexedDb
    -> CheckedIn
    -> CompletedAndPending (Dict PersonId Person)
summarizeChildActivity currentDate zscores features activity session isChw db checkedIn =
    checkedIn.children
        |> Dict.filter (\childId _ -> expectChildActivity currentDate zscores features session childId isChw db activity)
        |> Dict.partition (\childId _ -> childHasCompletedActivity childId activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| For a particular mother activity, figure out which mothers have completed
the activity and have the activity pending. (This may not add up to all the
mothers, because we only consider a mother "pending" if they are checked in and
the activity is expected.
-}
summarizeMotherActivity :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> MotherActivity
    -> OfflineSession
    -> Bool
    -> ModelIndexedDb
    -> CheckedIn
    -> CompletedAndPending (Dict PersonId Person)
summarizeMotherActivity _ _ _ activity session _ _ checkedIn =
    -- For participant consent, we only consider the activity to be completed once
    -- all expected consents have been saved.
    checkedIn.mothers
        |> Dict.filter (\motherId _ -> expectMotherActivity session motherId activity)
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
summarizeChildParticipant :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> PersonId
    -> OfflineSession
    -> Bool
    -> ModelIndexedDb
    -> CompletedAndPending (List ChildActivity)
summarizeChildParticipant currentDate zscores features id session isChw db =
    getAllChildActivities session
        |> List.filter (expectChildActivity currentDate zscores features session id isChw db)
        |> List.partition (\activity -> childHasCompletedActivity id activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| For a particular mother, figure out which activities are completed
and which are pending. (This may not add up to all the activities, because some
activities may not be expected for this mother).
-}
summarizeMotherParticipant :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> PersonId
    -> OfflineSession
    -> Bool
    -> ModelIndexedDb
    -> CompletedAndPending (List MotherActivity)
summarizeMotherParticipant _ _ _ id session _ _ =
    getAllMotherActivities session
        |> List.filter (expectMotherActivity session id)
        |> List.partition (\activity -> motherHasCompletedActivity id activity session)
        |> (\( completed, pending ) -> { completed = completed, pending = pending })


{-| This summarizes our summary, by counting how many activities have been
completed for the given mother.

It includes ativities for children of the mother, since we navigate from mother
to child.

-}
getActivityCountForMother : EditableSession -> PersonId -> SummaryByParticipant -> CompletedAndPending Int
getActivityCountForMother session id summary =
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

        Activity.Model.NCDA ->
            isCompleted (Maybe.map Tuple.second measurements.current.ncda)


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


{-| Adult is considered as a caregiver during session when all og the
children they brought to session are caretaken.
For example, a woman with 3 children, one her own and two others are
caretaken is considered a mother (and shuold be given mother activities).
-}
isCaregiver : PersonId -> OfflineSession -> Bool
isCaregiver personId offlineSession =
    Dict.get personId offlineSession.participants.byMotherId
        |> Maybe.map (List.all (.adultActivities >> (==) CaregiverActivities))
        |> Maybe.withDefault False
