module Activity.Utils
    exposing
        ( getActivityList
        , getActivityTypeList
        , getActivityIdentity
        , getTotalsNumberPerActivity
        , participantHasPendingActivity
        , hasAnyPendingChildActivity
        , hasAnyPendingMotherActivity
        , hasPendingChildActivity
        , hasPendingMotherActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Child.Model exposing (Child)
import Dict exposing (Dict)
import EverySet
import Examination.Model exposing (ExaminationChild, ExaminationMother, emptyExaminationChild, emptyExaminationMother)
import Maybe.Extra exposing (isNothing)
import Mother.Model exposing (Mother)
import Participant.Model exposing (Participant, ParticipantsDict, ParticipantTypeFilter(..), ParticipantType(..))


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

At the moment, this just looks at the single, mocked examination. Eventually,
this will need to be paramterized in some way to deal with multiple
examinations.

-}
getActivityList : ParticipantTypeFilter -> ParticipantsDict -> List ActivityListItem
getActivityList participantTypeFilter participants =
    List.map
        (\activityType ->
            { activity = getActivityIdentity activityType
            , totals = getTotalsNumberPerActivity activityType participants
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


{-| For the moment, we just look at each participant's single, mocked examination.
This will need to change once we can have more than one.
-}
getTotalsNumberPerActivity : ActivityType -> ParticipantsDict -> ( Int, Int )
getTotalsNumberPerActivity activityType participants =
    Dict.foldl
        (\_ participant ( pending, total ) ->
            case participant.info of
                ParticipantChild child ->
                    case activityType of
                        Child childActivityType ->
                            List.head child.examinations
                                |> Maybe.withDefault emptyExaminationChild
                                |> examinationHasPendingChildActivity childActivityType
                                |> (\result ->
                                        if result then
                                            ( pending + 1, total + 1 )
                                        else
                                            ( pending, total + 1 )
                                   )

                        _ ->
                            ( pending, total )

                ParticipantMother mother ->
                    case activityType of
                        Mother motherActivityType ->
                            List.head mother.examinations
                                |> Maybe.withDefault emptyExaminationMother
                                |> examinationHasPendingMotherActivity motherActivityType
                                |> (\result ->
                                        if result then
                                            ( pending + 1, total + 1 )
                                        else
                                            ( pending, total + 1 )
                                   )

                        _ ->
                            ( pending, total )
        )
        ( 0, 0 )
        participants


{-| This is using a single examination for the child, so will need to change
and become parameterized in some way.
-}
hasPendingChildActivity : Child -> ChildActivityType -> Bool
hasPendingChildActivity child activityType =
    child.examinations
        |> List.head
        |> Maybe.withDefault emptyExaminationChild
        |> examinationHasPendingChildActivity activityType


examinationHasPendingChildActivity : ChildActivityType -> ExaminationChild -> Bool
examinationHasPendingChildActivity childActivityType ex =
    case childActivityType of
        ChildPicture ->
            isNothing ex.photo

        Height ->
            isNothing ex.height

        Weight ->
            isNothing ex.weight

        Muac ->
            isNothing ex.muac

        NutritionSigns ->
            -- In this case, it is an empty set that indicates that it is
            -- pending.  You have to explicitly choose "none" to make it
            -- completed.
            EverySet.isEmpty (Tuple.second ex.nutrition)

        ProgressReport ->
            -- We don't have this in `ExaminationChild` yet, so it's
            -- necessarily pending.
            True


{-| This is using a single examination for the child, so will need to change
and become parameterized in some way.
-}
hasPendingMotherActivity : Mother -> MotherActivityType -> Bool
hasPendingMotherActivity mother activityType =
    mother.examinations
        |> List.head
        |> Maybe.withDefault emptyExaminationMother
        |> examinationHasPendingMotherActivity activityType


examinationHasPendingMotherActivity : MotherActivityType -> ExaminationMother -> Bool
examinationHasPendingMotherActivity motherActivityType ex =
    case motherActivityType of
        FamilyPlanning ->
            -- In this case, it is an empty set that indicates that it is
            -- pending.  You have to explicitly choose "none" to make it
            -- completed.
            EverySet.isEmpty (Tuple.second ex.familyPlanning)


hasAnyPendingMotherActivity : ExaminationMother -> Bool
hasAnyPendingMotherActivity ex =
    getAllMotherActivities
        |> List.any ((flip examinationHasPendingMotherActivity) ex)


hasAnyPendingChildActivity : ExaminationChild -> Bool
hasAnyPendingChildActivity ex =
    getAllChildActivities
        |> List.any ((flip examinationHasPendingChildActivity) ex)


{-| Just looking at the single examination for the moment
Will need to parameterize when we have more than one.
-}
participantHasPendingActivity : Participant -> Bool
participantHasPendingActivity participant =
    case participant.info of
        ParticipantChild child ->
            child.examinations
                |> List.head
                |> Maybe.withDefault emptyExaminationChild
                |> hasAnyPendingChildActivity

        ParticipantMother mother ->
            mother.examinations
                |> List.head
                |> Maybe.withDefault emptyExaminationMother
                |> hasAnyPendingMotherActivity
