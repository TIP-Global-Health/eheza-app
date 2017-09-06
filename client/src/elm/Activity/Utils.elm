module Activity.Utils
    exposing
        ( getActivityList
        , getActivityTypeList
        , getActivityIdentity
        , getTotalsNumberPerActivity
        , participantGotPendingActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Child.Model exposing (Child)
import Date exposing (Date)
import Dict exposing (Dict)
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

@todo: Add also "future"?

-}
getActivityList : Date -> ParticipantTypeFilter -> ParticipantsDict -> List ActivityListItem
getActivityList currentDate participantTypeFilter participants =
    List.map
        (\activityType ->
            { activity = getActivityIdentity activityType
            , totals = getTotalsNumberPerActivity currentDate activityType participants
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


getTotalsNumberPerActivity : Date -> ActivityType -> ParticipantsDict -> ( Int, Int )
getTotalsNumberPerActivity currentDate activityType participants =
    Dict.foldl
        (\_ participant ( pending, total ) ->
            case participant.info of
                ParticipantChild child ->
                    case activityType of
                        Child childActivityType ->
                            if hasPendingChildActivity currentDate child childActivityType then
                                ( pending + 1, total + 1 )
                            else
                                ( pending, total + 1 )

                        _ ->
                            ( pending, total )

                ParticipantMother mother ->
                    case activityType of
                        Mother motherActivityType ->
                            if hasPendingMotherActivity currentDate mother motherActivityType then
                                ( pending + 1, total + 1 )
                            else
                                ( pending, total + 1 )

                        _ ->
                            ( pending, total )
        )
        ( 0, 0 )
        participants


hasPendingChildActivity : Date -> Child -> ChildActivityType -> Bool
hasPendingChildActivity currentDate child childActivityType =
    let
        property =
            case childActivityType of
                ChildPicture ->
                    .childPicture

                Height ->
                    .height

                Weight ->
                    .weight

                Muac ->
                    .muac

                NutritionSigns ->
                    .nutritionSigns

                ProgressReport ->
                    .progressReport
    in
        Maybe.map
            (\date ->
                Date.toTime
                    date
                    <= Date.toTime currentDate
            )
            (child.activityDates |> property)
            |> Maybe.withDefault False


hasPendingMotherActivity : Date -> Mother -> MotherActivityType -> Bool
hasPendingMotherActivity currentDate mother motherActivityType =
    let
        property =
            case motherActivityType of
                FamilyPlanning ->
                    .familyPlanning
    in
        mother.activityDates
            |> property
            |> Maybe.map
                (\date -> Date.toTime date <= Date.toTime currentDate)
            |> Maybe.withDefault False


participantGotPendingActivity : Date -> Participant -> Bool
participantGotPendingActivity currentDate participant =
    case participant.info of
        ParticipantChild child ->
            (List.length <| List.filter (hasPendingChildActivity currentDate child) getAllChildActivities) > 0

        ParticipantMother mother ->
            (List.length <| List.filter (hasPendingMotherActivity currentDate mother) getAllMotherActivities) > 0
