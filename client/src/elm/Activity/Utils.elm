module Activity.Utils
    exposing
        ( getActivityList
        , getActivityTypeList
        , getActivityIdentity
        , getPendingNumberPerActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Child.Model exposing (Child)
import Date exposing (Date)
import Dict exposing (Dict)
import Mother.Model exposing (Mother)
import Participant.Model exposing (ParticipantsDict, ParticipantTypeFilter(..), ParticipantType(..))


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
            , remaining = getPendingNumberPerActivity currentDate activityType participants
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
                            ActivityIdentity "Nutrition signs" "nutrition"

                        ProgressReport ->
                            ActivityIdentity "Progress reports" "bar chart"

                Mother motherActivityType ->
                    case motherActivityType of
                        FamilyPlanning ->
                            ActivityIdentity "Family planning" "planning"
    in
        identityVal activityType


getPendingNumberPerActivity : Date -> ActivityType -> ParticipantsDict -> Int
getPendingNumberPerActivity currentDate activityType participants =
    Dict.foldl
        (\_ participant accum ->
            let
                hasPending =
                    case participant.info of
                        ParticipantChild child ->
                            case activityType of
                                Child childActivityType ->
                                    hasPendingChildActivity currentDate childActivityType child

                                _ ->
                                    False

                        ParticipantMother mother ->
                            case activityType of
                                Mother motherActivityType ->
                                    hasPendingMotherActivity currentDate motherActivityType mother

                                _ ->
                                    False
            in
                if hasPending then
                    accum + 1
                else
                    accum
        )
        0
        participants


hasPendingChildActivity : Date -> ChildActivityType -> Child -> Bool
hasPendingChildActivity currentDate childActivityType child =
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


hasPendingMotherActivity : Date -> MotherActivityType -> Mother -> Bool
hasPendingMotherActivity currentDate motherActivityType mother =
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
