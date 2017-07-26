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
import Patient.Model exposing (PatientsDict, PatientTypeFilter(..), PatientType(..))


getActivityTypeList : PatientTypeFilter -> List ActivityType
getActivityTypeList patientTypeFilter =
    let
        childrenActivities =
            [ Activity.Model.Child ChildPicture
            , Activity.Model.Child Weight
            , Activity.Model.Child Height
            , Activity.Model.Child Muac
            , Activity.Model.Child NutritionSigns
            ]

        mothersActivities =
            [ Activity.Model.Mother Aheza
            , Activity.Model.Mother Attendance
            , Activity.Model.Mother Education
            , Activity.Model.Mother FamilyPlanning
            , Activity.Model.Mother Hiv
            , Activity.Model.Mother MotherPicture
            ]
    in
        case patientTypeFilter of
            All ->
                childrenActivities ++ mothersActivities

            Children ->
                childrenActivities

            Mothers ->
                mothersActivities


{-| Get the pending and completed activities.

@todo: Add also "future"?

-}
getActivityList : Date -> PatientTypeFilter -> PatientsDict -> List ActivityListItem
getActivityList currentDate patientTypeFilter patients =
    List.map
        (\activityType ->
            { activity = getActivityIdentity activityType
            , remaining = getPendingNumberPerActivity currentDate activityType patients
            }
        )
        (getActivityTypeList patientTypeFilter)


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
                        Aheza ->
                            ActivityIdentity "Aheza" "food"

                        Attendance ->
                            ActivityIdentity "Attendance" "thumbs outline up"

                        Education ->
                            ActivityIdentity "Education" "student"

                        FamilyPlanning ->
                            ActivityIdentity "Family planning" "users"

                        Hiv ->
                            ActivityIdentity "HIV" "doctor"

                        MotherPicture ->
                            ActivityIdentity "Take pictures (Mother)" "photo"
    in
        identityVal activityType


getPendingNumberPerActivity : Date -> ActivityType -> PatientsDict -> Int
getPendingNumberPerActivity currentDate activityType patients =
    Dict.foldl
        (\_ patient accum ->
            let
                hasPending =
                    case patient.info of
                        PatientChild child ->
                            case activityType of
                                Child childActivityType ->
                                    hasPendingChildActivity currentDate childActivityType child

                                _ ->
                                    False

                        PatientMother mother ->
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
        patients


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
                Aheza ->
                    .aheza

                Attendance ->
                    .attendance

                Education ->
                    .education

                FamilyPlanning ->
                    .familyPlanning

                Hiv ->
                    .hiv

                MotherPicture ->
                    .motherPicture
    in
        Maybe.map
            (\date ->
                Date.toTime date <= Date.toTime currentDate
            )
            (mother.activityDates |> property)
            |> Maybe.withDefault False
