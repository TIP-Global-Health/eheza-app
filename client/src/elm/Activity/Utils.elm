module Activity.Utils
    exposing
        ( getActivityList
        , getActivityNameAndIcon
        , getPendingNumberPerActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Child.Model exposing (Child)
import Date exposing (Date)
import Dict exposing (Dict)
import Mother.Model exposing (Mother)
import Patient.Model exposing (PatientsDict, PatientType(..))


{-|

@todo: Use EveryDict

-}
getActivityDict : Date -> PatientsDict -> Dict String Int
getActivityDict currentDate pateints =
    Dict.foldl
        (\_ patient resultDict ->
            case patient.info of
                PatientChild child ->
                    resultDict

                PatientMother mother ->
                    resultDict
        )
        Dict.empty
        pateints


getActivityTypeList : List ActivityType
getActivityTypeList =
    [ Activity.Model.Child ChildPicture
    , Activity.Model.Child Height
    , Activity.Model.Child Muac
    , Activity.Model.Child ProgressReport
    , Activity.Model.Child Weight
      -- Mother
    , Activity.Model.Mother Aheza
    , Activity.Model.Mother Attendance
    , Activity.Model.Mother Education
    , Activity.Model.Mother FamilyPlanning
    , Activity.Model.Mother Hiv
    , Activity.Model.Mother MotherPicture
    , Activity.Model.Mother NutritionSigns
    ]


{-| Get the pending and completed activities.

@todo: Add also "future"?
@todo: Check Date, not just "pending"
-}
getActivityList : Date -> PatientsDict -> List ActivityListItem
getActivityList currentDate patients =
    List.map
        (\activityType ->
            { activity = getActivityNameAndIcon activityType
            , remaining = getPendingNumberPerActivity currentDate activityType patients
            }
        )
        getActivityTypeList


getActivityNameAndIcon : ActivityType -> ActivityIdentity
getActivityNameAndIcon activityType =
    case activityType of
        Child childActivityType ->
            case childActivityType of
                ChildPicture ->
                    ActivityIdentity "Take pictures (Child)" "photo"

                Height ->
                    ActivityIdentity "Height" "line chart"

                Weight ->
                    ActivityIdentity "Weight" "law"

                Muac ->
                    ActivityIdentity "MUAC" "treatment"

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

                NutritionSigns ->
                    ActivityIdentity "Nutrition signs" "heartbeat"


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

                ProgressReport ->
                    .progressReport
    in
        Maybe.map
            (\date ->
                Date.toTime date <= Date.toTime currentDate
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

                NutritionSigns ->
                    .nutritionSigns
    in
        Maybe.map
            (\date ->
                Date.toTime date <= Date.toTime currentDate
            )
            (mother.activityDates |> property)
            |> Maybe.withDefault False
