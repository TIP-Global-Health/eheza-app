module Activity.Utils
    exposing
        ( getActivityNameAndIcon
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


{-| Get the pending and completed activities.

@todo: Add also "future"?
@todo: Check Date, not just "pending"
-}
getActivityList : Date -> PatientsDict -> List ActivityListItem
getActivityList currentDate pateints =
    [ { activity =
            { name = "Weight"
            , icon = "law"
            }
      , remaining = 3
      }
    , { activity =
            { name = "Height"
            , icon = "line chart"
            }
      , remaining = 2
      }
    , { activity =
            { name = "MUAC"
            , icon = "treatment"
            }
      , remaining = 4
      }
    , { activity =
            { name = "Education"
            , icon = "student"
            }
      , remaining = 5
      }
    , { activity =
            { name = "Nutrition signs"
            , icon = "heartbeat"
            }
      , remaining = 4
      }
    , { activity =
            { name = "Aheza"
            , icon = "food"
            }
      , remaining = 7
      }
    , { activity =
            { name = "Family planning"
            , icon = "users"
            }
      , remaining = 4
      }
    , { activity =
            { name = "HIV"
            , icon = "doctor"
            }
      , remaining = 4
      }
    , { activity =
            { name = "Attendance"
            , icon = "thumbs outline up"
            }
      , remaining = 0
      }
    , { activity =
            { name = "Take pictures"
            , icon = "photo"
            }
      , remaining = 0
      }
    , { activity =
            { name = "Progress reports"
            , icon = "bar chart"
            }
      , remaining = 0
      }
    ]


getActivityNameAndIcon : ActivityType -> ActivityIdentity
getActivityNameAndIcon activityType =
    case activityType of
        Child childActivityType ->
            case childActivityType of
                ChildPicture ->
                    ActivityIdentity "" ""

                Height ->
                    ActivityIdentity "" ""

                Weight ->
                    ActivityIdentity "" ""

                Muac ->
                    ActivityIdentity "" ""

                ProgressReport ->
                    ActivityIdentity "" ""

        Mother motherActivityType ->
            case motherActivityType of
                Aheza ->
                    ActivityIdentity "" ""

                Attendance ->
                    ActivityIdentity "" ""

                Education ->
                    ActivityIdentity "" ""

                FamilyPlanning ->
                    ActivityIdentity "" ""

                Hiv ->
                    ActivityIdentity "" ""

                MotherPicture ->
                    ActivityIdentity "" ""

                NutritionSigns ->
                    ActivityIdentity "" ""


getPendingNumberPerActivity : ActivityType -> PatientsDict -> Int
getPendingNumberPerActivity activityType patients =
    Dict.foldl
        (\_ patient accum ->
            let
                hasPending =
                    case patient.info of
                        PatientChild child ->
                            case activityType of
                                Child childActivityType ->
                                    hasPendingChildActivity childActivityType child

                                _ ->
                                    False

                        PatientMother mother ->
                            case activityType of
                                Mother motherActivityType ->
                                    hasPendingMotherActivity motherActivityType mother

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


hasPendingChildActivity : ChildActivityType -> Child -> Bool
hasPendingChildActivity childActivityType child =
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
                -- @todo: Check date is now.
                True
            )
            (child.childActivityDates |> property)
            |> Maybe.withDefault False


{-| @todo: Implement
-}
hasPendingMotherActivity : MotherActivityType -> Mother -> Bool
hasPendingMotherActivity motherActivityType mother =
    case motherActivityType of
        Aheza ->
            False

        Attendance ->
            False

        Education ->
            False

        FamilyPlanning ->
            False

        Hiv ->
            False

        MotherPicture ->
            False

        NutritionSigns ->
            False
