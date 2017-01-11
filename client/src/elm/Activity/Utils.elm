module Activity.Utils
    exposing
        ( getActivityNameAndIcon
        , getPendingNumberPerActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Patient.Model exposing (PatientsDict)


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
    case activityType of
        Child childActivityType ->
            case childActivityType of
                ChildPicture ->
                    0

                Height ->
                    0

                Weight ->
                    0

                Muac ->
                    0

                ProgressReport ->
                    0

        Mother motherActivityType ->
            case motherActivityType of
                Aheza ->
                    0

                Attendance ->
                    0

                Education ->
                    0

                FamilyPlanning ->
                    0

                Hiv ->
                    0

                MotherPicture ->
                    0

                NutritionSigns ->
                    0
