module Activity.Utils
    exposing
        ( getActivityNameAndIcon
        , getPendingNumberPerActivity
        )

import Activity.Model exposing (ActivityIdentity, ActivityType(..), ChildActivityType(..))
import Patient.Model exposing (PatientsDict)


getActivityNameAndIcon : ActivityType -> ActivityIdentity
getActivityNameAndIcon activityType =
    ActivityIdentity "" ""


getPendingNumberPerActivity : ChildActivityType -> PatientsDict -> Int
getPendingNumberPerActivity childActivityType patients =
    0
