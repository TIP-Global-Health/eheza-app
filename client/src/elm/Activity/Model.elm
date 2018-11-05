module Activity.Model exposing (ActivityListItem, ActivityType(..), ChildActivityType(..), MotherActivityType(..))

{-| This module provides types relating to the UI for presenting activities.

So, we end up with values that represent one activity or another. In fact,
these more or less represent a data-level proxy for the various types in
`Backend.Measurement.Model`.

(Though not entirely, since not all activities necessarily result in
measuremnets ... for instance, `ProgressReport` might relate to whether the
nurse has reviewed the progress report with the mother. I suppose one could
conceive of that as a "measurement" ... especially if it ultimately needs to be
saved to the backend).

-}


type ActivityType
    = ChildActivity ChildActivityType
    | MotherActivity MotherActivityType


type ChildActivityType
    = ChildPicture
    | Height
    | Muac
    | NutritionSigns
    | ProgressReport
    | Weight


type MotherActivityType
    = FamilyPlanning


type alias ActivityListItem =
    { activityType : ActivityType
    , totals : { pending : Int, total : Int }
    }
