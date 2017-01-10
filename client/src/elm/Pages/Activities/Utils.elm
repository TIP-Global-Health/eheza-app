module Pages.Activities.Utils exposing (isActivityOpen, isActivityCompleted)

import Pages.Activities.Model exposing (ActivityReport)


isActivityOpen : ActivityReport -> Bool
isActivityOpen activity =
    activity.remaining /= 0


isActivityCompleted : ActivityReport -> Bool
isActivityCompleted activity =
    not <| isActivityOpen activity
