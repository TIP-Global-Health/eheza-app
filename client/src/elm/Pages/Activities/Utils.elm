module Pages.Activities.Utils exposing (isActivityOpen, isActivityCompleted)

import Pages.Activities.Model exposing (Activity)


isActivityOpen : Activity -> Bool
isActivityOpen activity =
    activity.remaining /= 0


isActivityCompleted : Activity -> Bool
isActivityCompleted activity =
    (isActivityOpen activity) == False
