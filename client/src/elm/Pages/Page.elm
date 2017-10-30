module Pages.Page exposing (..)

{-| A module that defines a type which controls what the user wishes
to be shown at the moment. Perhaps could be called `UserAttention`
instead? Or something like that.

This is in a separate module because it avoids circular dependencies
that otherwise tend to arise.

-}

import Activity.Model exposing (ActivityType(..))


{-| What does the user want to see?

It's debatable how much detail to model here vs. elsewhere. For instance,
we could just have

    ActivtyPage

here, and model the `ActivityType` as a `selectedActivity` in `Pages.Activity.Model`.

There probably isn't a universally right answer to that, so we'll do it this
way for now and see how it goes.

-}
type Page
    = ActivitiesPage
    | ActivityPage ActivityType
