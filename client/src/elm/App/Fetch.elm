module App.Fetch exposing (..)

import App.Model exposing (..)
import App.PageType exposing (Page(..))
import Pages.OpenSessions.Fetch
import RemoteData exposing (RemoteData(..), WebData)


{-| See the comment in Pages.OpenSessions.Fetch for an explanatio of this.
Basically, we're following down the `view` hierarchy to determine, given
what the `view` methods are going to want, what messages we ought to send
in order to get the data they will need.

For the sake of convenience, this isn't called by our own `update` method --
it would need to be called at the end, after the new model is determined.
Instead, it's integrated up one level, in `Main.elm`, where we hook together
the Elm architecture. (This is really a kind of little extension to the Elm
architecture).

As a future optimization, one could actually integrate this with
`animationFrame`, since you don't need to figure out what to fetch for
views. more often than that.

-}
fetch : Model -> List Msg
fetch model =
    -- For now, wait until we have a user to fetch anything else.
    -- This may or may not be the most elegant way to handle that.
    case model.user of
        Success user ->
            case model.activePage of
                OpenSessions ->
                    Pages.OpenSessions.Fetch.fetch model.currentDate model.clinics model.openSessions

                _ ->
                    -- For now, we've only implemented this pattern for OpenSessions.
                    []

        _ ->
            []
