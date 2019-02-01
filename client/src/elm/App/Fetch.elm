module App.Fetch exposing (fetch)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInModel, hasAccessToken)
import Date
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.Admin.Fetch
import Pages.Clinics.Fetch
import Pages.Page exposing (Page(..), UserPage(..))


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
    let
        currentDate =
            fromLocalDateTime (Date.fromTime model.currentTime)
    in
    case model.activePage of
        UserPage (ClinicsPage clinicId) ->
            getLoggedInModel model
                |> Maybe.map
                    (\loggedIn ->
                        Pages.Clinics.Fetch.fetch currentDate clinicId loggedIn.backend
                            |> List.map (MsgLoggedIn << MsgBackend)
                    )
                |> Maybe.withDefault []

        UserPage AdminPage ->
            getLoggedInModel model
                |> Maybe.map
                    (\loggedIn ->
                        Pages.Admin.Fetch.fetch currentDate loggedIn.backend loggedIn.adminPage
                            |> List.map (MsgLoggedIn << MsgPageAdmin)
                    )
                |> Maybe.withDefault []

        _ ->
            -- For now, we've only implemented this pattern for ClinicsPage.
            []
