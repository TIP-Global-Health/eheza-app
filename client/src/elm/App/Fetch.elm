module App.Fetch exposing (..)

import App.Model exposing (..)
import Pages.OpenSessions.Fetch
import Pages.Page exposing (Page(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Login exposing (maybeData)


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
    []



-- Will re-implement for this part of the new UI
{-
   case model.activePage of
       OpenSessionsPage ->
           model.configured.login
               |> maybeData
               |> Maybe.map (\data -> Pages.OpenSessions.Fetch.fetch model.currentDate data.backend.clinics data.backend.openSessions)
               |> Maybe.withDefault []

       _ ->
           -- For now, we've only implemented this pattern for OpenSessions.
           []
-}
