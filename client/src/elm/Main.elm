module Main exposing (..)

import App.Model exposing (Flags, Model, Msg)
import App.Update exposing (init, update, subscriptions)
import App.Router exposing (..)
import App.View exposing (view)
import RouteUrl
import Update.Extra exposing (sequence)


main : RouteUrl.RouteUrlProgram Flags Model Msg
main =
    RouteUrl.programWithFlags
        { delta2url = delta2url
        , location2messages = location2messages
        , init = App.Update.init
        , update = updateAndThenFetchWhatTheViewNeeds
        , view = App.View.view
        , subscriptions = App.Update.subscriptions
        }


{-| This integrates the `fetch` function descibed in `Pages.OpenSessions.View`
and `App.View` into the Elm architecture. The idea is to manage data from the
backend central (for the sake of a single source of truth), and yet allow the
view hierarchy to tell us which data needs to be loaded (since that depends on
the way the `view` function is dispatched, not the `update` function).
-}
updateAndThenFetchWhatTheViewNeeds : Msg -> Model -> ( Model, Cmd Msg )
updateAndThenFetchWhatTheViewNeeds msg model =
    let
        initialResult =
            App.Update.update msg model

        fetchMsgs =
            -- Of course, it's the new model that's relevant, since that's the
            -- one the `view` function will be getting. Now, we could actually
            -- integrate this with `animationFrame`, since we don't really need
            -- to check faster than the `view` method will actually be called.
            App.View.fetch (Tuple.first initialResult)
    in
        -- Note that we call ourselves recursively. So, it's vitally important
        -- that the `fetch` implementations use a `WebData`-like strategy to
        -- indicate that a request is in progress, and doesn't need to be triggered
        -- again. Otherwise, we'll immediately be in an infinite loop.
        --
        -- We could avoid that by sequencing through `App.Update.update`
        -- instead.  However, that would cause similar problems more slowly, so
        -- it's probably best to blow through the stack frames quickly ... that
        -- way, we can fix it.  And, you could imagine cases in which the fetch
        -- actually triggers another fetch in a way that will end, and is
        -- desirable.
        sequence updateAndThenFetchWhatTheViewNeeds fetchMsgs initialResult
