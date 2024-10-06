module App.Fetch exposing (fetch)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Pages.Completion.Fetch
import Pages.Reports.Fetch
import Pages.Scoreboard.Fetch


{-| See the comment in Pages.OpenSessions.Fetch for an explanation of this.
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
    case model.activePage of
        ScoreboardMenu ->
            []

        Scoreboard ->
            Pages.Scoreboard.Fetch.fetch model.backend model.scoreboardPage
                |> List.map (\subMsg -> MsgBackend subMsg)

        ReportsMenu ->
            []

        Reports ->
            Pages.Reports.Fetch.fetch model.backend model.reportsPage
                |> List.map (\subMsg -> MsgBackend subMsg)

        CompletionMenu ->
            []

        Completion ->
            Pages.Completion.Fetch.fetch model.backend model.completionPage
                |> List.map (\subMsg -> MsgBackend subMsg)

        NotFound ->
            []
