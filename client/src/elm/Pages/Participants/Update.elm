module Pages.Participants.Update exposing (update)

import App.PageType exposing (Page(..))
import Pages.Participants.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update msg model =
    case msg of
        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )
