module Pages.Participants.Update exposing (update)

import Pages.Page exposing (SessionPage)
import Pages.Participants.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg, Maybe SessionPage )
update msg model =
    case msg of
        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )
