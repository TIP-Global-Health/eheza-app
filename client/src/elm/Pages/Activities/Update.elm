module Pages.Activities.Update exposing (update)

import Pages.Page exposing (SessionPage(..))
import Pages.Activities.Model exposing (Model, Msg(..))


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe SessionPage )
update msg model =
    case msg of
        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )

        SetRedirectPage page ->
            ( model, Cmd.none, Just page )
