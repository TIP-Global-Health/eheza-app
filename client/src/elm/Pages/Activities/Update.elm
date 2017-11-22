module Pages.Activities.Update exposing (update)

import Backend.Session.Model exposing (EditableSession)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Activities.Model exposing (Model, Msg(..))


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : EditableSession -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update session msg model =
    case msg of
        CloseSession ->
            -- For now, just redirect ... will need to do a little more than that.
            ( { model | showEndSessionDialog = False }
            , Cmd.none
            , Just <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )

        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        ShowEndSessionDialog show ->
            ( { model | showEndSessionDialog = show }, Cmd.none, Nothing )
