module Pages.Participants.Update exposing (update)

import Backend.Session.Model exposing (EditableSession)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Participants.Model exposing (Model, Msg(..))
import Pages.Session.Model


update : EditableSession -> Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update session msg model =
    case msg of
        CloseSession ->
            ( { model | showEndSessionDialog = False }
            , Cmd.none
            , [ Pages.Session.Model.MsgEditableSession <| Backend.Session.Model.CloseSession
              , Pages.Session.Model.SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
              ]
            )

        SetRedirectPage page ->
            ( model
            , Cmd.none
            , [ Pages.Session.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        ShowEndSessionDialog show ->
            ( { model | showEndSessionDialog = show }, Cmd.none, [] )
