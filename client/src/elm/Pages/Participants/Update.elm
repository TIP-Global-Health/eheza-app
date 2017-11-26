module Pages.Participants.Update exposing (update)

import Backend.Session.Model exposing (EditableSession)
import Pages.Model exposing (MsgSession(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Participants.Model exposing (Model, Msg(..))


update : EditableSession -> Msg -> Model -> ( Model, Cmd Msg, List MsgSession )
update session msg model =
    case msg of
        CloseSession ->
            ( { model | showEndSessionDialog = False }
            , Cmd.none
            , [ MsgEditableSession <| Backend.Session.Model.CloseSession
              , SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
              ]
            )

        SetRedirectPage page ->
            ( model
            , Cmd.none
            , [ SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        ShowEndSessionDialog show ->
            ( { model | showEndSessionDialog = show }, Cmd.none, [] )
