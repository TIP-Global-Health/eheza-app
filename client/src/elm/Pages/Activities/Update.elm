module Pages.Activities.Update exposing (update)

import Backend.Session.Model exposing (EditableSession)
import Pages.Activities.Model exposing (Model, Msg(..))
import Pages.Model exposing (MsgSession(..))
import Pages.Page exposing (Page(..), UserPage(..))


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
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
