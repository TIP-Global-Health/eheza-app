module Pages.Activities.Update exposing (update)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import App.Ports exposing (bindDropZone)
import Backend.Session.Model exposing (EditableSession)
import Pages.Activities.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Session.Model


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : EditableSession -> Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update session msg model =
    case msg of
        CloseSession ->
            ( { model | showEndSessionDialog = False }
            , Cmd.none
            , [ Pages.Session.Model.MsgSession <| Backend.Session.Model.CloseSession
              , Pages.Session.Model.SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
              ]
            )

        SetRedirectPage page ->
            let
                cmd =
                    case page of
                        -- When switching to ChildPicture activity page, bind
                        -- DropZone to be able to take pictures.
                        UserPage (SessionPage _ (Pages.Page.ActivityPage (ChildActivity ChildPicture))) ->
                            bindDropZone ()

                        _ ->
                            Cmd.none
            in
            ( model
            , cmd
            , [ Pages.Session.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        ShowEndSessionDialog show ->
            ( { model | showEndSessionDialog = show }, Cmd.none, [] )
