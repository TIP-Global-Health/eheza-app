module Pages.Admin.Update exposing (..)

import App.Model
import Pages.Admin.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        MsgBackend subMsg ->
            ( model
            , Cmd.none
            , [ App.Model.MsgLoggedIn (App.Model.MsgBackend subMsg) ]
            )
