module Pages.Device.Update exposing (update)

import App.Model
import DataManager.Model
import Pages.Device.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetCode code ->
            ( { model | code = code }
            , Cmd.none
            , []
            )

        HandlePairClicked ->
            ( { model | code = "" }
            , Cmd.none
            , [ App.Model.TryPairingCode model.code ]
            )
