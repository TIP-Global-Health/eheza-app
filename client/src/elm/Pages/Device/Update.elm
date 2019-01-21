module Pages.Device.Update exposing (update)

import App.Model
import Pages.Device.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
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

        TrySyncing ->
            ( model
            , Cmd.none
            , [ App.Model.TrySyncing ]
            )
