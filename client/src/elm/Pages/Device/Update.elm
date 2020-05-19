module Pages.Device.Update exposing (update)

import App.Model
import Backend.Model
import Backend.SyncData.Model exposing (emptySyncData)
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

        SetSyncing uuid start ->
            let
                extraMsg =
                    if start then
                        Backend.Model.SaveSyncData uuid emptySyncData

                    else
                        Backend.Model.DeleteSyncData uuid
            in
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb extraMsg ]
            )

        TrySyncing ->
            ( model
            , Cmd.none
            , [ App.Model.TrySyncing ]
            )
