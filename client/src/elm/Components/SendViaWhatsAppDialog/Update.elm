module Components.SendViaWhatsAppDialog.Update exposing (update)

import App.Model
import Components.SendViaWhatsAppDialog.Model exposing (..)


update : Msg -> Model -> ( Model, List App.Model.Msg )
update msg model =
    case msg of
        SetState state ->
            let
                _ =
                    Debug.log "state" state
            in
            ( { model | state = state }, [] )
