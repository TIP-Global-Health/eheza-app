module Pages.OfflineSession.Update exposing (update)

import Pages.OfflineSession.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
