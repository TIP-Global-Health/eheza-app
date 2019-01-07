module Pages.Device.Update exposing (update)

import Pages.Device.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCode code ->
            ( { model | code = code }
            , Cmd.none
            )

        HandlePairClicked ->
            ( { model | code = "" }
            , Cmd.none
            )
