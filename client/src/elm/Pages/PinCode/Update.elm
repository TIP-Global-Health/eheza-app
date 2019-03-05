module Pages.PinCode.Update exposing (update)

import Pages.PinCode.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        ClearPinCode ->
            ( { model | code = "" }
            , Cmd.none
            , Nothing
            )

        HandleLoginClicked ->
            ( { model | code = "" }
            , Cmd.none
            , Just (TryPinCode model.code)
            )

        HandleLogoutClicked ->
            ( { model | code = "" }
            , Cmd.none
            , Just Logout
            )

        SendOutMsg outMsg ->
            ( model
            , Cmd.none
            , Just outMsg
            )

        SetPinCode code ->
            ( { model | code = code }
            , Cmd.none
            , Nothing
            )
