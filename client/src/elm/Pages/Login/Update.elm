module Pages.Login.Update exposing (update)

import Pages.Login.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        Clear ->
            ( emptyModel, Cmd.none, Nothing )

        SetName name ->
            let
                loginForm =
                    model.loginForm
            in
                ( { model | loginForm = { loginForm | name = name } }
                , Cmd.none
                , Nothing
                )

        SetPassword pass ->
            let
                loginForm =
                    model.loginForm
            in
                ( { model | loginForm = { loginForm | pass = pass } }
                , Cmd.none
                , Nothing
                )

        HandleLoginClicked ->
            ( model
            , Cmd.none
            , Just (TryLogin model.loginForm.name model.loginForm.pass)
            )
