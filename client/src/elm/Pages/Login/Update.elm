module Pages.Login.Update exposing (update)

import Pages.Login.Model exposing (..)


{-| TODO: We could ask for a `LoginStatus` as well, and prevent certain
actions depending on the status. However, the UI should take care of that ...
i.e. the UI shouldn't let the user do invalid things. So, it's not critical.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        ClearNameAndPassword ->
            ( emptyModel, Cmd.none, Nothing )

        HandleLoginClicked ->
            -- Once "login" is clicked, we immediaately forget the password.
            -- But we hang on to the username, so the user doesn't have to
            -- re-type that if an error occurs.
            ( { model | pass = "" }
            , Cmd.none
            , Just (TryLogin model.name model.pass)
            )

        HandleLogoutClicked ->
            -- We clear the password from the UI if logout is clicked. We
            -- leave the username, as that might be a convenience.
            ( { model | pass = "" }
            , Cmd.none
            , Just Logout
            )

        SetName name ->
            ( { model | name = name }
            , Cmd.none
            , Nothing
            )

        SetPassword pass ->
            ( { model | pass = pass }
            , Cmd.none
            , Nothing
            )
