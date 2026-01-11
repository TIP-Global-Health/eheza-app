module Pages.PinCode.Update exposing (update)

import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PinCode.Model exposing (..)
import Time
import Time.Extra


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
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

        SetNextNotification value ->
            ( { model | nextNotification = Just value }
            , Cmd.none
            , Nothing
            )

        HandleNotificationResponse accept ->
            let
                nextNotification =
                    Maybe.map (Time.Extra.add Time.Extra.Hour 1 Time.utc)
                        model.nextNotification

                outMsg =
                    if accept then
                        Just <| SetActivePage MessagingCenterPage

                    else
                        Nothing
            in
            ( { model | nextNotification = nextNotification }
            , Cmd.none
            , outMsg
            )
