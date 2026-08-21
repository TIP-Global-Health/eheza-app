module Pages.PinCode.Update exposing (update)

import Pages.Page exposing (Page(..))
import Pages.PinCode.Model exposing (Model, Msg(..), OutMsg(..))
import Time
import Time.Extra


update : Time.Posix -> Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update currentTime msg model =
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
                -- An hour from now. Adding to the time already stored would
                -- land in the past whenever more than an hour has passed since
                -- it was set, and the dialog shows again straight away.
                nextNotification =
                    Just <| Time.Extra.add Time.Extra.Hour 1 Time.utc currentTime

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
