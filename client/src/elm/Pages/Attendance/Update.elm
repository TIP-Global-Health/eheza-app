module Pages.Attendance.Update exposing (update)

import Backend.Session.Model
import Pages.Attendance.Model exposing (..)
import Pages.Model exposing (MsgSession)


update : Msg -> Model -> ( Model, Cmd Msg, List MsgSession )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ Pages.Model.SetActivePage page ]
            )

        SetCheckedIn motherId checkedIn ->
            ( model
            , Cmd.none
            , [ Pages.Model.MsgEditableSession <|
                    Backend.Session.Model.SetCheckedIn motherId checkedIn
              ]
            )

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , []
            )
