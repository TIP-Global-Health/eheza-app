module Pages.Attendance.Update exposing (update)

import Backend.Session.Model
import Pages.Attendance.Model exposing (..)
import Pages.Session.Model


update : Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ Pages.Session.Model.SetActivePage page ]
            )

        SetCheckedIn motherId checkedIn ->
            ( model
            , Cmd.none
            , [ Pages.Session.Model.MsgEditableSession <|
                    Backend.Session.Model.SetCheckedIn motherId checkedIn
              ]
            )

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , []
            )
