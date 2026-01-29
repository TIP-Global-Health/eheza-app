module Pages.Attendance.Update exposing (update)

import Backend.Session.Model
import Measurement.Model
import Pages.Attendance.Model exposing (InitialResultsDisplay(..), Model, Msg(..))
import Pages.Session.Model


update : Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | filter = "", initialResultsDisplay = InitialResultsShown }
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( { model | filter = "" }
            , Cmd.none
            , [ Pages.Session.Model.SetActivePage page ]
            )

        SetCheckedIn attendanceId motherId checkedIn ->
            ( model
            , Cmd.none
            , [ Measurement.Model.SaveAttendance attendanceId checkedIn
                    |> Backend.Session.Model.MeasurementOutMsgMother motherId
                    |> Pages.Session.Model.MsgSession
              ]
            )

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , []
            )

        ToggleInitialResultsDisplay ->
            let
                display =
                    if model.initialResultsDisplay == InitialResultsHidden then
                        InitialResultsShown

                    else
                        InitialResultsHidden
            in
            ( { model | initialResultsDisplay = display }
            , Cmd.none
            , []
            )
