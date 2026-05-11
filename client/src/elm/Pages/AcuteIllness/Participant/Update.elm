module Pages.AcuteIllness.Participant.Update exposing (update)

import App.Model
import Pages.AcuteIllness.Participant.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        MsgBackend msgBackend ->
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb msgBackend ]
            )

        SetViewMode mode ->
            ( { model | viewMode = mode }
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
