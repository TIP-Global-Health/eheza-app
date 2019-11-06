module Pages.Clinics.Update exposing (update)

import App.Model
import Pages.Clinics.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        MsgIndexedDb indexedDbMsg ->
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb indexedDbMsg ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetClinicType type_ ->
            ( { model | clinicType = type_ }
            , Cmd.none
            , []
            )
