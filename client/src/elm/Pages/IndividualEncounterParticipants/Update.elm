module Pages.IndividualEncounterParticipants.Update exposing (update)

import App.Model
import Components.PatientsSearchForm.Update
import Pages.IndividualEncounterParticipants.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        MsgPatientsSearchForm subMsg ->
            let
                ( modelUpdated, cmd ) =
                    Components.PatientsSearchForm.Update.update subMsg model
            in
            ( modelUpdated, Cmd.map MsgPatientsSearchForm cmd, [] )
