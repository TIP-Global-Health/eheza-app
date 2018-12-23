module Pages.PatientRegistration.Update exposing (update)

import App.Model
import Form
import Pages.PatientRegistration.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        MsgRegistrationForm subMsg ->
            ( { model | registrationForm = Form.update validateRegistrationForm subMsg model.registrationForm }, Cmd.none, [] )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )
