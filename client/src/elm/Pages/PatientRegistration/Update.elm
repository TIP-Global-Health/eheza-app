module Pages.PatientRegistration.Update exposing (update)

import App.Model
import Form
import Form.Field exposing (FieldValue(..))
import Pages.PatientRegistration.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        DropZoneComplete result ->
            -- The `fid` being Nothing signifies that we haven't uploaded this to
            -- the backend yet, so we don't know what file ID the backend will
            -- ultimately give it.
            ( { model
                | photo =
                    Just
                        { url = result.url
                        , fid = Nothing
                        }
              }
            , Cmd.none
            , []
            )

        MsgRegistrationForm subMsg ->
            let
                -- If one checkbox gets enabled, disable the other one.
                updatedForm =
                    case subMsg of
                        Form.Input "isMale" Form.Checkbox (Bool True) ->
                            Form.update validateRegistrationForm (Form.Input "isFemale" Form.Checkbox (Bool False)) model.registrationForm

                        Form.Input "isFemale" Form.Checkbox (Bool True) ->
                            Form.update validateRegistrationForm (Form.Input "isMale" Form.Checkbox (Bool False)) model.registrationForm

                        _ ->
                            model.registrationForm
            in
            ( { model | registrationForm = Form.update validateRegistrationForm subMsg updatedForm }, Cmd.none, [] )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetRegistrationStep step ->
            ( { model | registrationStep = step }, Cmd.none, [] )

        Submit ->
            ( model, Cmd.none, [] )
