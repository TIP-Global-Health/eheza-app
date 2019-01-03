module Pages.PatientRegistration.Update exposing (update)

import App.Model
import Form
import Form.Field exposing (FieldValue(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.PatientRegistration.Model exposing (..)
import Pages.PatientRegistration.Utils exposing (getFormFieldValue, sequenceExtra)
import Time.Date


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
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
                extraMsgs =
                    case subMsg of
                        -- If "isMale" checkbox gets enabled, disable isFemale" checkbox.
                        Form.Input "isMale" Form.Checkbox (Bool True) ->
                            [ MsgRegistrationForm (Form.Input "isFemale" Form.Checkbox (Bool False)) ]

                        -- If "isFemale" checkbox gets enabled, disable isMale" checkbox.
                        Form.Input "isFemale" Form.Checkbox (Bool True) ->
                            [ MsgRegistrationForm (Form.Input "isMale" Form.Checkbox (Bool False)) ]

                        Form.Input input Form.Select (String value) ->
                            let
                                dayOfBirth =
                                    Form.getFieldAsString "dayOfBirth" model.registrationForm

                                monthOfBirth =
                                    Form.getFieldAsString "monthOfBirth" model.registrationForm

                                yearOfBirth =
                                    Form.getFieldAsString "yearOfBirth" model.registrationForm

                                birthDateExtraMsgs fieldDay fieldMonth fieldYear =
                                    let
                                        day =
                                            getFormFieldValue fieldDay

                                        month =
                                            getFormFieldValue fieldMonth

                                        year =
                                            getFormFieldValue fieldYear
                                    in
                                    if day > 0 && month > 0 && year > 0 then
                                        let
                                            inputDate =
                                                Time.Date.date year month day

                                            inputDay =
                                                Time.Date.day inputDate

                                            inputMonth =
                                                Time.Date.month inputDate

                                            inputYear =
                                                Time.Date.year inputDate

                                            currentDay =
                                                Time.Date.day currentDate

                                            currentMonth =
                                                Time.Date.month currentDate

                                            currentYear =
                                                Time.Date.year currentDate
                                        in
                                        if currentYear == inputYear && currentMonth < inputMonth then
                                            -- Per selected month, we understand that we got future date as input.
                                            -- Need to update input month to current month.
                                            [ MsgRegistrationForm (Form.Input "monthOfBirth" Form.Select (String (toString currentMonth))) ]

                                        else if currentYear == inputYear && currentMonth == inputMonth && currentDay < inputDay then
                                            -- Per selected day, we understand that we got future date as input.
                                            -- Need to update input day to current day.
                                            [ MsgRegistrationForm (Form.Input "dayOfBirth" Form.Select (String (toString currentDay))) ]

                                        else if day /= inputDay then
                                            -- We got invalid day as input, and this was fixed by Time.Date.date.
                                            -- Need to update input day to fixed value.
                                            [ MsgRegistrationForm (Form.Input "dayOfBirth" Form.Select (String (toString currentDay))) ]

                                        else
                                            []

                                    else
                                        []
                            in
                            case input of
                                "dayOfBirth" ->
                                    birthDateExtraMsgs { dayOfBirth | value = Just value } monthOfBirth yearOfBirth

                                "monthOfBirth" ->
                                    birthDateExtraMsgs dayOfBirth { monthOfBirth | value = Just value } yearOfBirth

                                "yearOfBirth" ->
                                    birthDateExtraMsgs dayOfBirth monthOfBirth { yearOfBirth | value = Just value }

                                _ ->
                                    []

                        _ ->
                            []
            in
            ( { model | registrationForm = Form.update validateRegistrationForm subMsg model.registrationForm }, Cmd.none, [] )
                |> sequenceExtra (update currentDate) extraMsgs

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetRegistrationStep step ->
            ( { model | registrationStep = step }, Cmd.none, [] )

        Submit ->
            ( model, Cmd.none, [] )
