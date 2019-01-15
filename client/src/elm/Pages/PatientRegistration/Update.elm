module Pages.PatientRegistration.Update exposing (update)

import App.Model
import Backend.Child.Model exposing (Child, ModeOfDelivery(..))
import Backend.Mother.Model exposing (Mother)
import Backend.Patient.Model exposing (Gender(..), Ubudehe(..))
import Date
import EveryDict
import Form
import Form.Field exposing (FieldValue(..))
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Maybe.Extra exposing (unwrap)
import Pages.PatientRegistration.Model exposing (..)
import Pages.PatientRegistration.Utils
    exposing
        ( generateUuid
        , getFormFieldValue
        , getRegistratingParticipant
        , sequenceExtra
        )
import Participant.Model exposing (ParticipantType(..))
import Time exposing (Time)
import Time.Date
import Uuid


update : Time -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentTime msg model =
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
                currentDate =
                    fromLocalDateTime <| Date.fromTime currentTime

                extraMsgs =
                    case subMsg of
                        Form.Input "isMale" Form.Checkbox (Bool True) ->
                            -- "isMale" checkbox gets enabled => disable isFemale" checkbox.
                            [ MsgRegistrationForm (Form.Input "isFemale" Form.Checkbox (Bool False)) ]

                        Form.Input "isFemale" Form.Checkbox (Bool True) ->
                            -- "isFemale" checkbox gets enabled => disable isMale" checkbox.
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
                                    -- All 3 inputs are set.
                                    if day > 0 && month > 0 && year > 0 then
                                        let
                                            adjustedInputDate =
                                                Time.Date.date year month day

                                            adjustedInputDay =
                                                Time.Date.day adjustedInputDate

                                            adjustedInputMonth =
                                                Time.Date.month adjustedInputDate

                                            adjustedInputYear =
                                                Time.Date.year adjustedInputDate

                                            currentDay =
                                                Time.Date.day currentDate

                                            currentMonth =
                                                Time.Date.month currentDate

                                            currentYear =
                                                Time.Date.year currentDate
                                        in
                                        if currentYear == adjustedInputYear && currentMonth < adjustedInputMonth then
                                            -- Per selected month, we understand that we got future date as input.
                                            -- Need to update input month to current month.
                                            [ MsgRegistrationForm (Form.Input "monthOfBirth" Form.Select (String (toString currentMonth))) ]

                                        else if currentYear == adjustedInputYear && currentMonth == adjustedInputMonth && currentDay < adjustedInputDay then
                                            -- Per selected day, we understand that we got future date as input.
                                            -- Need to update input day to current day.
                                            [ MsgRegistrationForm (Form.Input "dayOfBirth" Form.Select (String (toString currentDay))) ]

                                        else if day /= adjustedInputDay then
                                            -- We got invalid day as input, and this was fixed by Time.Date.date.
                                            -- Need to update input day to fixed value.
                                            [ MsgRegistrationForm (Form.Input "dayOfBirth" Form.Select (String (toString adjustedInputDay))) ]

                                        else
                                            []

                                    else
                                        []
                            in
                            case input of
                                "dayOfBirth" ->
                                    -- Simulate new value for dayOfBirth.
                                    birthDateExtraMsgs { dayOfBirth | value = Just value } monthOfBirth yearOfBirth

                                "monthOfBirth" ->
                                    -- Simulate new value for monthOfBirth.
                                    birthDateExtraMsgs dayOfBirth { monthOfBirth | value = Just value } yearOfBirth

                                "yearOfBirth" ->
                                    -- Simulate new value for yearOfBirth.
                                    birthDateExtraMsgs dayOfBirth monthOfBirth { yearOfBirth | value = Just value }

                                _ ->
                                    []

                        _ ->
                            []
            in
            ( { model | registrationForm = Form.update validateRegistrationForm subMsg model.registrationForm }, Cmd.none, [] )
                |> sequenceExtra (update currentTime) extraMsgs

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetDialogState state ->
            ( { model | dialogState = state }, Cmd.none, [] )

        SetRegistrationStep step ->
            ( { model | registrationStep = step }, Cmd.none, [] )

        Submit ->
            let
                currentDate =
                    fromLocalDateTime <| Date.fromTime currentTime

                dayOfBirth =
                    Form.getFieldAsString "dayOfBirth" model.registrationForm
                        |> getFormFieldValue

                monthOfBirth =
                    Form.getFieldAsString "monthOfBirth" model.registrationForm
                        |> getFormFieldValue

                yearOfBirth =
                    Form.getFieldAsString "yearOfBirth" model.registrationForm
                        |> getFormFieldValue

                maybeRegistratingParticipant =
                    getRegistratingParticipant currentDate dayOfBirth monthOfBirth yearOfBirth

                log =
                    Debug.log "UUID" <| Uuid.toString <| generateUuid currentTime
            in
            case maybeRegistratingParticipant of
                Just participant ->
                    let
                        firstName =
                            Form.getFieldAsString "firstName" model.registrationForm
                                |> .value

                        middleName =
                            Form.getFieldAsString "middleName" model.registrationForm
                                |> .value

                        secondName =
                            Form.getFieldAsString "secondName" model.registrationForm
                                |> .value

                        name =
                            (firstName
                                |> Maybe.withDefault ""
                            )
                                ++ (case middleName of
                                        Just mName ->
                                            " " ++ mName ++ " "

                                        Nothing ->
                                            " "
                                   )
                                ++ (secondName
                                        |> Maybe.withDefault ""
                                   )

                        nationalIdNumber =
                            Form.getFieldAsString "nationalIdNumber" model.registrationForm
                                |> .value

                        avatarUrl =
                            model.photo |> Maybe.andThen (.url >> Just)

                        birthDate =
                            Time.Date.date yearOfBirth monthOfBirth dayOfBirth

                        isDateOfBirthEstimated =
                            Form.getFieldAsBool "isDateOfBirthEstimated" model.registrationForm
                                |> .value
                                |> Maybe.withDefault False

                        gender =
                            Form.getFieldAsString "gender" model.registrationForm
                                |> .value
                                |> unwrap
                                    Male
                                    (\gender ->
                                        case gender of
                                            "female" ->
                                                Female

                                            _ ->
                                                Male
                                    )

                        familyUbudehe =
                            Form.getFieldAsString "familyUbudehe" model.registrationForm
                                |> .value
                                |> Maybe.andThen
                                    (\ubudehe ->
                                        case ubudehe of
                                            "1" ->
                                                Just Ubudehe1

                                            "2" ->
                                                Just Ubudehe2

                                            "3" ->
                                                Just Ubudehe3

                                            "4" ->
                                                Just Ubudehe4

                                            _ ->
                                                Nothing
                                    )

                        province =
                            Nothing

                        district =
                            Form.getFieldAsString "district" model.registrationForm
                                |> .value

                        sector =
                            Form.getFieldAsString "sector" model.registrationForm
                                |> .value

                        cell =
                            Form.getFieldAsString "cell" model.registrationForm
                                |> .value

                        village =
                            Form.getFieldAsString "village" model.registrationForm
                                |> .value

                        telephoneNumber =
                            Form.getFieldAsString "telephoneNumber" model.registrationForm
                                |> .value

                        healthCenterName =
                            Form.getFieldAsString "healthCenterName" model.registrationForm
                                |> .value
                    in
                    case participant of
                        ChildParticipant _ ->
                            let
                                motherID =
                                    Nothing

                                motherUuid =
                                    Nothing

                                modeOfDelivery =
                                    Form.getFieldAsString "modeOfDelivery" model.registrationForm
                                        |> .value
                                        |> Maybe.andThen
                                            (\mode ->
                                                case mode of
                                                    "svd-episiotomy" ->
                                                        Just SpontaneousVaginalDeliveryWithEpisiotomy

                                                    "svd-no-episiotomy" ->
                                                        Just SpontaneousVaginalDeliveryWithoutEpisiotomy

                                                    "vd-vacuum" ->
                                                        Just VaginalDeliveryWithVacuumExtraction

                                                    "cesarean-delivery" ->
                                                        Just CesareanDelivery

                                                    _ ->
                                                        Nothing
                                            )

                                motherName =
                                    Form.getFieldAsString "motherName" model.registrationForm
                                        |> .value

                                motherNationalId =
                                    Form.getFieldAsString "motherNationalId" model.registrationForm
                                        |> .value

                                fatherName =
                                    Form.getFieldAsString "fatherName" model.registrationForm
                                        |> .value

                                fatherNationalId =
                                    Form.getFieldAsString "fatherNationalId" model.registrationForm
                                        |> .value

                                caregiverName =
                                    Form.getFieldAsString "caregiverName" model.registrationForm
                                        |> .value

                                caregiverNationalId =
                                    Form.getFieldAsString "caregiverNationalId" model.registrationForm
                                        |> .value

                                child =
                                    Child name
                                        (firstName |> Maybe.withDefault "")
                                        middleName
                                        (secondName |> Maybe.withDefault "")
                                        nationalIdNumber
                                        avatarUrl
                                        motherID
                                        motherUuid
                                        birthDate
                                        isDateOfBirthEstimated
                                        gender
                                        modeOfDelivery
                                        familyUbudehe
                                        motherName
                                        motherNationalId
                                        fatherName
                                        fatherNationalId
                                        caregiverName
                                        caregiverNationalId
                                        province
                                        district
                                        sector
                                        cell
                                        village
                                        telephoneNumber
                                        healthCenterName
                            in
                            let
                                updatedParticipantsData =
                                    { mothersToRegister = model.participantsData.mothersToRegister
                                    , childrenToRegister = EveryDict.insert (generateUuid currentTime) child model.participantsData.childrenToRegister
                                    }
                            in
                            ( { model | participantsData = updatedParticipantsData }, Cmd.none, [] )

                        MotherParticipant _ ->
                            let
                                mother =
                                    Nothing
                            in
                            ( model, Cmd.none, [] )

                Nothing ->
                    ( model, Cmd.none, [] )
