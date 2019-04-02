module Pages.ParticipantRegistration.Update exposing (update)

import App.Model
import Backend.Child.Decoder exposing (decodeModeOfDelivery)
import Backend.Child.Model exposing (Child, ModeOfDelivery(..))
import Backend.Measurement.Model exposing (PhotoUrl(..))
import Backend.Model
import Backend.Mother.Decoder exposing (decodeHivStatus)
import Backend.Mother.Model exposing (ChildrenRelationType(..), HIVStatus(..), Mother)
import Backend.Person.Decoder exposing (decodeEducationLevel, decodeGender, decodeMaritalStatus, decodeUbudehe)
import Backend.Person.Model exposing (EducationLevel(..), Gender(..), MaritalStatus(..), Ubudehe(..))
import EveryDict
import Form
import Form.Field exposing (FieldValue(..))
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Pages.Page
import Pages.ParticipantRegistration.Model exposing (..)
import Pages.ParticipantRegistration.Utils exposing (decodeStringToMaybe, getFormFieldValue, getRegistratingParticipant, sequenceExtra)
import Participant.Model exposing (ParticipantId(..), ParticipantType(..))
import Restful.Endpoint exposing (toEntityId, toEntityUuid)
import Time.Date
import Utils.GeoLocation exposing (geoInfo)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        DropZoneComplete result ->
            let
                subMsg =
                    Form.Input "photoUrl" Form.Text (Form.Field.String result.url)

                registrationForm =
                    Form.update validateRegistrationForm subMsg model.registrationForm
            in
            ( { model | registrationForm = registrationForm }
            , Cmd.none
            , []
            )

        MakeRelation participantId ->
            let
                ( newDialogState, backendCmds ) =
                    case participantId of
                        ParticipantMother motherUuid ->
                            case model.relationParticipant of
                                -- We're make relation with a mother, so we should never
                                -- get here, since we're not supposed to relate a
                                -- mother with nother mother.
                                Just (ParticipantMother _) ->
                                    ( Nothing, [] )

                                Just (ParticipantChild childUuid) ->
                                    ( Just <| SuccessfulRelation <| ParticipantChild childUuid
                                    , [ Backend.Model.SetMotherOfChild childUuid motherUuid ]
                                    )

                                -- This should never happen, as there must be a
                                -- relation participant in order to create a relation.
                                Nothing ->
                                    ( Nothing, [] )

                        ParticipantChild childUuid ->
                            case model.relationParticipant of
                                Just (ParticipantMother motherUuid) ->
                                    ( Just <| SuccessfulRelation <| ParticipantMother motherUuid
                                    , [ Backend.Model.SetMotherOfChild childUuid motherUuid ]
                                    )

                                -- We're make relation with a child, so we should never
                                -- get here, since we're not supposed to relate a
                                -- child with another child.
                                Just (ParticipantChild _) ->
                                    ( Nothing, [] )

                                -- This should never happen, as there must be a
                                -- relation participant in order to create a relation.
                                Nothing ->
                                    ( Nothing, [] )
            in
            ( { model | dialogState = newDialogState }
            , Cmd.none
            , List.map App.Model.MsgIndexedDb backendCmds
            )

        MsgRegistrationForm subMsg ->
            let
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
                                        maybeDay =
                                            getFormFieldValue fieldDay

                                        maybeMonth =
                                            getFormFieldValue fieldMonth

                                        maybeYear =
                                            getFormFieldValue fieldYear
                                    in
                                    case ( maybeDay, maybeMonth, maybeYear ) of
                                        -- All 3 inputs are set.
                                        ( Just day, Just month, Just year ) ->
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

                                        _ ->
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
                |> sequenceExtra (update currentDate) extraMsgs

        Reset ->
            ( model, Cmd.none, [] )
                |> sequenceExtra (update currentDate) [ SetActivePage Pages.Page.PinCodePage ]

        SearchForParticipant searchValue ->
            ( { model | submittedSearch = Just searchValue }, Cmd.none, [] )

        SetActivePage page ->
            ( emptyModel, Cmd.none, [ App.Model.SetActivePage page ] )

        SetDialogState state ->
            ( { model | dialogState = state }, Cmd.none, [] )

        SetRegistrationPhase phase ->
            let
                updatedPreviousPhases =
                    case phase of
                        -- We do not want to record every character typed during search.
                        ParticipantSearch _ ->
                            case model.previousPhases of
                                head :: rest ->
                                    case head of
                                        ParticipantSearch _ ->
                                            model.registrationPhase :: rest

                                        _ ->
                                            model.registrationPhase :: model.previousPhases

                                [] ->
                                    [ model.registrationPhase ]

                        _ ->
                            model.registrationPhase :: model.previousPhases
            in
            ( { model | registrationPhase = phase, previousPhases = updatedPreviousPhases }, Cmd.none, [] )

        SetRelationParticipant participantId ->
            ( { model | relationParticipant = participantId, submittedSearch = Nothing, dialogState = Nothing }, Cmd.none, [] )
                |> sequenceExtra (update currentDate) [ SetRegistrationPhase <| ParticipantSearch Nothing ]

        StepBack ->
            case model.previousPhases of
                head :: rest ->
                    ( { model | registrationPhase = head, previousPhases = rest }, Cmd.none, [] )

                [] ->
                    ( model, Cmd.none, [] )

        Submit ->
            let
                maybeDayOfBirth =
                    Form.getFieldAsString "dayOfBirth" model.registrationForm
                        |> getFormFieldValue

                maybeMonthOfBirth =
                    Form.getFieldAsString "monthOfBirth" model.registrationForm
                        |> getFormFieldValue

                maybeYearOfBirth =
                    Form.getFieldAsString "yearOfBirth" model.registrationForm
                        |> getFormFieldValue

                birthData =
                    case ( maybeDayOfBirth, maybeMonthOfBirth, maybeYearOfBirth ) of
                        ( Just dayOfBirth, Just monthOfBirth, Just yearOfBirth ) ->
                            getRegistratingParticipant currentDate maybeDayOfBirth maybeMonthOfBirth maybeYearOfBirth model.relationParticipant
                                |> Maybe.map (\participant -> ( participant, Time.Date.date yearOfBirth monthOfBirth dayOfBirth ))

                        _ ->
                            Nothing
            in
            case birthData of
                Just ( participant, birthDate ) ->
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

                        isDateOfBirthEstimated =
                            Form.getFieldAsBool "isDateOfBirthEstimated" model.registrationForm
                                |> .value
                                |> Maybe.withDefault False

                        gender =
                            Form.getFieldAsString "gender" model.registrationForm
                                |> .value
                                |> Maybe.andThen (decodeStringToMaybe decodeGender)
                                |> Maybe.withDefault Male

                        familyUbudehe =
                            Form.getFieldAsString "familyUbudehe" model.registrationForm
                                |> .value
                                |> Maybe.andThen (decodeStringToMaybe decodeUbudehe)

                        province =
                            Form.getFieldAsString "province" model.registrationForm
                                |> .value
                                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                                |> Maybe.map toEntityId
                                |> Maybe.andThen (\id -> EveryDict.get id geoInfo.provinces)
                                |> Maybe.map .name

                        district =
                            Form.getFieldAsString "district" model.registrationForm
                                |> .value
                                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                                |> Maybe.map toEntityId
                                |> Maybe.andThen (\id -> EveryDict.get id geoInfo.districts)
                                |> Maybe.map .name

                        sector =
                            Form.getFieldAsString "sector" model.registrationForm
                                |> .value
                                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                                |> Maybe.map toEntityId
                                |> Maybe.andThen (\id -> EveryDict.get id geoInfo.sectors)
                                |> Maybe.map .name

                        cell =
                            Form.getFieldAsString "cell" model.registrationForm
                                |> .value
                                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                                |> Maybe.map toEntityId
                                |> Maybe.andThen (\id -> EveryDict.get id geoInfo.cells)
                                |> Maybe.map .name

                        village =
                            Form.getFieldAsString "village" model.registrationForm
                                |> .value
                                |> Maybe.andThen (String.toInt >> Result.toMaybe)
                                |> Maybe.map toEntityId
                                |> Maybe.andThen (\id -> EveryDict.get id geoInfo.villages)
                                |> Maybe.map .name

                        telephoneNumber =
                            Form.getFieldAsString "telephoneNumber" model.registrationForm
                                |> .value

                        healthCenter =
                            Form.getFieldAsString "healthCenter" model.registrationForm
                                |> .value
                                |> Maybe.map toEntityUuid

                        clinic =
                            Form.getFieldAsString "clinic" model.registrationForm
                                |> .value
                                |> Maybe.map toEntityUuid

                        avatar =
                            Form.getFieldAsString "photoUrl" model.registrationForm
                                |> .value
                    in
                    case participant of
                        ChildParticipant _ ->
                            let
                                motherId =
                                    case model.relationParticipant of
                                        Just (ParticipantMother id) ->
                                            Just id

                                        _ ->
                                            Nothing

                                modeOfDelivery =
                                    Form.getFieldAsString "modeOfDelivery" model.registrationForm
                                        |> .value
                                        |> Maybe.andThen (decodeStringToMaybe decodeModeOfDelivery)

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
                                        avatar
                                        motherId
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
                                        healthCenter

                                newDialogState =
                                    case model.relationParticipant of
                                        Just (ParticipantMother motherUuid) ->
                                            Just <| SuccessfulRelation <| ParticipantMother motherUuid

                                        _ ->
                                            Just (Registering participant)
                            in
                            ( { model
                                | dialogState = newDialogState
                                , previousPhases = []
                                , registrationForm = emptyRegistrationForm
                              }
                            , Cmd.none
                            , [ App.Model.MsgIndexedDb <| Backend.Model.PostChild child ]
                            )

                        MotherParticipant _ ->
                            let
                                levelOfEducation =
                                    Form.getFieldAsString "levelOfEducation" model.registrationForm
                                        |> .value
                                        |> Maybe.andThen (decodeStringToMaybe decodeEducationLevel)

                                profession =
                                    Form.getFieldAsString "profession" model.registrationForm
                                        |> .value

                                maritalStatus =
                                    Form.getFieldAsString "maritalStatus" model.registrationForm
                                        |> .value
                                        |> Maybe.andThen (decodeStringToMaybe decodeMaritalStatus)

                                hivStatus =
                                    Form.getFieldAsString "hivStatus" model.registrationForm
                                        |> .value
                                        |> Maybe.andThen (decodeStringToMaybe decodeHivStatus)

                                householdSize =
                                    Form.getFieldAsString "householdSize" model.registrationForm
                                        |> getFormFieldValue

                                numberOfChildren =
                                    Form.getFieldAsString "numberOfChildren" model.registrationForm
                                        |> getFormFieldValue

                                mother =
                                    Mother name
                                        (firstName |> Maybe.withDefault "")
                                        middleName
                                        (secondName |> Maybe.withDefault "")
                                        nationalIdNumber
                                        avatar
                                        (Just birthDate)
                                        isDateOfBirthEstimated
                                        MotherRelation
                                        gender
                                        familyUbudehe
                                        levelOfEducation
                                        profession
                                        maritalStatus
                                        hivStatus
                                        householdSize
                                        numberOfChildren
                                        province
                                        district
                                        sector
                                        cell
                                        village
                                        telephoneNumber
                                        clinic
                                        healthCenter

                                ( newDialogState, relatedChild ) =
                                    case model.relationParticipant of
                                        Just (ParticipantChild childUuid) ->
                                            ( Just <| SuccessfulRelation <| ParticipantChild childUuid
                                            , Just childUuid
                                            )

                                        _ ->
                                            ( Just <| Registering participant
                                            , Nothing
                                            )
                            in
                            -- TODO: We're blanking the form on submission. It
                            -- would be better to go into a state in which
                            -- we're waiting to see what the result is.
                            ( { model
                                | dialogState = newDialogState
                                , previousPhases = []
                                , registrationForm = emptyRegistrationForm
                              }
                            , Cmd.none
                            , [ App.Model.MsgIndexedDb <| Backend.Model.PostMother mother relatedChild ]
                            )

                Nothing ->
                    -- We should not get here, so we have this to satisfy the compiler.
                    ( model, Cmd.none, [] )
