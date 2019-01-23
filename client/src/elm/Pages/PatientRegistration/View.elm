module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Child.Model exposing (ModeOfDelivery(..), modeOfDeliveryToValue)
import Backend.Measurement.Model exposing (PhotoValue)
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..), toStringEducationLevel)
import Backend.Patient.Model exposing (Gender(..), Ubudehe(..))
import Form exposing (Form)
import Form.Error
import Form.Input
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PatientRegistration.Model
    exposing
        ( DialogState(..)
        , Model
        , Msg(..)
        , ParticipantsData
        , RegistrationForm
        , RegistrationPhase(..)
        , RegistrationStep(..)
        )
import Pages.PatientRegistration.Utils exposing (getFormFieldValue, getRegistratingParticipant)
import Participant.Model exposing (ParticipantType(..))
import Time.Date
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import Utils.Form exposing (isFormFieldSet, isFormFieldValid)
import Utils.Html exposing (script, viewModal)


view : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
view language currentDate user backend cache model =
    div [ class "wrap wrap-alt-2" ]
        [ viewHeader language currentDate user backend cache model
        , div
            [ class "ui full segment blue" ]
            [ viewBody language currentDate user backend cache model
            ]
        , viewModal <| viewDialog language model.dialogState
        ]


viewHeader : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
viewHeader language currentDate user backend cache model =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.RegisterNewPatient ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage LoginPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewBody : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
viewBody language currentDate user backend cache model =
    let
        body =
            case model.registrationPhase of
                ParticipantSearch searchString ->
                    viewSearchForm language currentDate user backend cache model.participantsData searchString

                ParticipantRegistration step ->
                    viewRegistrationForm language currentDate user backend cache step model.registrationForm model.photo
    in
    div [ class "content" ]
        [ body ]


viewRegistrationForm :
    Language
    -> NominalDate
    -> User
    -> ModelBackend
    -> ModelCached
    -> RegistrationStep
    -> Form () RegistrationForm
    -> Maybe PhotoValue
    -> Html Msg
viewRegistrationForm language currentDate user backend cache step registrationForm photo =
    let
        -- FORM FIELDS --
        firstName =
            Form.getFieldAsString "firstName" registrationForm

        middleName =
            Form.getFieldAsString "middleName" registrationForm

        secondName =
            Form.getFieldAsString "secondName" registrationForm

        nationalIdNumber =
            Form.getFieldAsString "nationalIdNumber" registrationForm

        dayOfBirth =
            Form.getFieldAsString "dayOfBirth" registrationForm

        monthOfBirth =
            Form.getFieldAsString "monthOfBirth" registrationForm

        yearOfBirth =
            Form.getFieldAsString "yearOfBirth" registrationForm

        isDateOfBirthEstimated =
            Form.getFieldAsBool "isDateOfBirthEstimated" registrationForm

        gender =
            Form.getFieldAsString "gender" registrationForm

        levelOfEducation =
            Form.getFieldAsString "levelOfEducation" registrationForm

        profession =
            Form.getFieldAsString "profession" registrationForm

        maritalStatus =
            Form.getFieldAsString "maritalStatus" registrationForm

        hivStatus =
            Form.getFieldAsString "hivStatus" registrationForm

        modeOfDelivery =
            Form.getFieldAsString "modeOfDelivery" registrationForm

        -- END STEP 1 FIELDS --
        familyUbudehe =
            Form.getFieldAsString "familyUbudehe" registrationForm

        householdSize =
            Form.getFieldAsString "householdSize" registrationForm

        numberOfChildren =
            Form.getFieldAsString "numberOfChildren" registrationForm

        motherName =
            Form.getFieldAsString "motherName" registrationForm

        motherNationalId =
            Form.getFieldAsString "motherNationalId" registrationForm

        fatherName =
            Form.getFieldAsString "fatherName" registrationForm

        fatherNationalId =
            Form.getFieldAsString "fatherNationalId" registrationForm

        caregiverName =
            Form.getFieldAsString "caregiverName" registrationForm

        caregiverNationalId =
            Form.getFieldAsString "caregiverNationalId" registrationForm

        district =
            Form.getFieldAsString "district" registrationForm

        sector =
            Form.getFieldAsString "sector" registrationForm

        cell =
            Form.getFieldAsString "cell" registrationForm

        village =
            Form.getFieldAsString "village" registrationForm

        telephoneNumber =
            Form.getFieldAsString "telephoneNumber" registrationForm

        -- END STEP 2 FIELDS --
        healthCenterName =
            Form.getFieldAsString "healthCenterName" registrationForm

        -- END STEP 3 FIELDS --
        maybeRegistratingParticipant =
            getRegistratingParticipant currentDate
                (getFormFieldValue dayOfBirth)
                (getFormFieldValue monthOfBirth)
                (getFormFieldValue yearOfBirth)

        emptyOption =
            ( "", "" )

        formContent =
            case step of
                First ->
                    let
                        staticComponents =
                            let
                                dayOptions =
                                    emptyOption
                                        :: (List.repeat 31 "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                           )

                                monthOptions =
                                    emptyOption
                                        :: (List.repeat 12 "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                           )

                                currentYear =
                                    Time.Date.year currentDate

                                startFromYear =
                                    currentYear - 99

                                yearOptions =
                                    emptyOption
                                        :: (List.repeat 100 "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + startFromYear, toString <| index + startFromYear ))
                                                |> List.reverse
                                           )
                            in
                            [ viewTextInput language Translate.FirstName firstName True
                            , viewTextInput language Translate.MiddleName middleName False
                            , viewTextInput language Translate.SecondName secondName True
                            , viewTextInput language Translate.NationalIdNumber nationalIdNumber False
                            , div [ class "ui grid" ]
                                [ div [ class "six wide column birtdate-label required" ]
                                    [ text <| translate language Translate.DateOfBirth ++ ":" ]
                                , div [ class "three wide column month-input" ]
                                    [ span [] [ text <| translate language Translate.Month ]
                                    , Form.Input.selectInput monthOptions monthOfBirth []
                                    ]
                                , div [ class "three wide column day-input" ]
                                    [ span [] [ text <| translate language Translate.Day ]
                                    , Form.Input.selectInput dayOptions dayOfBirth []
                                    ]
                                , div [ class "three wide column year-input" ]
                                    [ span [] [ text <| translate language Translate.Year ]
                                    , Form.Input.selectInput yearOptions yearOfBirth []
                                    ]
                                , div [ class "one wide column estimated-input" ]
                                    [ span [] [ text <| translate language Translate.Estimated ]
                                    , Form.Input.checkboxInput isDateOfBirthEstimated []
                                    ]
                                ]
                            ]

                        dynamicComponents =
                            let
                                viewAge delta =
                                    let
                                        age =
                                            if delta.years > 0 then
                                                if delta.years == 1 then
                                                    translate language <| Translate.ChartPhrase Translate.OneYear

                                                else
                                                    translate language <| Translate.ChartPhrase (Translate.YearsPlural delta.years)

                                            else if delta.months > 0 then
                                                if delta.months == 1 then
                                                    translate language <| Translate.AgeSingleMonthWithoutDay 1

                                                else
                                                    translate language <| Translate.AgeMonthsWithoutDay delta.months

                                            else if delta.days == 1 then
                                                translate language <| Translate.AgeSingleDayWithoutMonth 0 1

                                            else
                                                translate language <| Translate.AgeDays delta.days
                                    in
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
                                            [ text "Age:" ]
                                        , div [ class "ten wide column" ]
                                            [ text age ]
                                        ]

                                viewGender =
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
                                            [ text <| translate language Translate.GenderLabel ++ ":" ]
                                        , Form.Input.radioInput "male" gender [ class "one wide column gender-input" ]
                                        , div [ class "three wide column" ]
                                            [ text <| translate language (Translate.Gender Male) ]
                                        , Form.Input.radioInput "female" gender [ class "one wide column gender-input" ]
                                        , div [ class "three wide column" ]
                                            [ text <| translate language (Translate.Gender Female) ]
                                        ]
                            in
                            case maybeRegistratingParticipant of
                                Just (MotherParticipant age) ->
                                    let
                                        motherInputs =
                                            let
                                                viewLevelOfEducation =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( toStringEducationLevel NoSchooling, translate language <| Translate.LevelOfEducation NoSchooling )
                                                                   , ( toStringEducationLevel PrimarySchool, translate language <| Translate.LevelOfEducation PrimarySchool )
                                                                   , ( toStringEducationLevel VocationalTrainingSchool, translate language <| Translate.LevelOfEducation VocationalTrainingSchool )
                                                                   , ( toStringEducationLevel SecondarySchool, translate language <| Translate.LevelOfEducation SecondarySchool )
                                                                   , ( toStringEducationLevel DiplomaProgram, translate language <| Translate.LevelOfEducation DiplomaProgram )
                                                                   , ( toStringEducationLevel HigherEducation, translate language <| Translate.LevelOfEducation HigherEducation )
                                                                   , ( toStringEducationLevel AdvancedDiploma, translate language <| Translate.LevelOfEducation AdvancedDiploma )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.LevelOfEducationLabel options levelOfEducation "ten" "select-input" True

                                                viewProfession =
                                                    viewTextInput language Translate.Profession profession False

                                                viewMaritalStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( String.toLower <| toString Divorced, translate language <| Translate.MaritalStatus Divorced )
                                                                   , ( String.toLower <| toString Maried, translate language <| Translate.MaritalStatus Maried )
                                                                   , ( String.toLower <| toString Single, translate language <| Translate.MaritalStatus Single )
                                                                   , ( String.toLower <| toString Widowed, translate language <| Translate.MaritalStatus Widowed )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.MaritalStatusLabel options maritalStatus "ten" "select-input" True

                                                viewHIVStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( String.toLower <| toString NA, translate language <| Translate.HIVStatus NA )
                                                                   , ( String.toLower <| toString Negative, translate language <| Translate.HIVStatus Negative )
                                                                   , ( String.toLower <| toString Positive, translate language <| Translate.HIVStatus Positive )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.HIVStatusLabel options hivStatus "ten" "select-input" True
                                            in
                                            [ viewGender
                                            , viewLevelOfEducation
                                            , viewProfession
                                            , viewMaritalStatus
                                            , viewHIVStatus
                                            ]
                                    in
                                    viewAge age :: motherInputs

                                Just (ChildParticipant age) ->
                                    let
                                        childInputs =
                                            let
                                                viewModeOfDelivery =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( modeOfDeliveryToValue SpontaneousVaginalDeliveryWithEpisiotomy, translate language <| Translate.ModeOfDelivery SpontaneousVaginalDeliveryWithEpisiotomy )
                                                                   , ( modeOfDeliveryToValue SpontaneousVaginalDeliveryWithoutEpisiotomy, translate language <| Translate.ModeOfDelivery SpontaneousVaginalDeliveryWithoutEpisiotomy )
                                                                   , ( modeOfDeliveryToValue VaginalDeliveryWithVacuumExtraction, translate language <| Translate.ModeOfDelivery VaginalDeliveryWithVacuumExtraction )
                                                                   , ( modeOfDeliveryToValue CesareanDelivery, translate language <| Translate.ModeOfDelivery CesareanDelivery )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.ModeOfDeliveryLabel options modeOfDelivery "ten" "select-input" True
                                            in
                                            [ viewGender
                                            , viewModeOfDelivery
                                            ]
                                    in
                                    viewAge age :: childInputs

                                Nothing ->
                                    []
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.PatientDemographicInformation ++ ":" ]
                    , viewPhoto language photo
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form" ] <|
                            staticComponents
                                ++ dynamicComponents
                    ]

                Second ->
                    let
                        viewFamilyUbudehe =
                            let
                                options =
                                    -- TODO: verify which values are required and translate.
                                    emptyOption
                                        :: [ ( toString Ubudehe1, toString Ubudehe1 )
                                           , ( toString Ubudehe2, toString Ubudehe2 )
                                           , ( toString Ubudehe3, toString Ubudehe3 )
                                           , ( toString Ubudehe4, toString Ubudehe4 )
                                           ]
                            in
                            viewSelectInput language Translate.FamilyUbudehe options familyUbudehe "ten" "select-input" True

                        familyInfoInputs =
                            viewFamilyUbudehe
                                :: (case maybeRegistratingParticipant of
                                        Just (MotherParticipant _) ->
                                            let
                                                viewHouseholdSize =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: (List.repeat 30 "."
                                                                        |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                                                   )
                                                    in
                                                    viewSelectInput language Translate.HouseholdSize options householdSize "four" "" False

                                                viewNumberOfChildren =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: (List.repeat 21 "."
                                                                        |> List.indexedMap (\index _ -> ( toString index, toString index ))
                                                                   )
                                                    in
                                                    viewSelectInput language Translate.NumberOfChildren options numberOfChildren "four" "" True
                                            in
                                            [ viewHouseholdSize, viewNumberOfChildren ]

                                        Just (ChildParticipant _) ->
                                            [ viewTextInput language Translate.MotherNameLabel motherName True
                                            , viewTextInput language Translate.MotherNationalId motherNationalId False
                                            , viewTextInput language Translate.FatherName fatherName True
                                            , viewTextInput language Translate.FatherNationalId fatherNationalId False
                                            , viewTextInput language Translate.CaregiverName caregiverName False
                                            , viewTextInput language Translate.CaregiverNationalId caregiverNationalId False
                                            ]

                                        -- We should never get here, as at second step
                                        -- registrating participant is already determined.
                                        Nothing ->
                                            []
                                   )

                        viewDistrict =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.District ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput district [] ]
                                ]

                        viewSector =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.Sector ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput sector [] ]
                                ]

                        viewCell =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.Cell ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput cell [] ]
                                ]

                        viewVillage =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.Village ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput village [] ]
                                ]

                        viewTelephoneNumber =
                            viewTextInput language Translate.TelephoneNumber telephoneNumber False
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.FamilyInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form family-info" ]
                            familyInfoInputs
                    , h3 [ class "ui header" ]
                        [ text <| translate language Translate.AddressInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form address-info" ]
                            [ viewDistrict
                            , viewSector
                            , viewCell
                            , viewVillage
                            ]
                    , h3 [ class "ui header" ]
                        [ text <| translate language Translate.ContactInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form address-info" ]
                            [ viewTelephoneNumber ]
                    ]

                Third ->
                    let
                        viewHealthCenterName =
                            div [ class "ui grid" ]
                                [ div [ class "six wide column" ]
                                    [ text <| translate language Translate.HealthCenterName ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput healthCenterName [] ]
                                ]
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.RegistratingHealthCenter ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form registrating-health-center" ]
                            [ viewHealthCenterName ]
                    ]

        leftButton =
            let
                action =
                    case step of
                        First ->
                            SetActivePage LoginPage

                        Second ->
                            SetRegistrationPhase (ParticipantRegistration First)

                        Third ->
                            SetRegistrationPhase (ParticipantRegistration Second)
            in
            button
                [ class "ui primary button"
                , onClick action
                ]
                [ text <| "< " ++ translate language Translate.Back ]

        rightButton =
            let
                maybeRegistratingParticipant =
                    getRegistratingParticipant currentDate
                        (getFormFieldValue dayOfBirth)
                        (getFormFieldValue monthOfBirth)
                        (getFormFieldValue yearOfBirth)

                ( label, action, disabled ) =
                    case step of
                        First ->
                            let
                                validatedFields =
                                    [ nationalIdNumber ]

                                requiredFields =
                                    [ firstName, secondName, dayOfBirth, monthOfBirth, yearOfBirth ]
                                        ++ (case maybeRegistratingParticipant of
                                                Just (MotherParticipant _) ->
                                                    [ levelOfEducation
                                                    , hivStatus
                                                    ]

                                                Just (ChildParticipant _) ->
                                                    [ modeOfDelivery ]

                                                Nothing ->
                                                    []
                                           )
                            in
                            ( Translate.Next
                            , SetRegistrationPhase (ParticipantRegistration Second)
                            , not (List.all isFormFieldSet requiredFields && List.all isFormFieldValid validatedFields)
                            )

                        Second ->
                            let
                                ( requiredFields, validatedFields ) =
                                    let
                                        requiredCommon =
                                            [ familyUbudehe, district, sector, cell, village ]
                                    in
                                    case maybeRegistratingParticipant of
                                        Just (MotherParticipant _) ->
                                            ( requiredCommon ++ [ numberOfChildren ]
                                            , []
                                            )

                                        Just (ChildParticipant _) ->
                                            ( requiredCommon ++ [ motherName, fatherName ]
                                            , [ motherNationalId, fatherNationalId, caregiverNationalId ]
                                            )

                                        Nothing ->
                                            ( requiredCommon, [] )
                            in
                            ( Translate.Next
                            , SetRegistrationPhase (ParticipantRegistration Third)
                            , not (List.all isFormFieldSet requiredFields && List.all isFormFieldValid validatedFields)
                            )

                        Third ->
                            ( Translate.Submit
                            , SetDialogState <| Just ConfirmSubmision
                            , False
                            )
            in
            button
                [ classList
                    [ ( "ui primary button", True )
                    , ( "disabled", disabled )
                    ]
                , onClick action
                ]
                [ text <| translate language label ++ " >" ]
    in
    div [ class "wrap-list" ]
        [ div [ class "ui form registration" ]
            formContent
        , div [ class "registration-form actions" ]
            [ leftButton, rightButton ]
        ]


viewSearchForm :
    Language
    -> NominalDate
    -> User
    -> ModelBackend
    -> ModelCached
    -> ParticipantsData
    -> String
    -> Html Msg
viewSearchForm language currentDate user backend cache participantsData searchString =
    div [ class "wrap-list" ]
        [ h3 [ class "ui header" ]
            [ text "Patient Information:" ]
        , span [] [ text "Search to see if patient already exists in E-Heza." ]
        , h3 [ class "ui header" ]
            [ text "Participant Directory:" ]
        , Html.form
            [ onSubmit <| SearchForParticipant searchString
            , action "javascript:void(0);"
            ]
            [ div
                [ class "ui search form" ]
                [ div []
                    [ input
                        [ placeholder "Enter patient name here"

                        -- , placeholder <| translateLogin Translate.Username
                        , type_ "text"

                        -- , name "username"
                        , onInput <| SetRegistrationPhase << ParticipantSearch
                        , value searchString
                        , autofocus True
                        ]
                        []

                    -- , i [ class "icon icon-username" ] []
                    ]
                ]
            , button
                [ class "ui fluid primary button"

                -- , disabled disableSubmitButton
                , type_ "submit"
                ]
                [ text "Search" ]

            -- [ span
            --     [ hidden <| not isLoading ]
            --     [ spinner ]
            -- , span
            --     [ hidden isLoading ]
            --     [ text <| translateLogin Translate.SignIn ]
            -- ]
            ]

        -- [ text <| translate language Translate.PatientDemographicInformation ++ ":" ]
        ]


viewDialog : Language -> Maybe DialogState -> Maybe (Html Msg)
viewDialog language dialogState =
    dialogState
        |> Maybe.andThen
            (\state ->
                case state of
                    ConfirmSubmision ->
                        Just <| confirmSubmisionDialog language

                    SuccessfulSubmision ->
                        Just <| successfulSubmisionDialog language
            )


confirmSubmisionDialog : Language -> Html Msg
confirmSubmisionDialog language =
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.ConfirmationRequired ]
        , div
            [ class "content" ]
            [ text <| translate language Translate.ConfirmRegisterPatient ]
        , div
            [ class "actions" ]
            [ div
                [ class "two ui buttons" ]
                [ button
                    [ class "ui  fluid button"
                    , onClick <| SetDialogState Nothing
                    ]
                    [ text <| translate language Translate.No ]
                , button
                    [ class "ui  primary fluid button"
                    , onClick Submit
                    ]
                    [ text <| translate language Translate.Yes ]
                ]
            ]
        ]


successfulSubmisionDialog : Language -> Html Msg
successfulSubmisionDialog language =
    div [ class "ui tiny active modal" ]
        [ div
            [ class "header" ]
            [ text <| translate language Translate.RegistartionSuccessful ]
        , div
            [ class "content" ]
            [ text <| translate language Translate.RegistartionSuccessfulAddNewPatient ]
        , div
            [ class "actions" ]
            [ div
                [ class "two ui buttons" ]
                [ button
                    [ class "ui  fluid button"
                    , onClick Reset
                    ]
                    [ text <| translate language Translate.No ]
                , button
                    [ class "ui  primary fluid button"
                    , onClick <| AddNewPatient Nothing
                    ]
                    [ text <| translate language Translate.Yes ]
                ]
            ]
        ]


viewTextInput : Language -> TranslationId -> Form.FieldState e String -> Bool -> Html Form.Msg
viewTextInput language labelId field isRequired =
    let
        formatValidationError =
            not isRequired && field.error == Just Form.Error.InvalidFormat
    in
    div [ class "ui grid" ]
        [ div
            [ classList
                [ ( "six wide column", True )
                , ( "required", isRequired )
                ]
            ]
            [ text <| translate language labelId ++ ":" ]
        , div [ class "ten wide column" ]
            [ Form.Input.textInput field [ classList [ ( "error", formatValidationError ) ] ] ]
        ]


viewSelectInput :
    Language
    -> TranslationId
    -> List ( String, String )
    -> Form.FieldState e String
    -> String
    -> String
    -> Bool
    -> Html Form.Msg
viewSelectInput language labelId options field width inputClass isRequired =
    div [ class "ui grid" ]
        [ div
            [ classList
                [ ( "six wide column", True )
                , ( "required", isRequired )
                ]
            ]
            [ text <| translate language labelId ++ ":" ]
        , div [ class <| width ++ " wide column" ]
            [ Form.Input.selectInput options field [ class inputClass ] ]
        ]


viewPhoto : Language -> Maybe PhotoValue -> Html Msg
viewPhoto language photo =
    div
        [ class "ui grid photo" ]
        [ Maybe.map viewPhotoThumb photo
            |> showMaybe
            |> List.singleton
            |> div [ class "eight wide column" ]
        , div
            [ id "dropzone"
            , class "eight wide column dropzone"
            , on "dropzonecomplete" (Json.Decode.map DropZoneComplete decodeDropZoneFile)
            ]
            [ div
                [ class "dz-message"
                , attribute "data-dz-message" ""
                ]
                [ span
                    []
                    [ text <| translate language Translate.DropzoneDefaultMessage ]
                ]
            ]

        -- This runs the function from our `app.js` at the precise moment this gets
        -- written to the DOM. Indeed very convenient.
        , script "bindDropZone()"
        ]


viewPhotoThumb : PhotoValue -> Html any
viewPhotoThumb photo =
    div []
        [ img
            [ src photo.url
            , class "ui small image"
            ]
            []
        ]
