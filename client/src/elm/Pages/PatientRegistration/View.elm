module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Child.Model exposing (Gender(..), ModeOfDelivery(..), modeOfDeliveryToValue)
import Backend.Measurement.Model exposing (PhotoValue)
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..), Ubudehe(..))
import Form
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
import Pages.PatientRegistration.Model exposing (DialogState(..), Model, Msg(..), RegistrationStep(..))
import Pages.PatientRegistration.Utils exposing (getFormFieldValue)
import Participant.Model exposing (ParticipantType(..))
import Time.Date
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import Utils.Form exposing (isFormFieldSet, isFormFieldValid)
import Utils.Html exposing (script, viewModal)


view : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
view language currentDate user backend cache model =
    let
        -- FORM FIELDS --
        firstName =
            Form.getFieldAsString "firstName" model.registrationForm

        secondName =
            Form.getFieldAsString "secondName" model.registrationForm

        nationalIdNumber =
            Form.getFieldAsString "nationalIdNumber" model.registrationForm

        dayOfBirth =
            Form.getFieldAsString "dayOfBirth" model.registrationForm

        monthOfBirth =
            Form.getFieldAsString "monthOfBirth" model.registrationForm

        yearOfBirth =
            Form.getFieldAsString "yearOfBirth" model.registrationForm

        isMale =
            Form.getFieldAsBool "isMale" model.registrationForm

        isFemale =
            Form.getFieldAsBool "isFemale" model.registrationForm

        levelOfEducation =
            Form.getFieldAsString "levelOfEducation" model.registrationForm

        profession =
            Form.getFieldAsString "profession" model.registrationForm

        maritalStatus =
            Form.getFieldAsString "maritalStatus" model.registrationForm

        hivStatus =
            Form.getFieldAsString "hivStatus" model.registrationForm

        modeOfDelivery =
            Form.getFieldAsString "modeOfDelivery" model.registrationForm

        -- END STEP 1 FIELDS --
        familyUbudehe =
            Form.getFieldAsString "familyUbudehe" model.registrationForm

        householdSize =
            Form.getFieldAsString "householdSize" model.registrationForm

        motherName =
            Form.getFieldAsString "motherName" model.registrationForm

        motherNationalId =
            Form.getFieldAsString "motherNationalId" model.registrationForm

        fatherName =
            Form.getFieldAsString "fatherName" model.registrationForm

        fatherNationalId =
            Form.getFieldAsString "fatherNationalId" model.registrationForm

        caregiverName =
            Form.getFieldAsString "caregiverName" model.registrationForm

        caregiverNationalId =
            Form.getFieldAsString "caregiverNationalId" model.registrationForm

        numberOfChildren =
            Form.getFieldAsString "numberOfChildren" model.registrationForm

        district =
            Form.getFieldAsString "district" model.registrationForm

        sector =
            Form.getFieldAsString "sector" model.registrationForm

        cell =
            Form.getFieldAsString "cell" model.registrationForm

        village =
            Form.getFieldAsString "village" model.registrationForm

        telephoneNumber =
            Form.getFieldAsString "telephoneNumber" model.registrationForm

        -- END STEP 2 FIELDS --
        healthCenterName =
            Form.getFieldAsString "healthCenterName" model.registrationForm

        -- END STEP 3 FIELDS --
        maybeAgeDateDelta =
            let
                birthDay =
                    getFormFieldValue dayOfBirth

                birthMonth =
                    getFormFieldValue monthOfBirth

                birthYear =
                    getFormFieldValue yearOfBirth
            in
            if birthDay > 0 && birthMonth > 0 && birthYear > 0 then
                Time.Date.delta currentDate (Time.Date.date birthYear birthMonth birthDay) |> Just

            else
                Nothing

        maybeRegistratingParticipant =
            maybeAgeDateDelta
                |> Maybe.andThen
                    (\delta ->
                        if delta.years > 12 then
                            Just <| MotherParticipant delta

                        else
                            Just <| ChildParticipant delta
                    )

        emptyOption =
            ( "", "" )

        formContent =
            case model.registrationStep of
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
                            , viewTextInput language Translate.SecondName secondName True
                            , viewTextInput language Translate.NationalIdNumber nationalIdNumber False
                            , div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.DateOfBirth ++ ":" ]
                                , div [ class "three wide column" ]
                                    [ Form.Input.selectInput monthOptions monthOfBirth [ class "select-month-input" ] ]
                                , div [ class "three wide column" ]
                                    [ Form.Input.selectInput dayOptions dayOfBirth [ class "select-day-input" ] ]
                                , div [ class "three wide column" ]
                                    [ Form.Input.selectInput yearOptions yearOfBirth [ class "select-year-input" ] ]
                                , div [ class "one wide column estimated" ]
                                    [ span [] [ text <| translate language Translate.Estimated ]
                                    , Form.Input.checkboxInput isFemale []
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
                                        [ div [ class "eight wide column" ]
                                            [ text <| translate language Translate.GenderLabel ++ ":" ]
                                        , Form.Input.checkboxInput isMale [ class "one wide column" ]
                                        , div [ class "three wide column" ]
                                            [ text <| translate language (Translate.Gender Male) ]
                                        , Form.Input.checkboxInput isFemale [ class "one wide column" ]
                                        , div [ class "two wide column" ]
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
                                                                :: [ ( toString NoSchooling, translate language <| Translate.LevelOfEducation NoSchooling )
                                                                   , ( toString PrimarySchool, translate language <| Translate.LevelOfEducation PrimarySchool )
                                                                   , ( toString VocationalTrainingSchool, translate language <| Translate.LevelOfEducation VocationalTrainingSchool )
                                                                   , ( toString SecondarySchool, translate language <| Translate.LevelOfEducation SecondarySchool )
                                                                   , ( toString DiplomaProgram, translate language <| Translate.LevelOfEducation DiplomaProgram )
                                                                   , ( toString HigherEducation, translate language <| Translate.LevelOfEducation HigherEducation )
                                                                   , ( toString AdvancedDiploma, translate language <| Translate.LevelOfEducation AdvancedDiploma )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.LevelOfEducationLabel options levelOfEducation "ten" "select-input" True

                                                viewProfession =
                                                    viewTextInput language Translate.Profession profession False

                                                viewMaritalStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( toString Divorced, translate language <| Translate.MaritalStatus Divorced )
                                                                   , ( toString Maried, translate language <| Translate.MaritalStatus Maried )
                                                                   , ( toString Single, translate language <| Translate.MaritalStatus Single )
                                                                   , ( toString Widowed, translate language <| Translate.MaritalStatus Widowed )
                                                                   ]
                                                    in
                                                    viewSelectInput language Translate.MaritalStatusLabel options maritalStatus "ten" "select-input" True

                                                viewHIVStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( toString NA, translate language <| Translate.HIVStatus NA )
                                                                   , ( toString Negative, translate language <| Translate.HIVStatus Negative )
                                                                   , ( toString Positive, translate language <| Translate.HIVStatus Positive )
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
                    , viewPhoto language model.photo
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
                    , div [ class "separator-line" ] []
                    , h3 [ class "ui header" ]
                        [ text <| translate language Translate.AddChild ++ ":" ]
                    , div [ class "ui grid" ]
                        [ div [ class "four wide column" ]
                            [ span [ class "icon-participant mother" ] [] ]
                        , div [ class "eight wide column add-child-label" ]
                            [ text <| translate language Translate.AddChildToFamily ]
                        , div [ class "four wide column" ]
                            [ div [ class "add-child-icon-wrapper" ] [ span [ class "add-child-icon" ] [] ] ]
                        ]
                    ]

        leftButton =
            let
                action =
                    case model.registrationStep of
                        First ->
                            SetActivePage LoginPage

                        Second ->
                            SetRegistrationStep First

                        Third ->
                            SetRegistrationStep Second
            in
            button
                [ class "ui primary button"
                , onClick action
                ]
                [ text <| "< " ++ translate language Translate.Back ]

        rightButton =
            let
                ( label, action, disabled ) =
                    case model.registrationStep of
                        First ->
                            let
                                validatedFields =
                                    [ nationalIdNumber ]

                                requiredFields =
                                    [ firstName, secondName, dayOfBirth, monthOfBirth, yearOfBirth ]
                                        ++ (case maybeRegistratingParticipant of
                                                Just (MotherParticipant _) ->
                                                    [ levelOfEducation, hivStatus ]

                                                Just (ChildParticipant _) ->
                                                    [ modeOfDelivery ]

                                                Nothing ->
                                                    []
                                           )
                            in
                            ( Translate.Next
                            , SetRegistrationStep Second
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
                            , SetRegistrationStep Third
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
    div [ class "wrap wrap-alt-2" ]
        [ div
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
        , div
            [ class "ui full segment blue" ]
            [ div [ class "content" ]
                [ div [ class "wrap-list" ]
                    [ div [ class "ui form registration" ]
                        formContent
                    ]
                ]
            , div [ class "registration-form actions" ]
                [ leftButton, rightButton ]
            ]
        , viewModal <| viewDialog language model.dialogState
        ]


viewDialog : Language -> Maybe DialogState -> Maybe (Html Msg)
viewDialog language dialogState =
    dialogState
        |> Maybe.andThen
            (\state ->
                case state of
                    ConfirmSubmision ->
                        Just <|
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
            )


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
