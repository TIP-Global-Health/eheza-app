module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Child.Model exposing (Gender(..))
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..), Ubudehe(..))
import Form
import Form.Input
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PatientRegistration.Model exposing (Model, Msg(..), RegistrationStep(..))
import Time.Date
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import Utils.Html exposing (script)


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

        -- END STEP 1 FIELDS --
        familyUbudehe =
            Form.getFieldAsString "familyUbudehe" model.registrationForm

        householdSize =
            Form.getFieldAsString "familyUbudehe" model.registrationForm

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
                getFieldValue field =
                    unwrap
                        0
                        (\value ->
                            case String.toInt value of
                                Ok value ->
                                    value

                                _ ->
                                    0
                        )
                        field.value

                birthDay =
                    getFieldValue dayOfBirth

                birthMonth =
                    getFieldValue monthOfBirth

                birthYear =
                    getFieldValue yearOfBirth
            in
            if birthDay > 0 && birthMonth > 0 && birthYear > 0 then
                Time.Date.delta currentDate (Time.Date.date birthYear birthMonth birthDay) |> Just

            else
                Nothing

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
                            [ div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.FirstName ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput firstName [] ]
                                ]
                            , div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.SecondName ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput secondName [] ]
                                ]
                            , div [ class "ui grid" ]
                                [ div [ class "six wide column" ]
                                    [ text <| translate language Translate.NationalIdNumber ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput nationalIdNumber [] ]
                                ]
                            , div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.DateOfBirth ++ ":" ]
                                , div [ class "three wide column" ]
                                    [ Form.Input.selectInput dayOptions dayOfBirth [ class "select-day-input" ] ]
                                , div [ class "three wide column" ]
                                    [ Form.Input.selectInput monthOptions monthOfBirth [ class "select-month-input" ] ]
                                , div [ class "four wide column" ]
                                    [ Form.Input.selectInput yearOptions yearOfBirth [ class "select-year-input" ] ]
                                ]
                            ]

                        dynamicComponents =
                            case maybeAgeDateDelta of
                                Just delta ->
                                    let
                                        viewAge =
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

                                        motherInputs =
                                            let
                                                viewGender =
                                                    div [ class "ui grid" ]
                                                        [ div [ class "six wide column" ]
                                                            [ text <| translate language Translate.GenderLabel ++ ":" ]
                                                        , Form.Input.checkboxInput isMale [ class "two wide column" ]
                                                        , div [ class "three wide column" ]
                                                            [ text <| translate language (Translate.Gender Male) ]
                                                        , Form.Input.checkboxInput isFemale [ class "two wide column" ]
                                                        , div [ class "two wide column" ]
                                                            [ text <| translate language (Translate.Gender Female) ]
                                                        ]

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
                                                    div [ class "ui grid" ]
                                                        [ div [ class "six wide column required" ]
                                                            [ text <| translate language Translate.LevelOfEducationLabel ]
                                                        , div [ class "ten wide column" ]
                                                            [ Form.Input.selectInput options levelOfEducation [ class "select-input" ] ]
                                                        ]

                                                viewProfession =
                                                    div [ class "ui grid" ]
                                                        [ div [ class "six wide column" ]
                                                            [ text <| translate language Translate.Profession ++ ":" ]
                                                        , div [ class "ten wide column" ]
                                                            [ Form.Input.textInput profession [] ]
                                                        ]

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
                                                    div [ class "ui grid" ]
                                                        [ div [ class "six wide column" ]
                                                            [ text <| translate language Translate.MaritalStatusLabel ++ ":" ]
                                                        , div [ class "ten wide column" ]
                                                            [ Form.Input.selectInput options maritalStatus [ class "select-input" ] ]
                                                        ]

                                                viewHIVStatus =
                                                    let
                                                        options =
                                                            emptyOption
                                                                :: [ ( toString NA, translate language <| Translate.HIVStatus NA )
                                                                   , ( toString Negative, translate language <| Translate.HIVStatus Negative )
                                                                   , ( toString Positive, translate language <| Translate.HIVStatus Positive )
                                                                   ]
                                                    in
                                                    div [ class "ui grid" ]
                                                        [ div [ class "six wide column required" ]
                                                            [ text <| translate language Translate.HIVStatusLabel ++ ":" ]
                                                        , div [ class "ten wide column" ]
                                                            [ Form.Input.selectInput options hivStatus [ class "select-input" ] ]
                                                        ]
                                            in
                                            [ viewGender
                                            , viewLevelOfEducation
                                            , viewProfession
                                            , viewMaritalStatus
                                            , viewHIVStatus
                                            ]

                                        childInputs =
                                            [ div [] [ text "Placeholder for child inputs" ] ]
                                    in
                                    viewAge
                                        :: (if delta.years > 12 then
                                                motherInputs

                                            else
                                                childInputs
                                           )

                                Nothing ->
                                    []
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.PatientDemographicInformation ++ ":" ]
                    , viewPhoto language Nothing
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
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.FamilyUbudehe ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.selectInput options familyUbudehe [ class "select-input" ] ]
                                ]

                        viewHouseholdSize =
                            let
                                options =
                                    emptyOption
                                        :: (List.repeat 30 "."
                                                |> List.indexedMap (\index _ -> ( toString <| index + 1, toString <| index + 1 ))
                                           )
                            in
                            div [ class "ui grid" ]
                                [ div [ class "six wide column" ]
                                    [ text <| translate language Translate.HouseholdSize ++ ":" ]
                                , div [ class "four wide column" ]
                                    [ Form.Input.selectInput options householdSize [] ]
                                ]

                        viewNumberOfChildren =
                            let
                                options =
                                    emptyOption
                                        :: (List.repeat 21 "."
                                                |> List.indexedMap (\index _ -> ( toString index, toString index ))
                                           )
                            in
                            div [ class "ui grid" ]
                                [ div [ class "six wide column required" ]
                                    [ text <| translate language Translate.NumberOfChildren ++ ":" ]
                                , div [ class "four wide column" ]
                                    [ Form.Input.selectInput options numberOfChildren [] ]
                                ]

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
                            div [ class "ui grid" ]
                                [ div [ class "six wide column" ]
                                    [ text <| translate language Translate.TelephoneNumber ++ ":" ]
                                , div [ class "ten wide column" ]
                                    [ Form.Input.textInput telephoneNumber [] ]
                                ]
                    in
                    [ h3 [ class "ui header" ]
                        [ text <| translate language Translate.FamilyInformation ++ ":" ]
                    , Html.map MsgRegistrationForm <|
                        fieldset [ class "registration-form family-info" ]
                            [ viewFamilyUbudehe
                            , viewHouseholdSize
                            , viewNumberOfChildren
                            ]
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
                isFieldSet field =
                    case field.value of
                        Nothing ->
                            False

                        Just "" ->
                            False

                        _ ->
                            True

                ( label, action, disabled ) =
                    case model.registrationStep of
                        First ->
                            ( Translate.Next
                            , SetRegistrationStep Second
                            , not <| List.all isFieldSet [ firstName, secondName, dayOfBirth, monthOfBirth, yearOfBirth, levelOfEducation, hivStatus ]
                            )

                        Second ->
                            ( Translate.Next
                            , SetRegistrationStep Third
                            , not <| List.all isFieldSet [ familyUbudehe, numberOfChildren, district, sector, cell, village ]
                            )

                        Third ->
                            ( Translate.Submit
                            , Submit
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
        ]


viewPhoto : Language -> Maybe String -> Html Msg
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

            -- , on "dropzonecomplete" (Json.Decode.map DropZoneComplete decodeDropZoneFile)
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


viewPhotoThumb : String -> Html any
viewPhotoThumb photo =
    div []
        [ img
            [ src photo
            , class "ui small image"
            ]
            []
        ]
