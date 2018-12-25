module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Child.Model exposing (Gender(..))
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Mother.Model exposing (EducationLevel(..), HIVStatus(..), MaritalStatus(..))
import Form
import Form.Input
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PatientRegistration.Model exposing (Model, Msg(..))
import Time.Date
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)
import Utils.Html exposing (script)


view : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Model -> Html Msg
view language currentDate user backend cache model =
    let
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

        maybeAgeDateDelta =
            if birthDay > 0 && birthMonth > 0 && birthYear > 0 then
                Time.Date.delta currentDate (Time.Date.date birthYear birthMonth birthDay) |> Just

            else
                Nothing

        staticComponents =
            let
                emptyOption =
                    ( "", "" )

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
                [ div [ class "six wide column" ]
                    [ text <| translate language Translate.FirstName ++ ":" ]
                , div [ class "ten wide column" ]
                    [ Form.Input.textInput firstName [] ]
                ]
            , div [ class "ui grid" ]
                [ div [ class "six wide column" ]
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
                [ div [ class "six wide column" ]
                    [ text <| translate language Translate.DateOfBirth ++ ":" ]
                , div [ class "three wide column" ]
                    [ Form.Input.selectInput dayOptions dayOfBirth [] ]
                , div [ class "three wide column" ]
                    [ Form.Input.selectInput monthOptions monthOfBirth [] ]
                , div [ class "four wide column" ]
                    [ Form.Input.selectInput yearOptions yearOfBirth [] ]
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
                                            [ text <| translate language Translate.Sex ++ ":" ]
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
                                            [ ( toString NoSchooling, translate language <| Translate.LevelOfEducation NoSchooling )
                                            , ( toString PrimarySchool, translate language <| Translate.LevelOfEducation PrimarySchool )
                                            , ( toString VocationalTrainingSchool, translate language <| Translate.LevelOfEducation VocationalTrainingSchool )
                                            , ( toString SecondarySchool, translate language <| Translate.LevelOfEducation SecondarySchool )
                                            , ( toString DiplomaProgram, translate language <| Translate.LevelOfEducation DiplomaProgram )
                                            , ( toString HigherEducation, translate language <| Translate.LevelOfEducation HigherEducation )
                                            , ( toString AdvancedDiploma, translate language <| Translate.LevelOfEducation AdvancedDiploma )
                                            ]
                                    in
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
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
                                            [ ( toString Divorced, translate language <| Translate.MaritalStatus Divorced )
                                            , ( toString Maried, translate language <| Translate.MaritalStatus Maried )
                                            , ( toString Single, translate language <| Translate.MaritalStatus Single )
                                            , ( toString Widowed, translate language <| Translate.MaritalStatus Widowed )
                                            ]
                                    in
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
                                            [ text <| translate language Translate.MaritalStatusLabel ]
                                        , div [ class "ten wide column" ]
                                            [ Form.Input.selectInput options maritalStatus [ class "select-input" ] ]
                                        ]

                                viewHIVStatus =
                                    let
                                        options =
                                            [ ( toString NA, translate language <| Translate.HIVStatus NA )
                                            , ( toString Negative, translate language <| Translate.HIVStatus Negative )
                                            , ( toString Positive, translate language <| Translate.HIVStatus Positive )
                                            ]
                                    in
                                    div [ class "ui grid" ]
                                        [ div [ class "six wide column" ]
                                            [ text <| translate language Translate.HIVStatusLabel ]
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
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic segment head" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.RegisterANewPatient ]
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
                    [ h3
                        [ class "ui header" ]
                        [ text <| translate language Translate.PatientDemographicInformation ++ ":" ]
                    , div [ class "ui form registration" ]
                        [ viewPhoto language Nothing
                        , Html.map MsgRegistrationForm <|
                            fieldset [ class "registration-form" ] <|
                                staticComponents
                                    ++ dynamicComponents
                        ]
                    ]
                ]
            , div [ class "registration-form actions" ]
                [ button
                    [ class "ui primary button"
                    , onClick <| SetActivePage LoginPage
                    ]
                    [ text "< Back" ]
                , button
                    [ class "ui primary button disabled"

                    -- , onClick <| SendOutMsg <| SetActivePage <| Pages.Page.UserPage <| Pages.Page.ClinicsPage Nothing
                    ]
                    [ text "Next >" ]
                ]
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
