module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
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

        viewAge =
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
                let
                    birthDate =
                        Time.Date.date birthYear birthMonth birthDay

                    delta =
                        Time.Date.delta currentDate birthDate

                    age =
                        if delta.years > 0 then
                            if delta.years == 1 then
                                translate language <| Translate.ChartPhrase Translate.OneYear

                            else
                                translate language <| Translate.ChartPhrase (Translate.XYears delta.years)

                        else if delta.months > 0 then
                            if delta.months == 1 then
                                translate language <| Translate.AgeSingleMonthWithoutDay 1

                            else
                                translate language <| Translate.AgeMonthsWithoutDay delta.month

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

            else
                emptyNode
    in
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
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
            [ class "ui full blue segment" ]
            [ div
                [ class "full content" ]
                [ div [ class "wrap-list" ]
                    [ h3
                        [ class "ui header" ]
                        [ text <| translate language Translate.PatientDemographicInformation ++ ":" ]
                    , div [ class "ui form registration" ]
                        [ viewPhoto language Nothing
                        , Html.map MsgRegistrationForm <|
                            fieldset [ class "registration-form" ]
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
                                , viewAge
                                ]
                        ]
                    ]
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
