module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Form
import Form.Input
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PatientRegistration.Model exposing (Model, Msg(..))
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
                                        [ Form.Input.textInput secondName [ class "ten wide column" ] ]
                                    ]
                                , div [ class "ui grid" ]
                                    [ div [ class "six wide column" ]
                                        [ text <| translate language Translate.NationalIdNumber ++ ":" ]
                                    , div [ class "ten wide column" ]
                                        [ Form.Input.textInput nationalIdNumber [ class "ten wide column" ] ]
                                    ]
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
