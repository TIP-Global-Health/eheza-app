module Pages.PinCode.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse, Role(..))
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PinCode.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner, viewLogo)


view : Language -> Page -> WebData ( NurseId, Nurse ) -> Model -> Html Msg
view language activePage nurseData model =
    div
        [ class "ui basic segment page-pincode" ]
    <|
        viewHeader language nurseData model
            :: viewContent language activePage nurseData model


viewHeader : Language -> WebData ( NurseId, Nurse ) -> Model -> Html Msg
viewHeader language nurseData model =
    case nurseData of
        Success _ ->
            case model.menu of
                ClinicalMenu ->
                    div [ class "ui basic head segment" ]
                        [ h1
                            [ class "ui header" ]
                            [ text <| translate language Translate.DeviceStatus ]
                        , a
                            [ class "link-back"
                            , onClick <| SetDisplayMenu MainMenu
                            ]
                            [ span [ class "icon-back" ] []
                            , span [] []
                            ]
                        ]

                MainMenu ->
                    viewLogo language

        _ ->
            viewLogo language


viewContent : Language -> Page -> WebData ( NurseId, Nurse ) -> Model -> List (Html Msg)
viewContent language activePage nurseData model =
    case nurseData of
        Success ( _, nurse ) ->
            viewWhenLoggedIn language nurse model

        _ ->
            let
                isLoading =
                    RemoteData.isLoading nurseData

                disableSubmitButton =
                    isLoading || model.code == ""

                activePageMsg =
                    -- Show a little message if the user wanted to view a different page,
                    -- but got sent here instead ...
                    showIf (activePage /= Pages.Page.PinCodePage) <|
                        p []
                            [ text <| translate language <| Translate.LoginPhrase Translate.YouMustLoginBefore
                            , text " "
                            , text <| translate language <| Translate.ActivePage activePage
                            , text " "
                            , text <| translate language Translate.Page
                            ]

                error =
                    if RemoteData.isFailure nurseData then
                        div
                            [ class "ui error message" ]
                            [ text <| translate language <| Translate.LoginPhrase Translate.PinCodeRejected ]

                    else
                        emptyNode
            in
            [ activePageMsg
            , Html.form
                [ onSubmit HandleLoginClicked
                , action "javascript:void(0);"
                ]
                [ div
                    [ class "ui login form" ]
                    [ div
                        [ class "ui transparent left icon input" ]
                        [ input
                            [ placeholder <| translate language <| Translate.LoginPhrase Translate.PinCode
                            , type_ "text"
                            , name "pincode"
                            , onInput SetPinCode
                            , value model.code
                            , autofocus True
                            ]
                            []
                        , i [ class "icon icon-password" ] []
                        ]
                    ]
                , button
                    [ class "ui primary button"
                    , disabled disableSubmitButton
                    , type_ "submit"
                    ]
                    [ span
                        [ hidden <| not isLoading ]
                        [ spinner ]
                    , span
                        [ hidden isLoading ]
                        [ text <| translate language <| Translate.LoginPhrase Translate.SignIn ]
                    ]
                ]
            , error
            , p []
                [ text <| translate language <| Translate.LoginPhrase Translate.ForgotPassword1
                , br [] []
                , text <| translate language <| Translate.LoginPhrase Translate.ForgotPassword2
                ]
            ]


viewWhenLoggedIn : Language -> Nurse -> Model -> List (Html Msg)
viewWhenLoggedIn language nurse model =
    case model.menu of
        ClinicalMenu ->
            let
                groupAssessmentButton =
                    button
                        [ class "ui primary button"
                        , onClick <| SendOutMsg <| SetActivePage <| UserPage <| ClinicsPage Nothing
                        ]
                        [ text <| translate language Translate.GroupAssessment ]
            in
            [ p [] [ text <| translate language Translate.WhatDoYouWantToDo ]
            , groupAssessmentButton
            ]

        MainMenu ->
            let
                loggedInAs =
                    p []
                        [ Translate.LoginPhrase Translate.LoggedInAs
                            |> translate language
                            |> text
                        , text <| ": " ++ nurse.name
                        ]

                deviceStatusButton =
                    button
                        [ class "ui primary button"
                        , Pages.Page.DevicePage
                            |> SetActivePage
                            |> SendOutMsg
                            |> onClick
                        ]
                        [ Translate.ActivePage DevicePage
                            |> translate language
                            |> text
                        ]

                clinicalButton =
                    button
                        [ class "ui primary button"
                        , onClick <| SetDisplayMenu ClinicalMenu
                        ]
                        [ text <| translate language Translate.Clinical ]

                registerParticipantButton =
                    button
                        [ class "ui primary button"
                        , onClick <| SendOutMsg <| SetActivePage <| UserPage <| PersonsPage Nothing
                        ]
                        [ text <| translate language Translate.RegisterAParticipant ]

                logoutButton =
                    button
                        [ class "ui button logout"
                        , onClick HandleLogoutClicked
                        ]
                        [ Translate.LoginPhrase Translate.SignOut
                            |> translate language
                            |> text
                        ]
            in
            [ loggedInAs
            , clinicalButton
            , registerParticipantButton
            , deviceStatusButton
            , logoutButton
            ]
