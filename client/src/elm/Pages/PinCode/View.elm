module Pages.PinCode.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (Nurse, Role(..))
import EverySet
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
    div [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic login segment" ]
            (viewLogo language :: viewContent language activePage nurseData model)
        ]


viewContent : Language -> Page -> WebData ( NurseId, Nurse ) -> Model -> List (Html Msg)
viewContent language activePage nurseData model =
    case nurseData of
        Success ( _, nurse ) ->
            viewWhenLoggedIn language nurse

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


viewWhenLoggedIn : Language -> Nurse -> List (Html Msg)
viewWhenLoggedIn language nurse =
    let
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

        selectClinicButton =
            button
                [ class "ui primary button"
                , onClick <| SendOutMsg <| SetActivePage <| UserPage <| ClinicsPage Nothing
                ]
                [ text <| translate language Translate.SelectYourClinic ]

        administrationButton =
            if EverySet.member RoleAdministrator nurse.roles then
                Just <|
                    button
                        [ class "ui primary button"
                        , onClick <| SendOutMsg <| SetActivePage <| UserPage AdminPage
                        ]
                        [ text <| translate language Translate.Admin ]

            else
                Nothing

        registerParticipantButton =
            button
                [ class "ui fluid primary button"
                , onClick <| SendOutMsg <| SetActivePage <| UserPage <| PersonsPage Nothing
                ]
                [ text <| translate language Translate.RegisterAParticipant ]

        logoutButton =
            button
                [ class "ui button logout"
                , onClick HandleLogoutClicked
                ]
                [ Translate.LoginPhrase Translate.Logout
                    |> translate language
                    |> text
                ]

        loggedInAs =
            p []
                [ Translate.LoginPhrase Translate.LoggedInAs
                    |> translate language
                    |> text
                , text <| ": " ++ nurse.name
                ]
    in
    [ Just loggedInAs
    , Just deviceStatusButton
    , Just selectClinicButton
    , administrationButton
    , Just registerParticipantButton
    , Just logoutButton
    ]
        |> List.filterMap identity
