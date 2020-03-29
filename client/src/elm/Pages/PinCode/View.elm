module Pages.PinCode.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Nurse.Model exposing (Nurse, Role(..))
import EverySet
import Gizra.Html exposing (emptyNode, showIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))
import Pages.PinCode.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.Html exposing (spinner, viewLogo)


view : Language -> Page -> WebData ( NurseId, Nurse ) -> Maybe HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language activePage nurseData healthCenterId model db =
    div
        [ class "ui basic segment page-pincode" ]
    <|
        viewHeader language nurseData healthCenterId model
            :: viewContent language activePage nurseData healthCenterId model db


viewHeader : Language -> WebData ( NurseId, Nurse ) -> Maybe HealthCenterId -> Model -> Html Msg
viewHeader language nurseData healthCenterId model =
    case nurseData of
        Success ( _, nurse ) ->
            let
                selectedAuthorizedHealthCenter =
                    healthCenterId
                        |> Maybe.map (\id -> EverySet.member id nurse.healthCenters)
                        |> Maybe.withDefault False
            in
            if selectedAuthorizedHealthCenter then
                viewLogo language

            else
                div [ class "ui basic head segment" ]
                    [ h1
                        [ class "ui header" ]
                        [ text <| translate language Translate.HealthCenter ]
                    ]

        _ ->
            viewLogo language


viewContent : Language -> Page -> WebData ( NurseId, Nurse ) -> Maybe HealthCenterId -> Model -> ModelIndexedDb -> List (Html Msg)
viewContent language activePage nurseData healthCenterId model db =
    case nurseData of
        Success ( _, nurse ) ->
            viewWhenLoggedIn language nurse healthCenterId model db

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
                            , type_ "password"
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


viewWhenLoggedIn : Language -> Nurse -> Maybe HealthCenterId -> Model -> ModelIndexedDb -> List (Html Msg)
viewWhenLoggedIn language nurse healthCenterId model db =
    let
        logoutButton =
            button
                [ class "ui button logout"
                , onClick HandleLogoutClicked
                ]
                [ Translate.LoginPhrase Translate.SignOut
                    |> translate language
                    |> text
                ]

        selectedAuthorizedHealthCenter =
            healthCenterId
                |> Maybe.map (\id -> EverySet.member id nurse.healthCenters)
                |> Maybe.withDefault False
    in
    if selectedAuthorizedHealthCenter then
        let
            loggedInAs =
                p [ class "logged-in-as" ]
                    [ Translate.LoginPhrase Translate.LoggedInAs
                        |> translate language
                        |> text
                    , text <| ": " ++ nurse.name
                    ]

            healthCenterName =
                healthCenterId
                    |> Maybe.andThen
                        (\id ->
                            RemoteData.toMaybe db.healthCenters
                                |> Maybe.andThen (Dict.get id)
                        )
                    |> Maybe.map
                        (\healthCenter ->
                            p
                                [ class "health-center-name" ]
                                [ text healthCenter.name ]
                        )
                    |> Maybe.withDefault emptyNode

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
                    , onClick <| SendOutMsg <| SetActivePage <| UserPage ClinicalPage
                    ]
                    [ text <| translate language Translate.Clinical ]

            dashboardButton =
                healthCenterId
                    |> Maybe.andThen
                        (\id ->
                            RemoteData.toMaybe db.healthCenters
                                |> Maybe.andThen (Dict.get id)
                        )
                    |> Maybe.map
                        (\healthCenter ->
                            if healthCenter.fbfClinics then
                                button
                                    [ class "ui primary button"
                                    , onClick <| SendOutMsg <| SetActivePage <| UserPage <| DashboardPage MainPage
                                    ]
                                    [ text <| translate language Translate.DashboardLabel
                                    ]

                            else
                                emptyNode
                        )
                    |> Maybe.withDefault emptyNode

            registerParticipantButton =
                button
                    [ class "ui primary button"
                    , onClick <| SendOutMsg <| SetActivePage <| UserPage <| PersonsPage Nothing
                    ]
                    [ text <| translate language Translate.ParticipantDirectory ]
        in
        [ loggedInAs
        , healthCenterName
        , clinicalButton
        , registerParticipantButton
        , dashboardButton
        , deviceStatusButton
        , logoutButton
        ]

    else
        let
            filteredHealthCenters =
                case db.healthCenters of
                    Success healthCenters ->
                        healthCenters
                            |> Dict.filter (\uuid _ -> EverySet.member uuid nurse.healthCenters)
                            |> Dict.toList
                            -- Reverse now because we are reversing again when adding the "logout" button.
                            |> List.reverse

                    _ ->
                        []

            selectHealthCenterButton ( healthCenterId_, healthCenter ) =
                button
                    [ class "ui primary button health-center"
                    , onClick <| SendOutMsg <| SetHealthCenter healthCenterId_
                    ]
                    [ text healthCenter.name ]
        in
        p [ class "select-health-center" ] [ text <| (translate language Translate.SelectYourHealthCenter ++ ":") ]
            :: (filteredHealthCenters
                    |> List.map selectHealthCenterButton
                    |> List.append [ logoutButton ]
                    |> List.reverse
               )
