module Pages.PinCode.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Nurse.Model exposing (Nurse, Role(..))
import Backend.Nurse.Utils exposing (assignedToHealthCenter, assignedToVillage, isCommunityHealthWorker)
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


view : Language -> Page -> WebData ( NurseId, Nurse ) -> ( Maybe HealthCenterId, Maybe VillageId ) -> Model -> ModelIndexedDb -> Html Msg
view language activePage nurseData ( healthCenterId, villageId ) model db =
    div
        [ class "ui basic segment page-pincode" ]
    <|
        viewHeader language nurseData healthCenterId model
            :: viewContent language activePage nurseData ( healthCenterId, villageId ) model db


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


viewContent : Language -> Page -> WebData ( NurseId, Nurse ) -> ( Maybe HealthCenterId, Maybe VillageId ) -> Model -> ModelIndexedDb -> List (Html Msg)
viewContent language activePage nurseData ( healthCenterId, villageId ) model db =
    case nurseData of
        Success ( _, nurse ) ->
            viewWhenLoggedIn language nurse ( healthCenterId, villageId ) model db

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


viewWhenLoggedIn : Language -> Nurse -> ( Maybe HealthCenterId, Maybe VillageId ) -> Model -> ModelIndexedDb -> List (Html Msg)
viewWhenLoggedIn language nurse ( healthCenterId, villageId ) model db =
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

            registerParticipantButton =
                if isCommunityHealthWorker nurse then
                    emptyNode

                else
                    button
                        [ class "ui primary button"
                        , onClick <| SendOutMsg <| SetActivePage <| UserPage <| PersonsPage Nothing
                        ]
                        [ text <| translate language Translate.ParticipantDirectory ]
        in
        [ loggedInAs
        , viewLocationName nurse ( healthCenterId, villageId ) db
        , clinicalButton
        , registerParticipantButton
        , deviceStatusButton
        , logoutButton
        ]

    else
        let
            locationOptions =
                if isCommunityHealthWorker nurse then
                    selectVillageOptions language nurse db

                else
                    selectHeathCenterOptions language nurse db
        in
        locationOptions ++ [ logoutButton ]


viewLocationName : Nurse -> ( Maybe HealthCenterId, Maybe VillageId ) -> ModelIndexedDb -> Html Msg
viewLocationName nurse ( healthCenterId, villageId ) db =
    let
        locationName =
            if isCommunityHealthWorker nurse then
                villageId
                    |> Maybe.andThen
                        (\id ->
                            RemoteData.toMaybe db.villages
                                |> Maybe.andThen (Dict.get id)
                        )
                    |> Maybe.map .name

            else
                healthCenterId
                    |> Maybe.andThen
                        (\id ->
                            RemoteData.toMaybe db.healthCenters
                                |> Maybe.andThen (Dict.get id)
                        )
                    |> Maybe.map .name
    in
    locationName
        |> Maybe.map
            (\name ->
                p
                    [ class "location-name" ]
                    [ text name ]
            )
        |> Maybe.withDefault emptyNode


selectHeathCenterOptions : Language -> Nurse -> ModelIndexedDb -> List (Html Msg)
selectHeathCenterOptions language nurse db =
    let
        filtered =
            db.healthCenters
                |> RemoteData.map (Dict.filter (\uuid _ -> assignedToHealthCenter uuid nurse) >> Dict.toList)
                |> RemoteData.withDefault []

        selectButton ( id, location ) =
            button
                [ class "ui primary button"
                , onClick <| SendOutMsg <| SetHealthCenter id
                ]
                [ text location.name ]
    in
    p [ class "select-location" ] [ text <| (translate language Translate.SelectYourHealthCenter ++ ":") ]
        :: List.map selectButton filtered


selectVillageOptions : Language -> Nurse -> ModelIndexedDb -> List (Html Msg)
selectVillageOptions language nurse db =
    let
        filtered =
            db.villages
                |> RemoteData.map (Dict.filter (\uuid _ -> assignedToVillage uuid nurse) >> Dict.toList)
                |> RemoteData.withDefault []

        selectButton ( id, location ) =
            button
                [ class "ui primary button"
                , onClick <| SendOutMsg <| SetVillage id
                ]
                [ text location.name ]
    in
    p [ class "select-location" ] [ text <| (translate language Translate.SelectYourVillage ++ ":") ]
        :: List.map selectButton filtered
