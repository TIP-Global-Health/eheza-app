module Pages.PinCode.View exposing (view)

import AssocList as Dict
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Nurse.Model exposing (Nurse, Role(..))
import Backend.Nurse.Utils exposing (assignedToHealthCenter, assignedToVillage, isCommunityHealthWorker)
import Backend.Person.Model exposing (Initiator(..))
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


view : Language -> Page -> WebData ( NurseId, Nurse ) -> ( Maybe HealthCenterId, Maybe VillageId ) -> Maybe String -> Model -> ModelIndexedDb -> Html Msg
view language activePage nurseData ( healthCenterId, villageId ) deviceName model db =
    let
        ( header, content ) =
            case nurseData of
                Success ( _, nurse ) ->
                    let
                        selectedAuthorizedLocation =
                            if isCommunityHealthWorker nurse then
                                villageId
                                    |> Maybe.map (\id -> EverySet.member id nurse.villages)
                                    |> Maybe.withDefault False

                            else
                                healthCenterId
                                    |> Maybe.map (\id -> EverySet.member id nurse.healthCenters)
                                    |> Maybe.withDefault False
                    in
                    ( viewLoggedInHeader language nurse selectedAuthorizedLocation
                    , viewLoggedInContent language nurse ( healthCenterId, villageId ) deviceName selectedAuthorizedLocation db
                    )

                _ ->
                    ( viewLogo language
                    , viewAnonymousContent language activePage nurseData model
                    )
    in
    (header :: content)
        |> div [ class "ui basic segment page-pincode" ]


viewLoggedInHeader : Language -> Nurse -> Bool -> Html Msg
viewLoggedInHeader language nurse selectedAuthorizedLocation =
    if selectedAuthorizedLocation then
        viewLogo language

    else
        let
            label =
                if isCommunityHealthWorker nurse then
                    Translate.Village

                else
                    Translate.HealthCenter
        in
        div [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language label ]
            ]


viewAnonymousContent : Language -> Page -> WebData ( NurseId, Nurse ) -> Model -> List (Html Msg)
viewAnonymousContent language activePage nurseData model =
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


viewLoggedInContent : Language -> Nurse -> ( Maybe HealthCenterId, Maybe VillageId ) -> Maybe String -> Bool -> ModelIndexedDb -> List (Html Msg)
viewLoggedInContent language nurse ( healthCenterId, villageId ) deviceName selectedAuthorizedLocation db =
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
    in
    if selectedAuthorizedLocation then
        let
            deviceInfo =
                deviceName
                    |> Maybe.map
                        (\name ->
                            p [ class "device-info" ]
                                [ text <| translate language Translate.Device
                                , text <| ": " ++ name
                                ]
                        )
                    |> Maybe.withDefault emptyNode

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

            generalInfo =
                div [ class "general-info" ]
                    [ deviceInfo
                    , loggedInAs
                    , viewLocationName nurse ( healthCenterId, villageId ) db
                    ]

            clinicalButton =
                button
                    [ class "ui primary button"
                    , onClick <| SendOutMsg <| SetActivePage <| UserPage ClinicalPage
                    ]
                    [ text <| translate language Translate.Clinical ]

            healthCenterGotFbfClinic =
                healthCenterId
                    |> Maybe.andThen
                        (\id ->
                            db.clinics
                                |> RemoteData.toMaybe
                                |> Maybe.map
                                    (Dict.values
                                        >> List.filter (\clinic -> clinic.healthCenterId == id && clinic.clinicType == Fbf)
                                        >> List.isEmpty
                                        >> not
                                    )
                        )
                    |> Maybe.withDefault False

            dashboardButton =
                if not (isCommunityHealthWorker nurse) && healthCenterGotFbfClinic then
                    button
                        [ class "ui primary button"
                        , onClick <| SendOutMsg <| SetActivePage <| UserPage <| DashboardPage MainPage
                        ]
                        [ text <| translate language Translate.DashboardLabel
                        ]

                else
                    emptyNode

            participantDirectoryButton =
                button
                    [ class "ui primary button"
                    , onClick <| SendOutMsg <| SetActivePage <| UserPage <| PersonsPage Nothing ParticipantDirectoryOrigin
                    ]
                    [ text <| translate language Translate.ParticipantDirectory ]
        in
        [ generalInfo
        , clinicalButton
        , participantDirectoryButton
        , dashboardButton
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
