module Pages.PinCode.View exposing (view)

import AssocList as Dict
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Nurse.Model exposing (Nurse, Role(..))
import Backend.Nurse.Utils exposing (assignedToHealthCenter, assignedToVillage, isCommunityHealthWorker)
import Backend.Person.Model exposing (Initiator(..))
import Backend.Person.Utils exposing (getHealthCenterName)
import EverySet
import Gizra.Html exposing (emptyNode, showIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Pages.Page exposing (DashboardPage(..), NurseDashboardPage(..), Page(..), UserPage(..))
import Pages.PinCode.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, spinner, viewLogo)


view : Language -> Page -> WebData ( NurseId, Nurse ) -> ( Maybe HealthCenterId, Maybe VillageId ) -> Maybe String -> Model -> ModelIndexedDb -> Html Msg
view language activePage nurseData ( healthCenterId, villageId ) deviceName model db =
    let
        ( header, content ) =
            case nurseData of
                Success ( _, nurse ) ->
                    let
                        isChw =
                            isCommunityHealthWorker nurse

                        selectedAuthorizedLocation =
                            if isChw then
                                villageId
                                    |> Maybe.map (\id -> EverySet.member id nurse.villages)
                                    |> Maybe.withDefault False

                            else
                                healthCenterId
                                    |> Maybe.map (\id -> EverySet.member id nurse.healthCenters)
                                    |> Maybe.withDefault False
                    in
                    ( viewLoggedInHeader language nurse selectedAuthorizedLocation
                    , viewLoggedInContent language nurse ( healthCenterId, villageId ) isChw deviceName selectedAuthorizedLocation db
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


viewLoggedInContent : Language -> Nurse -> ( Maybe HealthCenterId, Maybe VillageId ) -> Bool -> Maybe String -> Bool -> ModelIndexedDb -> List (Html Msg)
viewLoggedInContent language nurse ( healthCenterId, villageId ) isChw deviceName selectedAuthorizedLocation db =
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

            generalInfo =
                div [ class "general-info" ]
                    [ deviceInfo
                    , loggedInAs
                    , viewLocationName nurse ( healthCenterId, villageId ) db
                    ]

            viewCard activity =
                let
                    ( activityIcon, navigationPage ) =
                        case activity of
                            MenuClinical ->
                                ( "clinical"
                                , UserPage ClinicalPage
                                )

                            MenuParticipantDirectory ->
                                ( "participant-directory"
                                , UserPage <| PersonsPage Nothing ParticipantDirectoryOrigin
                                )

                            MenuDashboards ->
                                ( "dashboards"
                                , UserPage <| DashboardPage MainPage
                                )

                            MenuCaseManagement ->
                                ( "case-management"
                                , UserPage GlobalCaseManagementPage
                                )

                            MenuDeviceStatus ->
                                ( "device-status"
                                , DevicePage
                                )

                            MenuMessagingCenter ->
                                ( "messaging-center"
                                , --@todo
                                  DevicePage
                                )
                in
                activityCard language
                    (Translate.MainMenuActivity activity)
                    activityIcon
                    (SendOutMsg <| SetActivePage navigationPage)

            cards =
                div [ class "wrap-cards" ]
                    [ div [ class "ui four cards" ] <|
                        List.map viewCard
                            [ MenuClinical
                            , MenuParticipantDirectory
                            , MenuDashboards
                            , MenuCaseManagement
                            , MenuDeviceStatus
                            , MenuMessagingCenter
                            ]
                    ]
        in
        [ generalInfo, cards, logoutButton ]

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
                getHealthCenterName healthCenterId db
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
                |> RemoteData.map
                    (Dict.filter (\uuid _ -> assignedToHealthCenter uuid nurse)
                        >> Dict.toList
                        >> List.sortBy (Tuple.second >> .name)
                    )
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
                |> RemoteData.map
                    (Dict.filter (\uuid _ -> assignedToVillage uuid nurse)
                        >> Dict.toList
                        >> List.sortBy (Tuple.second >> .name)
                    )
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
