module Pages.PinCode.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (assignedToHealthCenter, assignedToVillage, isCommunityHealthWorker, isLabTechnician)
import Backend.Person.Model exposing (Initiator(..))
import Backend.Person.Utils exposing (getHealthCenterName)
import Backend.ResilienceMessage.Model exposing (ResilienceCategory(..), ResilienceMessage, ResilienceMessageOrder(..))
import Backend.Utils exposing (groupEncountersEnabled, individualEncountersEnabled, stockManagementEnabled)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Maybe.Extra exposing (isJust)
import Pages.MessagingCenter.Utils exposing (resolveNumberOfUnreadMessages)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))
import Pages.PinCode.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import SyncManager.Model exposing (SiteFeature)
import Time exposing (posixToMillis)
import Time.Extra
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, activityCardWithCounter, spinner, viewLogo, viewModal)


view :
    Language
    -> Time.Posix
    -> EverySet SiteFeature
    -> Page
    -> WebData ( NurseId, Nurse )
    -> ( Maybe HealthCenterId, Maybe VillageId )
    -> Maybe String
    -> Model
    -> ModelIndexedDb
    -> Html Msg
view language currentTime features activePage nurseData ( healthCenterId, villageId ) deviceName model db =
    let
        ( header, content ) =
            case nurseData of
                Success ( nurseId, nurse ) ->
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
                    ( viewLoggedInHeader language isChw selectedAuthorizedLocation
                    , viewLoggedInContent language
                        currentTime
                        features
                        nurseId
                        nurse
                        ( healthCenterId, villageId )
                        isChw
                        deviceName
                        selectedAuthorizedLocation
                        db
                        model
                    )

                _ ->
                    ( viewLogo language
                    , viewAnonymousContent language activePage nurseData model
                    )
    in
    (header :: content)
        |> div [ class "ui basic segment page-pincode" ]


viewLoggedInHeader : Language -> Bool -> Bool -> Html Msg
viewLoggedInHeader language isChw selectedAuthorizedLocation =
    if selectedAuthorizedLocation then
        viewLogo language

    else
        let
            label =
                if isChw then
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


viewLoggedInContent :
    Language
    -> Time.Posix
    -> EverySet SiteFeature
    -> NurseId
    -> Nurse
    -> ( Maybe HealthCenterId, Maybe VillageId )
    -> Bool
    -> Maybe String
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> List (Html Msg)
viewLoggedInContent language currentTime features nurseId nurse ( healthCenterId, villageId ) isChw deviceName selectedAuthorizedLocation db model =
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
            currentDate =
                fromLocalDateTime currentTime

            isLabTech =
                isLabTechnician nurse

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
                    , viewLocationName isChw ( healthCenterId, villageId ) db
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
                                , UserPage <| DashboardPage PageMain
                                )

                            MenuCaseManagement ->
                                ( "case-management"
                                , UserPage GlobalCaseManagementPage
                                )

                            MenuDeviceStatus ->
                                ( "device-status"
                                , DevicePage
                                )

                            MenuWellbeing ->
                                ( "messaging-center"
                                , UserPage WellbeingPage
                                )

                            MenuStockManagement ->
                                ( "stock-management"
                                , UserPage StockManagementPage
                                )

                    viewCardFunc =
                        if activity == MenuWellbeing then
                            resolveNumberOfUnreadMessages currentTime currentDate nurse
                                |> activityCardWithCounter

                        else
                            activityCard
                in
                viewCardFunc language
                    (Translate.MainMenuActivity activity)
                    activityIcon
                    (SendOutMsg <| SetActivePage navigationPage)

            activities =
                if isLabTech then
                    [ MenuCaseManagement
                    , MenuDeviceStatus
                    ]

                else if
                    individualEncountersEnabled isChw features
                        || groupEncountersEnabled isChw features
                then
                    [ MenuClinical
                    , MenuParticipantDirectory
                    , MenuDashboards
                    , MenuCaseManagement
                    , MenuDeviceStatus
                    ]

                else
                    []
                        ++ (if nurse.resilienceProgramEnabled then
                                [ MenuWellbeing ]

                            else
                                []
                           )
                        ++ (if
                                stockManagementEnabled features
                                    && not isChw
                            then
                                [ MenuStockManagement ]

                            else
                                []
                           )

            cards =
                div [ class "wrap-cards" ]
                    [ div [ class "ui four cards" ] <|
                        List.map viewCard activities
                    ]
        in
        [ generalInfo
        , cards
        , logoutButton
        , viewModal <|
            -- If both reminder and notification need to be presented,
            -- reminder is given a priority.
            Maybe.Extra.or
                (resilienceReminderDialog language currentTime currentDate nurseId nurse)
                (resilienceNotificationDialog language currentTime currentDate nurseId nurse db model)
        ]

    else
        let
            locationOptions =
                if isChw then
                    selectVillageOptions language nurse db

                else
                    selectHeathCenterOptions language nurse db
        in
        locationOptions ++ [ logoutButton ]


resilienceReminderDialog : Language -> Time.Posix -> NominalDate -> NurseId -> Nurse -> Maybe (Html Msg)
resilienceReminderDialog language currentTime currentDate nurseId nurse =
    Maybe.andThen
        (\programStartDate ->
            resolveResilienceReminderType currentDate programStartDate
                |> Maybe.andThen
                    (\reminderType ->
                        let
                            showReminder =
                                Maybe.map
                                    (\scheduledTime ->
                                        posixToMillis scheduledTime < posixToMillis currentTime
                                    )
                                    nurse.resilienceNextReminder
                                    |> Maybe.withDefault
                                        (case reminderType of
                                            ResilienceReminderDrinkWatter ->
                                                let
                                                    scheduledTime =
                                                        Time.Extra.partsToPosix Time.utc
                                                            { year = Date.year programStartDate
                                                            , month = Date.month programStartDate
                                                            , day = Date.day programStartDate
                                                            , hour = 0
                                                            , minute = 0
                                                            , second = 0
                                                            , millisecond = 0
                                                            }
                                                            |> resolveTomorrow8AmUTC
                                                in
                                                posixToMillis scheduledTime < posixToMillis currentTime

                                            ResilienceReminderTakeBreak ->
                                                Time.toHour Time.utc currentTime >= 10
                                        )
                        in
                        if showReminder then
                            let
                                nextReminder =
                                    case reminderType of
                                        ResilienceReminderDrinkWatter ->
                                            resolveTomorrow8AmUTC currentTime

                                        ResilienceReminderTakeBreak ->
                                            Time.Extra.add Time.Extra.Hour 1 Time.utc currentTime

                                nurseWithUpdatedReminder =
                                    { nurse | resilienceNextReminder = Just nextReminder }
                            in
                            Just <|
                                div [ class "ui active modal reminder" ]
                                    [ div [ class "header" ]
                                        [ text <|
                                            translate language <|
                                                Translate.ResilienceReminderHeader nurse.name reminderType
                                        ]
                                    , div
                                        [ class "content" ]
                                        [ p [] [ text <| translate language <| Translate.ResilienceReminderParagraph1 reminderType ]
                                        , p [] [ text <| translate language <| Translate.ResilienceReminderParagraph2 reminderType ]
                                        ]
                                    , div [ class "footer" ]
                                        [ text <| translate language <| Translate.ResilienceReminderFooter reminderType ]
                                    , div
                                        [ class "actions" ]
                                        [ button
                                            [ class "ui primary fluid button"
                                            , onClick <| SendOutMsg <| UpdateNurse nurseId nurseWithUpdatedReminder
                                            ]
                                            [ text <| translate language Translate.OK ]
                                        ]
                                    ]

                        else
                            Nothing
                    )
        )
        nurse.resilienceProgramStartDate


resilienceNotificationDialog :
    Language
    -> Time.Posix
    -> NominalDate
    -> NurseId
    -> Nurse
    -> ModelIndexedDb
    -> Model
    -> Maybe (Html Msg)
resilienceNotificationDialog language currentTime currentDate nurseId nurse db model =
    let
        notificationTimeReached =
            Maybe.map
                (\nextNotification ->
                    posixToMillis nextNotification <= posixToMillis currentTime
                )
                model.nextNotification
                |> Maybe.withDefault False
    in
    if notificationTimeReached && isJust nurse.resilienceProgramStartDate then
        let
            numberOfUnreadMessages =
                resolveNumberOfUnreadMessages currentTime currentDate nurse
        in
        if numberOfUnreadMessages > 0 then
            Just <|
                div [ class "ui active modal notification" ]
                    [ div [ class "header" ]
                        [ text <|
                            translate language <|
                                Translate.ResilienceNotificationHeader nurse.name
                        ]
                    , div [ class "content" ]
                        [ p [] [ text <| translate language <| Translate.ResilienceNotificationNumberOfUnread numberOfUnreadMessages ]
                        , p [] [ text <| translate language Translate.ResilienceNotificationReadNowQuestion ]
                        ]
                    , div [ class "actions" ]
                        [ div [ class "two ui buttons" ]
                            [ button
                                [ class "ui velvet button"
                                , onClick <| HandleNotificationResponse False
                                ]
                                [ text <| translate language Translate.Ignore ]
                            , button
                                [ class "ui primary fluid button"
                                , onClick <| HandleNotificationResponse True
                                ]
                                [ text <| translate language Translate.Read ]
                            ]
                        ]
                    ]

        else
            Nothing

    else
        Nothing


resolveResilienceReminderType : NominalDate -> NominalDate -> Maybe ResilienceReminderType
resolveResilienceReminderType currentDate programStartDate =
    case Date.diff Months programStartDate currentDate of
        0 ->
            -- First month of the program.
            Just ResilienceReminderDrinkWatter

        1 ->
            -- Second month of the program.
            Just ResilienceReminderTakeBreak

        _ ->
            Nothing


resolveTomorrow8AmUTC : Time.Posix -> Time.Posix
resolveTomorrow8AmUTC =
    Time.Extra.ceiling Time.Extra.Day Time.utc
        >> Time.Extra.add Time.Extra.Hour 8 Time.utc


viewLocationName : Bool -> ( Maybe HealthCenterId, Maybe VillageId ) -> ModelIndexedDb -> Html Msg
viewLocationName isChw ( healthCenterId, villageId ) db =
    let
        locationName =
            if isChw then
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
