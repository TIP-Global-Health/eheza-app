module App.View exposing (view)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInData)
import AssocList as Dict
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Person.Model exposing (Initiator(..), ParticipantDirectoryOperation(..))
import Browser
import Config.View
import Date
import Error.View
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Pages.AcuteIllnessActivity.Model
import Pages.AcuteIllnessActivity.View
import Pages.AcuteIllnessEncounter.Model
import Pages.AcuteIllnessEncounter.View
import Pages.AcuteIllnessOutcome.Model
import Pages.AcuteIllnessOutcome.View
import Pages.AcuteIllnessParticipant.Model
import Pages.AcuteIllnessParticipant.View
import Pages.AcuteIllnessProgressReport.Model
import Pages.AcuteIllnessProgressReport.View
import Pages.Clinical.View
import Pages.ClinicalProgressReport.Model
import Pages.ClinicalProgressReport.View
import Pages.Clinics.View
import Pages.Dashboard.View
import Pages.DemographicsReport.View
import Pages.Device.View
import Pages.GlobalCaseManagement.View
import Pages.HomeVisitActivity.Model
import Pages.HomeVisitActivity.View
import Pages.HomeVisitEncounter.Model
import Pages.HomeVisitEncounter.View
import Pages.IndividualEncounterParticipants.View
import Pages.IndividualEncounterTypes.View
import Pages.MyAccount.View
import Pages.NutritionActivity.Model
import Pages.NutritionActivity.View
import Pages.NutritionEncounter.Model
import Pages.NutritionEncounter.View
import Pages.NutritionParticipant.View
import Pages.NutritionProgressReport.Model
import Pages.NutritionProgressReport.View
import Pages.Page exposing (DashboardPage(..), Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.PatientRecord.Model
import Pages.PatientRecord.View
import Pages.People.View
import Pages.Person.Model
import Pages.Person.View
import Pages.PinCode.View
import Pages.PregnancyOutcome.Model
import Pages.PregnancyOutcome.View
import Pages.PrenatalActivity.Model
import Pages.PrenatalActivity.View
import Pages.PrenatalEncounter.Model
import Pages.PrenatalEncounter.View
import Pages.PrenatalLabResults.Model
import Pages.PrenatalLabResults.View
import Pages.PrenatalParticipant.Model
import Pages.PrenatalParticipant.View
import Pages.Relationship.Model
import Pages.Relationship.View
import Pages.Session.Model
import Pages.Session.View
import Pages.TraceContact.Model
import Pages.TraceContact.View
import Pages.WellChildActivity.Model
import Pages.WellChildActivity.View
import Pages.WellChildEncounter.Model
import Pages.WellChildEncounter.View
import Pages.WellChildParticipant.View
import Pages.WellChildProgressReport.Model
import Pages.WellChildProgressReport.View
import RemoteData exposing (RemoteData(..), WebData)
import ServiceWorker.View
import SyncManager.View
import Translate exposing (translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (viewLoading)
import Version


view : Model -> Browser.Document Msg
view model =
    { title = translate model.language Translate.AppName
    , body =
        case model.configuration of
            Failure err ->
                [ Config.View.view model.language err ]

            Success configuration ->
                [ viewConfiguredModel model configuration ]

            _ ->
                -- Don't show anything if config resolution is in process but
                -- hasn't failed yet.
                [ viewLoading ]
    }


{-| Given some HTML, wrap it in the new flex-box based structure.
-}
flexPageWrapper : Model -> Html Msg -> Html Msg
flexPageWrapper model html =
    let
        syncManager =
            if model.activePage == DevicePage then
                [ Error.View.view model.language model.configuration model.errors
                , Html.map MsgSyncManager (SyncManager.View.view model.language model.configuration model.indexedDb model.syncManager)
                ]

            else
                []

        content =
            div
                [ class "page-content" ]
                [ html ]
    in
    div [ class "page container" ] <|
        (viewLanguageSwitcherAndVersion model :: syncManager)
            ++ [ content ]


{-| Given some HTML, wrap it the old way.
-}
oldPageWrapper : Model -> Html Msg -> Html Msg
oldPageWrapper model html =
    div [ class "container" ]
        [ viewLanguageSwitcherAndVersion model
        , html
        ]


{-| The language switcher view which sets a preferred language for each user and
saves the current language via the Update function in local storage.
-}
viewLanguageSwitcherAndVersion : Model -> Html Msg
viewLanguageSwitcherAndVersion model =
    let
        devicePageShortcut =
            case model.activePage of
                UserPage (SessionPage _ (NextStepsPage _ _)) ->
                    -- We do not show the shortcut on Next steps page
                    -- to prevent navigating outside of that page,
                    -- before all tasks are completed.
                    emptyNode

                _ ->
                    span
                        [ onClick <| SetActivePage DevicePage
                        , class "sync-icon"
                        ]
                        [ i [ class "icon undo" ] [] ]
    in
    div
        [ class "ui language-switcher" ]
        [ ul
            [ class "links-translate" ]
            [ li
                [ classList
                    [ ( "item english", True )
                    , ( "active", model.language == English )
                    ]
                , onClick <| SetLanguage English
                ]
                [ text "English"
                , a [] [ span [ class "icon-english" ] [] ]
                ]
            , li
                [ classList
                    [ ( "item kinyarwanda", True )
                    , ( "active", model.language == Kinyarwanda )
                    ]
                , onClick <| SetLanguage Kinyarwanda
                ]
                [ text "Kinyarwanda"
                , a [] [ span [ class "icon-kinyarwanda" ] [] ]
                ]
            ]
        , devicePageShortcut
        , span
            [ class "version"
            , onClick <| SetActivePage ServiceWorkerPage
            ]
            [ ServiceWorker.View.viewIcon model.serviceWorker
            , text <| translate model.language Translate.Version
            , text ": "
            , text <| .build Version.version
            ]
        ]


{-| We call this after checking our config. We ask for the model itself,
because we'll need several things from it and it's easier than asking for those
things as separate params. We ask for the `ConfiguredModel` as a guarantee that
we have one. That is, it's the caller's job to do something sensible if we
don't have one.
-}
viewConfiguredModel : Model -> ConfiguredModel -> Html Msg
viewConfiguredModel model configured =
    if not model.serviceWorker.active then
        -- If our service worker is not active, then the only thing we allow
        -- is showing the status of the service worker. (Since we need the
        -- service worker for the normal operation of the app).
        ServiceWorker.View.view model.currentTime model.language model.serviceWorker
            |> Html.map MsgServiceWorker
            |> flexPageWrapper model

    else if not (RemoteData.isSuccess configured.device) then
        -- If our device is not paired, then the only thing we allow is the pairing
        -- of the device, or deployment of a new version.
        case model.activePage of
            ServiceWorkerPage ->
                ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                    |> Html.map MsgServiceWorker
                    |> flexPageWrapper model

            _ ->
                Pages.Device.View.view model.language configured.device model configured.devicePage
                    |> Html.map MsgPageDevice
                    |> flexPageWrapper model

    else
        let
            deviceName =
                if String.isEmpty model.syncManager.syncInfoGeneral.deviceName then
                    Nothing

                else
                    Just model.syncManager.syncInfoGeneral.deviceName
        in
        case model.activePage of
            DevicePage ->
                Pages.Device.View.view model.language configured.device model configured.devicePage
                    |> Html.map MsgPageDevice
                    |> flexPageWrapper model

            PinCodePage ->
                Pages.PinCode.View.view model.language
                    model.activePage
                    (RemoteData.map .nurse configured.loggedIn)
                    ( model.healthCenterId, model.villageId )
                    deviceName
                    configured.pinCodePage
                    model.indexedDb
                    |> Html.map MsgPagePinCode
                    |> flexPageWrapper model

            PageNotFound url ->
                Pages.PageNotFound.View.view model.language url
                    |> oldPageWrapper model

            ServiceWorkerPage ->
                ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                    |> Html.map MsgServiceWorker
                    |> flexPageWrapper model

            UserPage userPage ->
                viewUserPage userPage deviceName model configured


viewUserPage : UserPage -> Maybe String -> Model -> ConfiguredModel -> Html Msg
viewUserPage page deviceName model configured =
    case getLoggedInData model of
        Just ( healthCenterId, loggedInModel ) ->
            let
                currentDate =
                    fromLocalDateTime model.currentTime

                isChw =
                    Tuple.second loggedInModel.nurse
                        |> isCommunityHealthWorker

                selectedAuthorizedHealthCenter =
                    Tuple.second loggedInModel.nurse
                        |> .healthCenters
                        |> EverySet.member healthCenterId
            in
            if selectedAuthorizedHealthCenter then
                case page of
                    MyAccountPage ->
                        Pages.MyAccount.View.view model.language loggedInModel.nurse
                            |> oldPageWrapper model

                    ClinicalPage ->
                        Pages.Clinical.View.view model.language currentDate ( healthCenterId, model.villageId ) isChw model
                            |> flexPageWrapper model

                    ClinicsPage clinicId ->
                        Pages.Clinics.View.view model.language
                            currentDate
                            (Tuple.second loggedInModel.nurse)
                            healthCenterId
                            clinicId
                            loggedInModel.clinicsPage
                            model.indexedDb
                            model.syncManager
                            |> Html.map (MsgLoggedIn << MsgPageClinics)
                            |> flexPageWrapper model

                    ClinicalProgressReportPage initiator prenatalEncounterId ->
                        let
                            page_ =
                                Dict.get prenatalEncounterId loggedInModel.clinicalProgressReportPages
                                    |> Maybe.withDefault Pages.ClinicalProgressReport.Model.emptyModel
                        in
                        Pages.ClinicalProgressReport.View.view model.language currentDate prenatalEncounterId isChw initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageClinicalProgressReport prenatalEncounterId)
                            |> flexPageWrapper model

                    CreatePersonPage relation initiator ->
                        Pages.Person.View.viewCreateEditForm model.language
                            currentDate
                            model.villageId
                            isChw
                            (CreatePerson relation)
                            initiator
                            loggedInModel.createPersonPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageCreatePerson)
                            |> flexPageWrapper model

                    DashboardPage subPage ->
                        let
                            viewDashboardPage =
                                Pages.Dashboard.View.view model.language
                                    subPage
                                    currentDate
                                    healthCenterId
                                    isChw
                                    (Tuple.second loggedInModel.nurse)
                                    loggedInModel.dashboardPage
                                    model.indexedDb
                                    |> Html.map (MsgLoggedIn << MsgPageDashboard subPage)
                                    |> flexPageWrapper model

                            viewPageNotFound =
                                Pages.PageNotFound.View.viewPage model.language (SetActivePage PinCodePage) (UserPage <| DashboardPage subPage)
                        in
                        case subPage of
                            MainPage ->
                                -- Main page is common for Nurse and CHw.
                                viewDashboardPage

                            NursePage _ ->
                                if isChw then
                                    -- Only Nursed may access Nursed pages.
                                    viewPageNotFound

                                else
                                    viewDashboardPage

                            ChwPage _ ->
                                if isChw then
                                    viewDashboardPage

                                else
                                    -- Only CHW may access CHW pages.
                                    viewPageNotFound

                    GlobalCaseManagementPage ->
                        Pages.GlobalCaseManagement.View.view model.language
                            currentDate
                            ( healthCenterId, model.villageId )
                            isChw
                            loggedInModel.globalCaseManagementPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageGlobalCaseManagement)
                            |> flexPageWrapper model

                    DemographicsReportPage initiator prenatalEncounterId ->
                        Pages.DemographicsReport.View.view model.language currentDate prenatalEncounterId initiator model.indexedDb
                            |> flexPageWrapper model

                    EditPersonPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.editPersonPages
                                    |> Maybe.withDefault Pages.Person.Model.emptyEditModel
                        in
                        Pages.Person.View.viewCreateEditForm model.language
                            currentDate
                            model.villageId
                            isChw
                            (EditPerson id)
                            ParticipantDirectoryOrigin
                            page_
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageEditPerson id)
                            |> flexPageWrapper model

                    PersonPage id initiator ->
                        Pages.Person.View.view model.language currentDate isChw initiator id model.indexedDb
                            |> flexPageWrapper model

                    PersonsPage relation initiator ->
                        Pages.People.View.view model.language currentDate model.villageId isChw initiator relation loggedInModel.personsPage model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPagePersons)
                            |> flexPageWrapper model

                    PrenatalParticipantPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.prenatalParticipantPages
                                    |> Maybe.withDefault Pages.PrenatalParticipant.Model.emptyModel
                        in
                        Pages.PrenatalParticipant.View.view model.language currentDate healthCenterId id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalParticipant id)
                            |> flexPageWrapper model

                    NutritionParticipantPage id ->
                        Pages.NutritionParticipant.View.view model.language currentDate healthCenterId id isChw model.indexedDb

                    WellChildParticipantPage id ->
                        Pages.WellChildParticipant.View.view model.language currentDate healthCenterId id isChw model.indexedDb
                            |> flexPageWrapper model

                    AcuteIllnessParticipantPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.acuteIllnessParticipantPages
                                    |> Maybe.withDefault Pages.AcuteIllnessParticipant.Model.emptyModel
                        in
                        Pages.AcuteIllnessParticipant.View.view model.language currentDate healthCenterId id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessParticipant id)
                            |> flexPageWrapper model

                    IndividualEncounterParticipantsPage encounterType ->
                        Pages.IndividualEncounterParticipants.View.view model.language
                            currentDate
                            ( healthCenterId, model.villageId )
                            isChw
                            encounterType
                            loggedInModel.individualEncounterParticipantsPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageIndividualEncounterParticipants)
                            |> flexPageWrapper model

                    RelationshipPage id1 id2 initiator ->
                        let
                            page_ =
                                Dict.get ( id1, id2 ) loggedInModel.relationshipPages
                                    |> Maybe.withDefault Pages.Relationship.Model.emptyModel
                        in
                        Pages.Relationship.View.view model.language
                            currentDate
                            ( healthCenterId, model.villageId )
                            isChw
                            initiator
                            id1
                            id2
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageRelationship id1 id2)
                            |> flexPageWrapper model

                    SessionPage sessionId subPage ->
                        let
                            sessionPages =
                                Dict.get sessionId loggedInModel.sessionPages
                                    |> Maybe.withDefault Pages.Session.Model.emptyModel
                        in
                        Pages.Session.View.view
                            model.language
                            currentDate
                            model.zscores
                            isChw
                            (Tuple.second loggedInModel.nurse)
                            sessionId
                            subPage
                            sessionPages
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageSession sessionId)
                            |> oldPageWrapper model

                    PrenatalEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.prenatalEncounterPages
                                    |> Maybe.withDefault Pages.PrenatalEncounter.Model.emptyModel
                        in
                        Pages.PrenatalEncounter.View.view model.language currentDate id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalEncounter id)
                            |> flexPageWrapper model

                    PrenatalActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.prenatalActivityPages
                                    |> Maybe.withDefault Pages.PrenatalActivity.Model.emptyModel
                        in
                        Pages.PrenatalActivity.View.view model.language currentDate id isChw activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalActivity id activity)
                            |> flexPageWrapper model

                    PrenatalLabResultsPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.prenatalLabResultsPages
                                    |> Maybe.withDefault Pages.PrenatalLabResults.Model.emptyModel
                        in
                        Pages.PrenatalLabResults.View.view model.language currentDate id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalLabResults id)
                            |> flexPageWrapper model

                    IndividualEncounterTypesPage ->
                        Pages.IndividualEncounterTypes.View.view model.language currentDate healthCenterId isChw model
                            |> flexPageWrapper model

                    PregnancyOutcomePage initiator id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.pregnancyOutcomePages
                                    |> Maybe.withDefault Pages.PregnancyOutcome.Model.emptyModel
                        in
                        Pages.PregnancyOutcome.View.view model.language currentDate id isChw initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePregnancyOutcome id)
                            |> flexPageWrapper model

                    NutritionEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.nutritionEncounterPages
                                    |> Maybe.withDefault Pages.NutritionEncounter.Model.emptyModel
                        in
                        Pages.NutritionEncounter.View.view model.language currentDate model.zscores id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNutritionEncounter id)
                            |> flexPageWrapper model

                    NutritionActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.nutritionActivityPages
                                    |> Maybe.withDefault Pages.NutritionActivity.Model.emptyModel
                        in
                        Pages.NutritionActivity.View.view model.language currentDate model.zscores id activity isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNutritionActivity id activity)
                            |> flexPageWrapper model

                    NutritionProgressReportPage encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.nutritionProgressReportPages
                                    |> Maybe.withDefault Pages.NutritionProgressReport.Model.emptyModel
                        in
                        Pages.NutritionProgressReport.View.view model.language currentDate model.zscores encounterId isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNutritionProgressReport encounterId)
                            |> flexPageWrapper model

                    AcuteIllnessEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.acuteIllnessEncounterPages
                                    |> Maybe.withDefault Pages.AcuteIllnessEncounter.Model.emptyModel
                        in
                        Pages.AcuteIllnessEncounter.View.view model.language currentDate id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessEncounter id)
                            |> flexPageWrapper model

                    AcuteIllnessActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.acuteIllnessActivityPages
                                    |> Maybe.withDefault Pages.AcuteIllnessActivity.Model.emptyModel
                        in
                        Pages.AcuteIllnessActivity.View.view model.language currentDate id isChw activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessActivity id activity)
                            |> flexPageWrapper model

                    AcuteIllnessProgressReportPage initiator encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.acuteIllnessProgressReportPages
                                    |> Maybe.withDefault Pages.AcuteIllnessProgressReport.Model.emptyModel
                        in
                        Pages.AcuteIllnessProgressReport.View.view model.language currentDate encounterId isChw initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessProgressReport encounterId)
                            |> flexPageWrapper model

                    AcuteIllnessOutcomePage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.acuteIllnessOutcomePages
                                    |> Maybe.withDefault Pages.AcuteIllnessOutcome.Model.emptyModel
                        in
                        Pages.AcuteIllnessOutcome.View.view model.language currentDate id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessOutcome id)
                            |> flexPageWrapper model

                    HomeVisitEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.homeVisitEncounterPages
                                    |> Maybe.withDefault Pages.HomeVisitEncounter.Model.emptyModel
                        in
                        Pages.HomeVisitEncounter.View.view model.language currentDate id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageHomeVisitEncounter id)
                            |> flexPageWrapper model

                    HomeVisitActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.homeVisitActivityPages
                                    |> Maybe.withDefault Pages.HomeVisitActivity.Model.emptyModel
                        in
                        Pages.HomeVisitActivity.View.view model.language currentDate id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageHomeVisitActivity id activity)
                            |> flexPageWrapper model

                    WellChildEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.wellChildEncounterPages
                                    |> Maybe.withDefault Pages.WellChildEncounter.Model.emptyModel
                        in
                        Pages.WellChildEncounter.View.view model.language currentDate model.zscores id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageWellChildEncounter id)
                            |> flexPageWrapper model

                    WellChildActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.wellChildActivityPages
                                    |> Maybe.withDefault Pages.WellChildActivity.Model.emptyModel
                        in
                        Pages.WellChildActivity.View.view model.language currentDate model.zscores id isChw activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageWellChildActivity id activity)
                            |> flexPageWrapper model

                    WellChildProgressReportPage encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.wellChildProgressReportPages
                                    |> Maybe.withDefault Pages.WellChildProgressReport.Model.emptyModel
                        in
                        Pages.WellChildProgressReport.View.view model.language currentDate model.zscores encounterId isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageWellChildProgressReport encounterId)
                            |> flexPageWrapper model

                    TraceContactPage traceContactId ->
                        let
                            page_ =
                                Dict.get traceContactId loggedInModel.traceContactPages
                                    |> Maybe.withDefault Pages.TraceContact.Model.emptyModel
                        in
                        Pages.TraceContact.View.view model.language
                            currentDate
                            traceContactId
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageTraceContact traceContactId)
                            |> flexPageWrapper model

                    PatientRecordPage initiator personId ->
                        let
                            page_ =
                                Dict.get personId loggedInModel.patientRecordPages
                                    |> Maybe.withDefault Pages.PatientRecord.Model.emptyModel
                        in
                        Pages.PatientRecord.View.view model.language
                            currentDate
                            model.zscores
                            personId
                            isChw
                            initiator
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPagePatientRecord personId)
                            |> flexPageWrapper model

            else
                Pages.PinCode.View.view model.language
                    model.activePage
                    (Success loggedInModel.nurse)
                    ( model.healthCenterId, model.villageId )
                    deviceName
                    configured.pinCodePage
                    model.indexedDb
                    |> Html.map MsgPagePinCode
                    |> flexPageWrapper model

        Nothing ->
            Pages.PinCode.View.view model.language
                model.activePage
                (RemoteData.map .nurse configured.loggedIn)
                ( model.healthCenterId, model.villageId )
                deviceName
                configured.pinCodePage
                model.indexedDb
                |> Html.map MsgPagePinCode
                |> flexPageWrapper model
