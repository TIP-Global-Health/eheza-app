module App.View exposing (view)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInData)
import AssocList as Dict
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Backend.Nurse.Utils exposing (isCommunityHealthWorker, isLabTechnician)
import Backend.Person.Model exposing (Initiator(..), ParticipantDirectoryOperation(..))
import Browser
import Config.Model
import Config.View
import Error.View
import EverySet exposing (EverySet)
import GeoLocation.Model exposing (GeoInfo, ReverseGeoInfo)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Pages.AcuteIllness.Activity.Model
import Pages.AcuteIllness.Activity.View
import Pages.AcuteIllness.Encounter.Model
import Pages.AcuteIllness.Encounter.View
import Pages.AcuteIllness.Outcome.Model
import Pages.AcuteIllness.Outcome.View
import Pages.AcuteIllness.Participant.Model
import Pages.AcuteIllness.Participant.View
import Pages.AcuteIllness.ProgressReport.Model
import Pages.AcuteIllness.ProgressReport.View
import Pages.ChildScoreboard.Activity.Model
import Pages.ChildScoreboard.Activity.View
import Pages.ChildScoreboard.Encounter.Model
import Pages.ChildScoreboard.Encounter.View
import Pages.ChildScoreboard.Participant.View
import Pages.ChildScoreboard.ProgressReport.Model
import Pages.ChildScoreboard.ProgressReport.View
import Pages.Clinical.View
import Pages.Clinics.View
import Pages.Dashboard.View
import Pages.Device.View
import Pages.EducationSession.Model
import Pages.EducationSession.View
import Pages.GlobalCaseManagement.View
import Pages.GroupEncounterTypes.View
import Pages.HIV.Activity.Model
import Pages.HIV.Activity.View
import Pages.HIV.Encounter.Model
import Pages.HIV.Encounter.View
import Pages.HIV.Participant.View
import Pages.HomeVisit.Activity.Model
import Pages.HomeVisit.Activity.View
import Pages.HomeVisit.Encounter.Model
import Pages.HomeVisit.Encounter.View
import Pages.IndividualEncounterParticipants.View
import Pages.IndividualEncounterTypes.View
import Pages.MessagingCenter.Model
import Pages.MessagingCenter.View
import Pages.MessagingGuide.View
import Pages.MyAccount.View
import Pages.NCD.Activity.Model
import Pages.NCD.Activity.View
import Pages.NCD.Encounter.Model
import Pages.NCD.Encounter.View
import Pages.NCD.Participant.View
import Pages.NCD.ProgressReport.Model
import Pages.NCD.ProgressReport.View
import Pages.NCD.RecurrentActivity.Model
import Pages.NCD.RecurrentActivity.View
import Pages.NCD.RecurrentEncounter.Model
import Pages.NCD.RecurrentEncounter.View
import Pages.Nutrition.Activity.Model
import Pages.Nutrition.Activity.View
import Pages.Nutrition.Encounter.Model
import Pages.Nutrition.Encounter.View
import Pages.Nutrition.Participant.View
import Pages.Nutrition.ProgressReport.Model
import Pages.Nutrition.ProgressReport.View
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.PatientRecord.Model
import Pages.PatientRecord.View
import Pages.People.View
import Pages.Person.Model
import Pages.Person.View
import Pages.PinCode.View
import Pages.Prenatal.Activity.Model
import Pages.Prenatal.Activity.View
import Pages.Prenatal.DemographicsReport.View
import Pages.Prenatal.Encounter.Model
import Pages.Prenatal.Encounter.View
import Pages.Prenatal.Outcome.Model
import Pages.Prenatal.Outcome.View
import Pages.Prenatal.Participant.Model
import Pages.Prenatal.Participant.View
import Pages.Prenatal.ProgressReport.Model
import Pages.Prenatal.ProgressReport.View
import Pages.Prenatal.RecurrentActivity.Model
import Pages.Prenatal.RecurrentActivity.View
import Pages.Prenatal.RecurrentEncounter.Model
import Pages.Prenatal.RecurrentEncounter.View
import Pages.Relationship.Model
import Pages.Relationship.View
import Pages.Session.Model
import Pages.Session.View
import Pages.StockManagement.View
import Pages.TraceContact.Model
import Pages.TraceContact.View
import Pages.Tuberculosis.Activity.Model
import Pages.Tuberculosis.Activity.View
import Pages.Tuberculosis.Encounter.Model
import Pages.Tuberculosis.Encounter.View
import Pages.Tuberculosis.Participant.View
import Pages.Tuberculosis.ProgressReport.Model
import Pages.Tuberculosis.ProgressReport.View
import Pages.WellChild.Activity.Model
import Pages.WellChild.Activity.View
import Pages.WellChild.Encounter.Model
import Pages.WellChild.Encounter.View
import Pages.WellChild.Participant.View
import Pages.WellChild.ProgressReport.Model
import Pages.WellChild.ProgressReport.View
import Pages.Wellbeing.View
import RemoteData exposing (RemoteData(..))
import ServiceWorker.View
import SyncManager.Model exposing (Site(..), SiteFeature)
import SyncManager.View
import Translate exposing (translate)
import Translate.Model exposing (Language(..))
import Translate.Utils exposing (languageToString)
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
flexPageWrapper : Config.Model.Model -> Model -> Html Msg -> Html Msg
flexPageWrapper config model html =
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
        (viewLanguageSwitcherAndVersion config model :: syncManager)
            ++ [ content ]


{-| Given some HTML, wrap it the old way.
-}
oldPageWrapper : Config.Model.Model -> Model -> Html Msg -> Html Msg
oldPageWrapper config model html =
    div [ class "container" ]
        [ viewLanguageSwitcherAndVersion config model
        , html
        ]


{-| The language switcher view which sets a preferred language for each user and
saves the current language via the Update function in local storage.
-}
viewLanguageSwitcherAndVersion : Config.Model.Model -> Model -> Html Msg
viewLanguageSwitcherAndVersion config model =
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

        languagesMenu =
            let
                viewItem language =
                    let
                        label =
                            languageToString language
                    in
                    li
                        [ classList
                            [ ( "item", True )
                            , ( "active", model.language == language )
                            ]
                        , onClick <| SetLanguage language
                        ]
                        [ text label
                        , a [] [ span [ class <| "icon-" ++ String.toLower label ] [] ]
                        ]

                languagesBySite =
                    case model.syncManager.syncInfoGeneral.site of
                        SiteRwanda ->
                            [ English, Kinyarwanda ]

                        SiteBurundi ->
                            [ English, Kirundi ]

                        SiteUnknown ->
                            [ English ]
            in
            List.map viewItem languagesBySite
                |> ul [ class "links-translate" ]

        envName =
            if List.any (\domain -> String.contains domain config.backendUrl) [ "pantheonsite.io", "ddev.site" ] then
                span [ class "env-name" ] [ text <| String.toUpper config.name ++ " env" ]

            else
                emptyNode
    in
    div
        [ class "ui language-switcher" ]
        [ languagesMenu
        , devicePageShortcut
        , div
            [ class "version-env"
            , onClick <| SetActivePage ServiceWorkerPage
            ]
            [ span [ class "version" ]
                [ ServiceWorker.View.viewIcon model.serviceWorker
                , text <| translate model.language Translate.Version
                , text ": "
                , text <| .build Version.version
                ]
            , envName
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
            |> flexPageWrapper configured.config model

    else if not (RemoteData.isSuccess configured.device) then
        -- If our device is not paired, then the only thing we allow is the pairing
        -- of the device, or deployment of a new version.
        case model.activePage of
            ServiceWorkerPage ->
                ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                    |> Html.map MsgServiceWorker
                    |> flexPageWrapper configured.config model

            _ ->
                Pages.Device.View.view model.language configured.device model configured.devicePage
                    |> Html.map MsgPageDevice
                    |> flexPageWrapper configured.config model

    else
        let
            features =
                model.syncManager.syncInfoGeneral.features

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
                    |> flexPageWrapper configured.config model

            PinCodePage ->
                Pages.PinCode.View.view model.language
                    model.currentTime
                    features
                    model.activePage
                    (RemoteData.map .nurse configured.loggedIn)
                    ( model.healthCenterId, model.villageId )
                    deviceName
                    configured.pinCodePage
                    model.indexedDb
                    |> Html.map MsgPagePinCode
                    |> flexPageWrapper configured.config model

            PageNotFound url ->
                Pages.PageNotFound.View.view model.language url
                    |> oldPageWrapper configured.config model

            ServiceWorkerPage ->
                ServiceWorker.View.view model.currentTime model.language model.serviceWorker
                    |> Html.map MsgServiceWorker
                    |> flexPageWrapper configured.config model

            UserPage userPage ->
                let
                    site =
                        model.syncManager.syncInfoGeneral.site

                    geoInfo =
                        model.syncManager.geoInfo

                    reverseGeoInfo =
                        model.syncManager.reverseGeoInfo
                in
                viewUserPage userPage deviceName site features geoInfo reverseGeoInfo model configured


viewUserPage : UserPage -> Maybe String -> Site -> EverySet SiteFeature -> GeoInfo -> ReverseGeoInfo -> Model -> ConfiguredModel -> Html Msg
viewUserPage page deviceName site features geoInfo reverseGeoInfo model configured =
    case getLoggedInData model of
        Just ( healthCenterId, loggedInModel ) ->
            let
                selectedAuthorizedHealthCenter =
                    Tuple.second loggedInModel.nurse
                        |> .healthCenters
                        |> EverySet.member healthCenterId
            in
            if selectedAuthorizedHealthCenter then
                let
                    currentDate =
                        fromLocalDateTime model.currentTime

                    ( isChw, isLabTech ) =
                        Tuple.second loggedInModel.nurse
                            |> (\nurse ->
                                    ( isCommunityHealthWorker nurse, isLabTechnician nurse )
                               )
                in
                case page of
                    MyAccountPage ->
                        Pages.MyAccount.View.view model.language loggedInModel.nurse
                            |> oldPageWrapper configured.config model

                    ClinicalPage ->
                        Pages.Clinical.View.view model.language currentDate healthCenterId isChw model
                            |> flexPageWrapper configured.config model

                    ClinicsPage ->
                        Pages.Clinics.View.view model.language
                            currentDate
                            (Tuple.second loggedInModel.nurse)
                            healthCenterId
                            loggedInModel.clinicsPage
                            model.indexedDb
                            model.syncManager
                            |> Html.map (MsgLoggedIn << MsgPageClinics)
                            |> flexPageWrapper configured.config model

                    ClinicalProgressReportPage initiator prenatalEncounterId ->
                        let
                            page_ =
                                Dict.get prenatalEncounterId loggedInModel.clinicalProgressReportPages
                                    |> Maybe.withDefault Pages.Prenatal.ProgressReport.Model.emptyModel
                        in
                        Pages.Prenatal.ProgressReport.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            (Tuple.second loggedInModel.nurse)
                            prenatalEncounterId
                            isChw
                            initiator
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageClinicalProgressReport prenatalEncounterId)
                            |> flexPageWrapper configured.config model

                    CreatePersonPage relation initiator ->
                        Pages.Person.View.viewCreateEditForm model.language
                            currentDate
                            model.coordinates
                            site
                            features
                            geoInfo
                            reverseGeoInfo
                            model.villageId
                            isChw
                            (CreatePerson relation)
                            initiator
                            loggedInModel.createPersonPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageCreatePerson)
                            |> flexPageWrapper configured.config model

                    DashboardPage subPage ->
                        Pages.Dashboard.View.view model.language
                            subPage
                            currentDate
                            site
                            features
                            healthCenterId
                            isChw
                            (Tuple.second loggedInModel.nurse)
                            loggedInModel.dashboardPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageDashboard subPage)
                            |> flexPageWrapper configured.config model

                    GlobalCaseManagementPage ->
                        Pages.GlobalCaseManagement.View.view model.language
                            currentDate
                            features
                            healthCenterId
                            model.villageId
                            isLabTech
                            model.syncManager
                            model.indexedDb
                            loggedInModel.globalCaseManagementPage
                            |> Html.map (MsgLoggedIn << MsgPageGlobalCaseManagement)
                            |> flexPageWrapper configured.config model

                    DemographicsReportPage initiator prenatalEncounterId ->
                        Pages.Prenatal.DemographicsReport.View.view model.language
                            currentDate
                            site
                            prenatalEncounterId
                            initiator
                            model.indexedDb
                            |> flexPageWrapper configured.config model

                    EditPersonPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.editPersonPages
                                    |> Maybe.withDefault (Pages.Person.Model.emptyEditModel site)
                        in
                        Pages.Person.View.viewCreateEditForm model.language
                            currentDate
                            model.coordinates
                            site
                            features
                            geoInfo
                            reverseGeoInfo
                            model.villageId
                            isChw
                            (EditPerson id)
                            ParticipantDirectoryOrigin
                            page_
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageEditPerson id)
                            |> flexPageWrapper configured.config model

                    PersonPage id initiator ->
                        Pages.Person.View.view model.language currentDate isChw initiator id model.indexedDb
                            |> flexPageWrapper configured.config model

                    PersonsPage relation initiator ->
                        Pages.People.View.view model.language
                            currentDate
                            healthCenterId
                            model.villageId
                            isChw
                            initiator
                            relation
                            model.syncManager
                            model.indexedDb
                            loggedInModel.personsPage
                            |> Html.map (MsgLoggedIn << MsgPagePersons)
                            |> flexPageWrapper configured.config model

                    PrenatalParticipantPage initiator id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.prenatalParticipantPages
                                    |> Maybe.withDefault Pages.Prenatal.Participant.Model.emptyModel
                        in
                        Pages.Prenatal.Participant.View.view model.language currentDate healthCenterId id isChw initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalParticipant id)
                            |> flexPageWrapper configured.config model

                    NutritionParticipantPage initiator id ->
                        Pages.Nutrition.Participant.View.view model.language currentDate healthCenterId id isChw initiator model.indexedDb
                            |> flexPageWrapper configured.config model

                    WellChildParticipantPage initiator id ->
                        Pages.WellChild.Participant.View.view model.language currentDate healthCenterId id isChw initiator model.indexedDb
                            |> flexPageWrapper configured.config model

                    AcuteIllnessParticipantPage initiator id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.acuteIllnessParticipantPages
                                    |> Maybe.withDefault Pages.AcuteIllness.Participant.Model.emptyModel
                        in
                        Pages.AcuteIllness.Participant.View.view model.language currentDate healthCenterId id isChw initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessParticipant id)
                            |> flexPageWrapper configured.config model

                    NCDParticipantPage initiator id ->
                        Pages.NCD.Participant.View.view model.language currentDate healthCenterId id initiator model.indexedDb
                            |> flexPageWrapper configured.config model

                    ChildScoreboardParticipantPage id ->
                        Pages.ChildScoreboard.Participant.View.view model.language currentDate healthCenterId id model.indexedDb
                            |> flexPageWrapper configured.config model

                    TuberculosisParticipantPage id ->
                        Pages.Tuberculosis.Participant.View.view model.language currentDate healthCenterId id model.indexedDb
                            |> flexPageWrapper configured.config model

                    HIVParticipantPage id ->
                        Pages.HIV.Participant.View.view model.language currentDate healthCenterId id model.indexedDb
                            |> flexPageWrapper configured.config model

                    HealthyStartParticipantPage id ->
                        Pages.HealthyStart.Participant.View.view model.language currentDate healthCenterId id model.indexedDb
                            |> flexPageWrapper configured.config model

                    HealthyStartEncounterPage id ->
                        Pages.HealthyStart.Encounter.View.view model.language currentDate healthCenterId id model.indexedDb
                            |> flexPageWrapper configured.config model

                    IndividualEncounterParticipantsPage encounterType ->
                        Pages.IndividualEncounterParticipants.View.view model.language
                            currentDate
                            ( healthCenterId, model.villageId )
                            isChw
                            encounterType
                            loggedInModel.individualEncounterParticipantsPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageIndividualEncounterParticipants)
                            |> flexPageWrapper configured.config model

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
                            |> flexPageWrapper configured.config model

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
                            site
                            features
                            isChw
                            (Tuple.second loggedInModel.nurse)
                            sessionId
                            subPage
                            sessionPages
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageSession sessionId)
                            |> oldPageWrapper configured.config model

                    PrenatalEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.prenatalEncounterPages
                                    |> Maybe.withDefault Pages.Prenatal.Encounter.Model.emptyModel
                        in
                        Pages.Prenatal.Encounter.View.view model.language
                            currentDate
                            site
                            id
                            isChw
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalEncounter id)
                            |> flexPageWrapper configured.config model

                    PrenatalActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.prenatalActivityPages
                                    |> Maybe.withDefault Pages.Prenatal.Activity.Model.emptyModel
                        in
                        Pages.Prenatal.Activity.View.view model.language
                            currentDate
                            model.zscores
                            site
                            id
                            isChw
                            activity
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalActivity id activity)
                            |> flexPageWrapper configured.config model

                    PrenatalRecurrentEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.prenatalRecurrentEncounterPages
                                    |> Maybe.withDefault Pages.Prenatal.RecurrentEncounter.Model.emptyModel
                        in
                        Pages.Prenatal.RecurrentEncounter.View.view model.language currentDate (Tuple.second loggedInModel.nurse) id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalRecurrentEncounter id)
                            |> flexPageWrapper configured.config model

                    PrenatalRecurrentActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.prenatalRecurrentActivityPages
                                    |> Maybe.withDefault Pages.Prenatal.RecurrentActivity.Model.emptyModel
                        in
                        Pages.Prenatal.RecurrentActivity.View.view model.language
                            currentDate
                            (Tuple.second loggedInModel.nurse)
                            id
                            activity
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalRecurrentActivity id activity)
                            |> flexPageWrapper configured.config model

                    PrenatalLabsHistoryPage id labEncounterId lab ->
                        let
                            page_ =
                                Dict.get ( id, labEncounterId, lab ) loggedInModel.prenatalLabsHistoryPages
                                    |> Maybe.withDefault Pages.Prenatal.RecurrentActivity.Model.emptyLabResultsData
                        in
                        Pages.Prenatal.RecurrentActivity.View.viewLabsHistory model.language currentDate id labEncounterId lab model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalLabsHistory id labEncounterId lab)
                            |> flexPageWrapper configured.config model

                    IndividualEncounterTypesPage ->
                        Pages.IndividualEncounterTypes.View.view model.language currentDate features healthCenterId isChw model
                            |> flexPageWrapper configured.config model

                    GroupEncounterTypesPage ->
                        Pages.GroupEncounterTypes.View.view model.language
                            currentDate
                            features
                            healthCenterId
                            (Tuple.first loggedInModel.nurse)
                            model
                            |> flexPageWrapper configured.config model

                    PregnancyOutcomePage initiator id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.pregnancyOutcomePages
                                    |> Maybe.withDefault Pages.Prenatal.Outcome.Model.emptyModel
                        in
                        Pages.Prenatal.Outcome.View.view model.language currentDate id isChw initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePregnancyOutcome id)
                            |> flexPageWrapper configured.config model

                    NutritionEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.nutritionEncounterPages
                                    |> Maybe.withDefault Pages.Nutrition.Encounter.Model.emptyModel
                        in
                        Pages.Nutrition.Encounter.View.view model.language
                            currentDate
                            model.zscores
                            features
                            id
                            isChw
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageNutritionEncounter id)
                            |> flexPageWrapper configured.config model

                    NutritionActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.nutritionActivityPages
                                    |> Maybe.withDefault Pages.Nutrition.Activity.Model.emptyModel
                        in
                        Pages.Nutrition.Activity.View.view model.language
                            currentDate
                            model.zscores
                            site
                            id
                            activity
                            isChw
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageNutritionActivity id activity)
                            |> flexPageWrapper configured.config model

                    NutritionProgressReportPage encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.nutritionProgressReportPages
                                    |> Maybe.withDefault Pages.Nutrition.ProgressReport.Model.emptyModel
                        in
                        Pages.Nutrition.ProgressReport.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            encounterId
                            isChw
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageNutritionProgressReport encounterId)
                            |> flexPageWrapper configured.config model

                    AcuteIllnessEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.acuteIllnessEncounterPages
                                    |> Maybe.withDefault Pages.AcuteIllness.Encounter.Model.emptyModel
                        in
                        Pages.AcuteIllness.Encounter.View.view model.language currentDate features id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessEncounter id)
                            |> flexPageWrapper configured.config model

                    AcuteIllnessActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.acuteIllnessActivityPages
                                    |> Maybe.withDefault Pages.AcuteIllness.Activity.Model.emptyModel
                        in
                        Pages.AcuteIllness.Activity.View.view model.language
                            currentDate
                            site
                            features
                            geoInfo
                            id
                            isChw
                            activity
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessActivity id activity)
                            |> flexPageWrapper configured.config model

                    AcuteIllnessProgressReportPage initiator encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.acuteIllnessProgressReportPages
                                    |> Maybe.withDefault Pages.AcuteIllness.ProgressReport.Model.emptyModel
                        in
                        Pages.AcuteIllness.ProgressReport.View.view model.language
                            currentDate
                            site
                            features
                            encounterId
                            isChw
                            initiator
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessProgressReport encounterId)
                            |> flexPageWrapper configured.config model

                    AcuteIllnessOutcomePage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.acuteIllnessOutcomePages
                                    |> Maybe.withDefault Pages.AcuteIllness.Outcome.Model.emptyModel
                        in
                        Pages.AcuteIllness.Outcome.View.view model.language currentDate features id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageAcuteIllnessOutcome id)
                            |> flexPageWrapper configured.config model

                    HomeVisitEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.homeVisitEncounterPages
                                    |> Maybe.withDefault Pages.HomeVisit.Encounter.Model.emptyModel
                        in
                        Pages.HomeVisit.Encounter.View.view model.language currentDate id isChw model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageHomeVisitEncounter id)
                            |> flexPageWrapper configured.config model

                    HomeVisitActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.homeVisitActivityPages
                                    |> Maybe.withDefault Pages.HomeVisit.Activity.Model.emptyModel
                        in
                        Pages.HomeVisit.Activity.View.view model.language currentDate id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageHomeVisitActivity id activity)
                            |> flexPageWrapper configured.config model

                    WellChildEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.wellChildEncounterPages
                                    |> Maybe.withDefault Pages.WellChild.Encounter.Model.emptyModel
                        in
                        Pages.WellChild.Encounter.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            id
                            isChw
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageWellChildEncounter id)
                            |> flexPageWrapper configured.config model

                    WellChildActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.wellChildActivityPages
                                    |> Maybe.withDefault Pages.WellChild.Activity.Model.emptyModel
                        in
                        Pages.WellChild.Activity.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            id
                            isChw
                            activity
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageWellChildActivity id activity)
                            |> flexPageWrapper configured.config model

                    WellChildProgressReportPage encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.wellChildProgressReportPages
                                    |> Maybe.withDefault Pages.WellChild.ProgressReport.Model.emptyModel
                        in
                        Pages.WellChild.ProgressReport.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            encounterId
                            isChw
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageWellChildProgressReport encounterId)
                            |> flexPageWrapper configured.config model

                    NCDEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.ncdEncounterPages
                                    |> Maybe.withDefault Pages.NCD.Encounter.Model.emptyModel
                        in
                        Pages.NCD.Encounter.View.view model.language currentDate id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNCDEncounter id)
                            |> flexPageWrapper configured.config model

                    NCDActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.ncdActivityPages
                                    |> Maybe.withDefault Pages.NCD.Activity.Model.emptyModel
                        in
                        Pages.NCD.Activity.View.view model.language currentDate site id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNCDActivity id activity)
                            |> flexPageWrapper configured.config model

                    NCDRecurrentEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.ncdRecurrentEncounterPages
                                    |> Maybe.withDefault Pages.NCD.RecurrentEncounter.Model.emptyModel
                        in
                        Pages.NCD.RecurrentEncounter.View.view model.language currentDate id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNCDRecurrentEncounter id)
                            |> flexPageWrapper configured.config model

                    NCDRecurrentActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.ncdRecurrentActivityPages
                                    |> Maybe.withDefault Pages.NCD.RecurrentActivity.Model.emptyModel
                        in
                        Pages.NCD.RecurrentActivity.View.view model.language currentDate id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNCDRecurrentActivity id activity)
                            |> flexPageWrapper configured.config model

                    NCDProgressReportPage initiator ->
                        let
                            encounterId =
                                case initiator of
                                    InitiatorEncounterPage id ->
                                        id

                                    InitiatorRecurrentEncounterPage id ->
                                        id

                            page_ =
                                Dict.get encounterId loggedInModel.ncdProgressReportPages
                                    |> Maybe.withDefault Pages.NCD.ProgressReport.Model.emptyModel
                        in
                        Pages.NCD.ProgressReport.View.view model.language currentDate site features encounterId initiator model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageNCDProgressReport encounterId)
                            |> flexPageWrapper configured.config model

                    ChildScoreboardEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.childScoreboardEncounterPages
                                    |> Maybe.withDefault Pages.ChildScoreboard.Encounter.Model.emptyModel
                        in
                        Pages.ChildScoreboard.Encounter.View.view model.language currentDate site id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageChildScoreboardEncounter id)
                            |> flexPageWrapper configured.config model

                    ChildScoreboardActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.childScoreboardActivityPages
                                    |> Maybe.withDefault Pages.ChildScoreboard.Activity.Model.emptyModel
                        in
                        Pages.ChildScoreboard.Activity.View.view model.language
                            currentDate
                            model.zscores
                            site
                            id
                            activity
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageChildScoreboardActivity id activity)
                            |> flexPageWrapper configured.config model

                    ChildScoreboardProgressReportPage encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.childScoreboardReportPages
                                    |> Maybe.withDefault Pages.ChildScoreboard.ProgressReport.Model.emptyModel
                        in
                        Pages.ChildScoreboard.ProgressReport.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            encounterId
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageChildScoreboardReport encounterId)
                            |> flexPageWrapper configured.config model

                    TuberculosisEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.tuberculosisEncounterPages
                                    |> Maybe.withDefault Pages.Tuberculosis.Encounter.Model.emptyModel
                        in
                        Pages.Tuberculosis.Encounter.View.view model.language currentDate site id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageTuberculosisEncounter id)
                            |> flexPageWrapper configured.config model

                    TuberculosisActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.tuberculosisActivityPages
                                    |> Maybe.withDefault Pages.Tuberculosis.Activity.Model.emptyModel
                        in
                        Pages.Tuberculosis.Activity.View.view model.language currentDate id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageTuberculosisActivity id activity)
                            |> flexPageWrapper configured.config model

                    TuberculosisProgressReportPage encounterId ->
                        let
                            page_ =
                                Dict.get encounterId loggedInModel.tuberculosisProgressReportPages
                                    |> Maybe.withDefault Pages.Tuberculosis.ProgressReport.Model.emptyModel
                        in
                        Pages.Tuberculosis.ProgressReport.View.view model.language
                            currentDate
                            site
                            features
                            encounterId
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageTuberculosisProgressReport encounterId)
                            |> flexPageWrapper configured.config model

                    EducationSessionPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.educationSessionPages
                                    |> Maybe.withDefault Pages.EducationSession.Model.emptyModel
                        in
                        Pages.EducationSession.View.view model.language currentDate model.villageId id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageEducationSession id)
                            |> flexPageWrapper configured.config model

                    HIVEncounterPage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.hivEncounterPages
                                    |> Maybe.withDefault Pages.HIV.Encounter.Model.emptyModel
                        in
                        Pages.HIV.Encounter.View.view model.language currentDate site id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageHIVEncounter id)
                            |> flexPageWrapper configured.config model

                    HIVActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.hivActivityPages
                                    |> Maybe.withDefault Pages.HIV.Activity.Model.emptyModel
                        in
                        Pages.HIV.Activity.View.view model.language currentDate id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPageHIVActivity id activity)
                            |> flexPageWrapper configured.config model

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
                            |> flexPageWrapper configured.config model

                    PatientRecordPage initiator personId ->
                        let
                            page_ =
                                Dict.get personId loggedInModel.patientRecordPages
                                    |> Maybe.withDefault Pages.PatientRecord.Model.emptyModel
                        in
                        Pages.PatientRecord.View.view model.language
                            currentDate
                            model.zscores
                            site
                            features
                            personId
                            isChw
                            initiator
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPagePatientRecord personId)
                            |> flexPageWrapper configured.config model

                    MessagingCenterPage ->
                        let
                            ( nurseId, nurse ) =
                                loggedInModel.nurse

                            page_ =
                                Dict.get nurseId loggedInModel.messagingCenterPages
                                    |> Maybe.withDefault Pages.MessagingCenter.Model.emptyModel
                        in
                        Pages.MessagingCenter.View.view model.language
                            model.currentTime
                            nurseId
                            nurse
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageMessagingCenter nurseId)
                            |> flexPageWrapper configured.config model

                    WellbeingPage ->
                        let
                            ( nurseId, nurse ) =
                                loggedInModel.nurse

                            page_ =
                                Dict.get nurseId loggedInModel.messagingCenterPages
                                    |> Maybe.withDefault Pages.MessagingCenter.Model.emptyModel
                        in
                        Pages.Wellbeing.View.view model.language
                            model.currentTime
                            nurseId
                            nurse
                            model.indexedDb
                            page_
                            |> Html.map (MsgLoggedIn << MsgPageMessagingCenter nurseId)
                            |> flexPageWrapper configured.config model

                    MessagingGuide ->
                        Pages.MessagingGuide.View.view model.language
                            |> flexPageWrapper configured.config model

                    StockManagementPage ->
                        let
                            ( nurseId, nurse ) =
                                loggedInModel.nurse
                        in
                        Pages.StockManagement.View.view model.language
                            currentDate
                            model.healthCenterId
                            nurseId
                            nurse
                            model.syncManager.syncInfoAuthorities
                            model.indexedDb
                            loggedInModel.stockManagementPage
                            |> Html.map (MsgLoggedIn << MsgPageStockManagement)
                            |> flexPageWrapper configured.config model

            else
                Pages.PinCode.View.view model.language
                    model.currentTime
                    features
                    model.activePage
                    (Success loggedInModel.nurse)
                    ( model.healthCenterId, model.villageId )
                    deviceName
                    configured.pinCodePage
                    model.indexedDb
                    |> Html.map MsgPagePinCode
                    |> flexPageWrapper configured.config model

        Nothing ->
            Pages.PinCode.View.view model.language
                model.currentTime
                features
                model.activePage
                (RemoteData.map .nurse configured.loggedIn)
                ( model.healthCenterId, model.villageId )
                deviceName
                configured.pinCodePage
                model.indexedDb
                |> Html.map MsgPagePinCode
                |> flexPageWrapper configured.config model
