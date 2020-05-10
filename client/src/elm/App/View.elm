module App.View exposing (view)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInData)
import AssocList as Dict
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.Person.Model exposing (ParticipantDirectoryOperation(..), RegistrationInitiator(..))
import Browser
import Config.View
import Date
import EverySet
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Pages.Clinical.View
import Pages.ClinicalProgressReport.View
import Pages.Clinics.View
import Pages.DemographicsReport.View
import Pages.Device.View
import Pages.IndividualEncounterParticipants.View
import Pages.IndividualEncounterTypes.View
import Pages.MyAccount.View
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.People.View
import Pages.Person.View
import Pages.PinCode.View
import Pages.PregnancyOutcome.Model
import Pages.PregnancyOutcome.View
import Pages.PrenatalActivity.Model
import Pages.PrenatalActivity.View
import Pages.PrenatalEncounter.Model
import Pages.PrenatalEncounter.View
import Pages.PrenatalParticipant.View
import Pages.Relationship.Model
import Pages.Relationship.View
import Pages.Session.Model
import Pages.Session.View exposing (view)
import RemoteData exposing (RemoteData(..), WebData)
import ServiceWorker.View
import Translate exposing (translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (spinner, wrapPage)
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
    div [ class "page container" ]
        [ viewLanguageSwitcherAndVersion model
        , div
            [ class "page-content" ]
            [ html ]
        ]


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
        , span
            [ onClick <| SetActivePage DevicePage
            , class "sync-icon"
            ]
            [ i [ class "icon undo" ] [] ]
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
                viewUserPage userPage model configured


viewUserPage : UserPage -> Model -> ConfiguredModel -> Html Msg
viewUserPage page model configured =
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
                        Pages.Clinical.View.view model.language currentDate model.villageId isChw model.indexedDb
                            |> flexPageWrapper model

                    ClinicsPage clinicId ->
                        Pages.Clinics.View.view model.language currentDate (Tuple.second loggedInModel.nurse) healthCenterId clinicId loggedInModel.clinicsPage model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageClinics)
                            |> flexPageWrapper model

                    ClinicalProgressReportPage prenatalEncounterId ->
                        Pages.ClinicalProgressReport.View.view model.language currentDate prenatalEncounterId model.indexedDb
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

                    DemographicsReportPage prenatalEncounterId ->
                        Pages.DemographicsReport.View.view model.language currentDate prenatalEncounterId model.indexedDb
                            |> flexPageWrapper model

                    EditPersonPage id ->
                        Pages.Person.View.viewCreateEditForm model.language
                            currentDate
                            model.villageId
                            isChw
                            (EditPerson id)
                            ParticipantDirectoryOrigin
                            loggedInModel.editPersonPage
                            model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageEditPerson)
                            |> flexPageWrapper model

                    PersonPage id ->
                        Pages.Person.View.view model.language currentDate isChw id model.indexedDb
                            |> flexPageWrapper model

                    PersonsPage relation ->
                        Pages.People.View.view model.language currentDate model.villageId isChw relation loggedInModel.personsPage model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPagePersons)
                            |> flexPageWrapper model

                    PrenatalParticipantPage id ->
                        Pages.PrenatalParticipant.View.view model.language currentDate id model.indexedDb
                            |> flexPageWrapper model

                    IndividualEncounterParticipantsPage encounterType ->
                        Pages.IndividualEncounterParticipants.View.view model.language currentDate healthCenterId encounterType loggedInModel.individualEncounterParticipantsPage model.indexedDb
                            |> Html.map (MsgLoggedIn << MsgPageIndividualEncounterParticipants)
                            |> flexPageWrapper model

                    RelationshipPage id1 id2 ->
                        let
                            page_ =
                                Dict.get ( id1, id2 ) loggedInModel.relationshipPages
                                    |> Maybe.withDefault Pages.Relationship.Model.emptyModel
                        in
                        Pages.Relationship.View.view model.language currentDate model.villageId isChw id1 id2 model.indexedDb page_
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
                        Pages.PrenatalEncounter.View.view model.language currentDate id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalEncounter id)
                            |> flexPageWrapper model

                    PrenatalActivityPage id activity ->
                        let
                            page_ =
                                Dict.get ( id, activity ) loggedInModel.prenatalActivityPages
                                    |> Maybe.withDefault Pages.PrenatalActivity.Model.emptyModel
                        in
                        Pages.PrenatalActivity.View.view model.language currentDate id activity model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePrenatalActivity id activity)
                            |> flexPageWrapper model

                    IndividualEncounterTypesPage ->
                        Pages.IndividualEncounterTypes.View.view model.language currentDate model.indexedDb
                            |> flexPageWrapper model

                    PregnancyOutcomePage id ->
                        let
                            page_ =
                                Dict.get id loggedInModel.pregnancyOutcomePages
                                    |> Maybe.withDefault Pages.PregnancyOutcome.Model.emptyModel
                        in
                        Pages.PregnancyOutcome.View.view model.language currentDate id model.indexedDb page_
                            |> Html.map (MsgLoggedIn << MsgPagePregnancyOutcome id)
                            |> flexPageWrapper model

            else
                Pages.PinCode.View.view model.language model.activePage (Success loggedInModel.nurse) ( model.healthCenterId, model.villageId ) configured.pinCodePage model.indexedDb
                    |> Html.map MsgPagePinCode
                    |> flexPageWrapper model

        Nothing ->
            Pages.PinCode.View.view model.language
                model.activePage
                (RemoteData.map .nurse configured.loggedIn)
                ( model.healthCenterId, model.villageId )
                configured.pinCodePage
                model.indexedDb
                |> Html.map MsgPagePinCode
                |> flexPageWrapper model


{-| Just show a generic loading indicator, for cases that will resolve soon,
where we don't need to show any progress.
-}
viewLoading : Html any
viewLoading =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui segment center aligned" ]
            [ spinner ]
        ]
