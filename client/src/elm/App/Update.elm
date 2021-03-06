module App.Update exposing (init, subscriptions, updateAndThenFetch)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import App.Fetch
import App.Model exposing (..)
import App.Ports exposing (..)
import App.Utils exposing (getLoggedInData, updateSubModel)
import AssocList as Dict
import Backend.Endpoints exposing (nurseEndpoint)
import Backend.Model
import Backend.Nurse.Utils exposing (isCommunityHealthWorker)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.Person.Model exposing (Initiator(..))
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
import Backend.Session.Utils exposing (getSession)
import Backend.Update
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Browser
import Browser.Navigation as Nav
import Config
import Device.Decoder
import Device.Encoder
import Dict as LegacyDict
import Gizra.NominalDate exposing (fromLocalDateTime)
import Http exposing (Error(..))
import HttpBuilder
import Json.Decode
import Json.Encode
import Pages.AcuteIllnessActivity.Model
import Pages.AcuteIllnessActivity.Update
import Pages.AcuteIllnessEncounter.Model
import Pages.AcuteIllnessEncounter.Update
import Pages.AcuteIllnessOutcome.Model
import Pages.AcuteIllnessOutcome.Update
import Pages.AcuteIllnessParticipant.Model
import Pages.AcuteIllnessParticipant.Update
import Pages.AcuteIllnessProgressReport.Model
import Pages.AcuteIllnessProgressReport.Update
import Pages.Clinics.Update
import Pages.Dashboard.Model
import Pages.Dashboard.Update
import Pages.Device.Model
import Pages.Device.Update
import Pages.GlobalCaseManagement.Update
import Pages.HomeVisitActivity.Model
import Pages.HomeVisitActivity.Update
import Pages.HomeVisitEncounter.Model
import Pages.HomeVisitEncounter.Update
import Pages.IndividualEncounterParticipants.Update
import Pages.NutritionActivity.Model
import Pages.NutritionActivity.Update
import Pages.NutritionEncounter.Model
import Pages.NutritionEncounter.Update
import Pages.NutritionProgressReport.Model
import Pages.NutritionProgressReport.Update
import Pages.Page exposing (..)
import Pages.People.Update
import Pages.Person.Model
import Pages.Person.Update
import Pages.PinCode.Model
import Pages.PinCode.Update
import Pages.PregnancyOutcome.Model
import Pages.PregnancyOutcome.Update
import Pages.PrenatalActivity.Model
import Pages.PrenatalActivity.Update
import Pages.PrenatalEncounter.Model
import Pages.PrenatalEncounter.Update
import Pages.PrenatalParticipant.Model
import Pages.PrenatalParticipant.Update
import Pages.Relationship.Model
import Pages.Relationship.Update
import Pages.Router exposing (activePageByUrl, pageToFragment)
import Pages.Session.Model
import Pages.Session.Update
import Pages.TraceContact.Model
import Pages.TraceContact.Update
import Pages.WellChildActivity.Model
import Pages.WellChildActivity.Update
import Pages.WellChildEncounter.Model
import Pages.WellChildEncounter.Update
import Pages.WellChildProgressReport.Model
import Pages.WellChildProgressReport.Update
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid, select, toCmd)
import ServiceWorker.Model
import ServiceWorker.Update
import SyncManager.Model
import SyncManager.Update
import Task
import Time
import Translate.Model exposing (Language(..))
import Translate.Utils exposing (languageFromCode, languageToCode)
import Update.Extra exposing (sequence)
import Url
import Version
import ZScore.Model
import ZScore.Update


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        activeLanguage =
            case languageFromCode flags.activeLanguage of
                Ok language ->
                    language

                Err msg ->
                    English

        url_ =
            { url | query = Nothing }

        model =
            emptyModel key url_ flags

        ( updatedModel, cmd ) =
            case Dict.get flags.hostname Config.configs of
                Just config ->
                    let
                        fetchCachedDevice =
                            HttpBuilder.get "/sw/config/device"
                                |> HttpBuilder.withExpectJson (Device.Decoder.decode config.backendUrl)
                                |> HttpBuilder.toTask
                                |> RemoteData.fromTask
                                |> Task.map
                                    (\response ->
                                        -- We convert 404s to NotAsked ...  if
                                        -- we can't find it locally, we'll ask
                                        -- for a pairing code.
                                        case response of
                                            Failure (BadStatus { status }) ->
                                                if status.code == 404 then
                                                    NotAsked

                                                else
                                                    response

                                            _ ->
                                                response
                                    )
                                |> Task.perform HandlePairedDevice

                        cmd_ =
                            -- We always check the cache for an offline session, since that affects
                            -- the UI we'll offer to show at a basic level. (An alternative would be
                            -- to fetch it only when we really, really need it).
                            Cmd.batch
                                [ Task.perform Tick Time.now
                                , fetchCachedDevice
                                , Nav.pushUrl model.navigationKey (Url.toString model.url)
                                ]

                        configuredModel =
                            { config = config
                            , device = Loading
                            , devicePage = Pages.Device.Model.emptyModel
                            , loggedIn = NotAsked
                            , pinCodePage = Pages.PinCode.Model.emptyModel
                            }

                        tryPinCode =
                            if flags.pinCode == "" then
                                []

                            else
                                [ TryPinCode flags.pinCode ]
                    in
                    ( { model | configuration = Success configuredModel }
                    , cmd_
                    )
                        |> sequence update
                            (List.append tryPinCode
                                [ MsgServiceWorker (ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Register)
                                , MsgZScore ZScore.Model.FetchAllTables
                                ]
                            )

                Nothing ->
                    ( { model | configuration = Failure <| "No config found for: " ++ flags.hostname }
                    , Cmd.none
                    )
    in
    ( { updatedModel | language = activeLanguage }, cmd )


updateAndThenFetch : Msg -> Model -> ( Model, Cmd Msg )
updateAndThenFetch msg model =
    -- If it's a CheckData message, then `update` will turn this off.
    update msg
        { model | scheduleDataWantedCheck = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none )

        currentDate =
            fromLocalDateTime model.currentTime

        loggedInData =
            getLoggedInData model

        isChw =
            loggedInData
                |> Maybe.map (Tuple.second >> .nurse >> Tuple.second >> isCommunityHealthWorker)
                |> Maybe.withDefault False
    in
    case msg of
        MsgIndexedDb subMsg ->
            let
                nurseId =
                    loggedInData
                        |> Maybe.map (Tuple.second >> .nurse >> Tuple.first)

                ( subModel, subCmd, extraMsgs ) =
                    Backend.Update.updateIndexedDb model.language
                        currentDate
                        model.currentTime
                        model.zscores
                        nurseId
                        model.healthCenterId
                        model.villageId
                        isChw
                        model.activePage
                        model.syncManager
                        subMsg
                        model.indexedDb

                -- Most revisions are handled at the IndexedDB level, but there
                -- is at least one we need to catch here.
                revisionMsgs =
                    case subMsg of
                        Backend.Model.HandleRevisions revisions ->
                            List.filterMap (handleRevision model) revisions

                        _ ->
                            []
            in
            ( { model | indexedDb = subModel }
            , Cmd.map MsgIndexedDb subCmd
            )
                |> sequence update (extraMsgs ++ revisionMsgs)

        MsgLoggedIn loggedInMsg ->
            updateLoggedIn
                (\data ->
                    case loggedInMsg of
                        MsgPageClinics subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.Clinics.Update.update subMsg data.clinicsPage
                            in
                            ( { data | clinicsPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPageClinics) subCmd
                            , appMsgs
                            )

                        MsgPageCreatePerson subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.Person.Update.update currentDate model.healthCenterId model.villageId isChw subMsg model.indexedDb data.createPersonPage
                            in
                            ( { data | createPersonPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPageCreatePerson) subCmd
                            , appMsgs
                            )

                        MsgPageDashboard subPage subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.Dashboard.Update.update currentDate model.healthCenterId subPage model.indexedDb subMsg data.dashboardPage
                            in
                            ( { data | dashboardPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPageDashboard subPage) subCmd
                            , appMsgs
                            )

                        MsgPageGlobalCaseManagement subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.GlobalCaseManagement.Update.update currentDate model.healthCenterId subMsg model.indexedDb data.globalCaseManagementPage
                            in
                            ( { data | globalCaseManagementPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPageGlobalCaseManagement) subCmd
                            , appMsgs
                            )

                        MsgPageEditPerson id subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Dict.get id data.editPersonPages
                                        |> Maybe.withDefault Pages.Person.Model.emptyEditModel
                                        |> Pages.Person.Update.update currentDate model.healthCenterId model.villageId isChw subMsg model.indexedDb
                            in
                            ( { data | editPersonPages = Dict.insert id subModel data.editPersonPages }
                            , Cmd.map (MsgLoggedIn << MsgPageEditPerson id) subCmd
                            , appMsgs
                            )

                        MsgPagePersons subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.People.Update.update subMsg data.personsPage
                            in
                            ( { data | personsPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPagePersons) subCmd
                            , appMsgs
                            )

                        MsgPagePrenatalParticipant id subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    data.prenatalParticipantPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.PrenatalParticipant.Model.emptyModel
                                        |> Pages.PrenatalParticipant.Update.update currentDate id subMsg
                            in
                            ( { data | prenatalParticipantPages = Dict.insert id subModel data.prenatalParticipantPages }
                            , Cmd.map (MsgLoggedIn << MsgPagePrenatalParticipant id) subCmd
                            , appMsgs
                            )

                        MsgPageIndividualEncounterParticipants subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Pages.IndividualEncounterParticipants.Update.update subMsg data.individualEncounterParticipantsPage
                            in
                            ( { data | individualEncounterParticipantsPage = subModel }
                            , Cmd.map (MsgLoggedIn << MsgPageIndividualEncounterParticipants) subCmd
                            , appMsgs
                            )

                        MsgPageRelationship id1 id2 subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.relationshipPages
                                        |> Dict.get ( id1, id2 )
                                        |> Maybe.withDefault Pages.Relationship.Model.emptyModel
                                        |> Pages.Relationship.Update.update id1 id2 subMsg
                            in
                            ( { data | relationshipPages = Dict.insert ( id1, id2 ) subModel data.relationshipPages }
                            , Cmd.map (MsgLoggedIn << MsgPageRelationship id1 id2) subCmd
                            , extraMsgs
                            )

                        MsgPageAcuteIllnessParticipant id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.acuteIllnessParticipantPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.AcuteIllnessParticipant.Model.emptyModel
                                        |> Pages.AcuteIllnessParticipant.Update.update currentDate id subMsg
                            in
                            ( { data | acuteIllnessParticipantPages = Dict.insert id subModel data.acuteIllnessParticipantPages }
                            , Cmd.map (MsgLoggedIn << MsgPageAcuteIllnessParticipant id) subCmd
                            , extraMsgs
                            )

                        MsgPageSession sessionId subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.sessionPages
                                        |> Dict.get sessionId
                                        |> Maybe.withDefault Pages.Session.Model.emptyModel
                                        |> Pages.Session.Update.update currentDate model.zscores sessionId model.indexedDb subMsg
                            in
                            ( { data | sessionPages = Dict.insert sessionId subModel data.sessionPages }
                            , Cmd.map (MsgLoggedIn << MsgPageSession sessionId) subCmd
                            , extraMsgs
                            )

                        MsgPagePrenatalEncounter id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.prenatalEncounterPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.PrenatalEncounter.Model.emptyModel
                                        |> Pages.PrenatalEncounter.Update.update subMsg
                            in
                            ( { data | prenatalEncounterPages = Dict.insert id subModel data.prenatalEncounterPages }
                            , Cmd.map (MsgLoggedIn << MsgPagePrenatalEncounter id) subCmd
                            , extraMsgs
                            )

                        MsgPageNutritionEncounter id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.nutritionEncounterPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.NutritionEncounter.Model.emptyModel
                                        |> Pages.NutritionEncounter.Update.update subMsg
                            in
                            ( { data | nutritionEncounterPages = Dict.insert id subModel data.nutritionEncounterPages }
                            , Cmd.map (MsgLoggedIn << MsgPageNutritionEncounter id) subCmd
                            , extraMsgs
                            )

                        MsgPageAcuteIllnessEncounter id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.acuteIllnessEncounterPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.AcuteIllnessEncounter.Model.emptyModel
                                        |> Pages.AcuteIllnessEncounter.Update.update subMsg
                            in
                            ( { data | acuteIllnessEncounterPages = Dict.insert id subModel data.acuteIllnessEncounterPages }
                            , Cmd.map (MsgLoggedIn << MsgPageAcuteIllnessEncounter id) subCmd
                            , extraMsgs
                            )

                        MsgPageHomeVisitEncounter id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.homeVisitEncounterPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.HomeVisitEncounter.Model.emptyModel
                                        |> Pages.HomeVisitEncounter.Update.update subMsg
                            in
                            ( { data | homeVisitEncounterPages = Dict.insert id subModel data.homeVisitEncounterPages }
                            , Cmd.map (MsgLoggedIn << MsgPageHomeVisitEncounter id) subCmd
                            , extraMsgs
                            )

                        MsgPageWellChildEncounter id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.wellChildEncounterPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.WellChildEncounter.Model.emptyModel
                                        |> Pages.WellChildEncounter.Update.update currentDate model.zscores isChw model.indexedDb subMsg
                            in
                            ( { data | wellChildEncounterPages = Dict.insert id subModel data.wellChildEncounterPages }
                            , Cmd.map (MsgLoggedIn << MsgPageWellChildEncounter id) subCmd
                            , extraMsgs
                            )

                        MsgPagePrenatalActivity id activity subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.prenatalActivityPages
                                        |> Dict.get ( id, activity )
                                        |> Maybe.withDefault Pages.PrenatalActivity.Model.emptyModel
                                        |> Pages.PrenatalActivity.Update.update currentDate id model.indexedDb subMsg
                            in
                            ( { data | prenatalActivityPages = Dict.insert ( id, activity ) subModel data.prenatalActivityPages }
                            , Cmd.map (MsgLoggedIn << MsgPagePrenatalActivity id activity) subCmd
                            , extraMsgs
                            )

                        MsgPageNutritionActivity id activity subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.nutritionActivityPages
                                        |> Dict.get ( id, activity )
                                        |> Maybe.withDefault Pages.NutritionActivity.Model.emptyModel
                                        |> Pages.NutritionActivity.Update.update currentDate id model.indexedDb subMsg
                            in
                            ( { data | nutritionActivityPages = Dict.insert ( id, activity ) subModel data.nutritionActivityPages }
                            , Cmd.map (MsgLoggedIn << MsgPageNutritionActivity id activity) subCmd
                            , extraMsgs
                            )

                        MsgPageAcuteIllnessActivity id activity subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.acuteIllnessActivityPages
                                        |> Dict.get ( id, activity )
                                        |> Maybe.withDefault Pages.AcuteIllnessActivity.Model.emptyModel
                                        |> Pages.AcuteIllnessActivity.Update.update currentDate model.healthCenterId id model.indexedDb subMsg
                            in
                            ( { data | acuteIllnessActivityPages = Dict.insert ( id, activity ) subModel data.acuteIllnessActivityPages }
                            , Cmd.map (MsgLoggedIn << MsgPageAcuteIllnessActivity id activity) subCmd
                            , extraMsgs
                            )

                        MsgPageHomeVisitActivity id activity subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.homeVisitActivityPages
                                        |> Dict.get ( id, activity )
                                        |> Maybe.withDefault Pages.HomeVisitActivity.Model.emptyModel
                                        |> Pages.HomeVisitActivity.Update.update currentDate id model.indexedDb subMsg
                            in
                            ( { data | homeVisitActivityPages = Dict.insert ( id, activity ) subModel data.homeVisitActivityPages }
                            , Cmd.map (MsgLoggedIn << MsgPageHomeVisitActivity id activity) subCmd
                            , extraMsgs
                            )

                        MsgPageWellChildActivity id activity subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.wellChildActivityPages
                                        |> Dict.get ( id, activity )
                                        |> Maybe.withDefault Pages.WellChildActivity.Model.emptyModel
                                        |> Pages.WellChildActivity.Update.update currentDate isChw id model.indexedDb subMsg
                            in
                            ( { data | wellChildActivityPages = Dict.insert ( id, activity ) subModel data.wellChildActivityPages }
                            , Cmd.map (MsgLoggedIn << MsgPageWellChildActivity id activity) subCmd
                            , extraMsgs
                            )

                        MsgPagePregnancyOutcome id subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    data.pregnancyOutcomePages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.PregnancyOutcome.Model.emptyModel
                                        |> Pages.PregnancyOutcome.Update.update currentDate id subMsg
                            in
                            ( { data | pregnancyOutcomePages = Dict.insert id subModel data.pregnancyOutcomePages }
                            , Cmd.map (MsgLoggedIn << MsgPagePregnancyOutcome id) subCmd
                            , appMsgs
                            )

                        MsgPageAcuteIllnessProgressReport id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.acuteIllnessProgressReportPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.AcuteIllnessProgressReport.Model.emptyModel
                                        |> Pages.AcuteIllnessProgressReport.Update.update subMsg
                            in
                            ( { data | acuteIllnessProgressReportPages = Dict.insert id subModel data.acuteIllnessProgressReportPages }
                            , Cmd.map (MsgLoggedIn << MsgPageAcuteIllnessProgressReport id) subCmd
                            , extraMsgs
                            )

                        MsgPageNutritionProgressReport id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.nutritionProgressReportPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.NutritionProgressReport.Model.emptyModel
                                        |> Pages.NutritionProgressReport.Update.update subMsg
                            in
                            ( { data | nutritionProgressReportPages = Dict.insert id subModel data.nutritionProgressReportPages }
                            , Cmd.map (MsgLoggedIn << MsgPageNutritionProgressReport id) subCmd
                            , extraMsgs
                            )

                        MsgPageWellChildProgressReport id subMsg ->
                            let
                                ( subModel, subCmd, extraMsgs ) =
                                    data.wellChildProgressReportPages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.WellChildProgressReport.Model.emptyModel
                                        |> Pages.WellChildProgressReport.Update.update subMsg
                            in
                            ( { data | wellChildProgressReportPages = Dict.insert id subModel data.wellChildProgressReportPages }
                            , Cmd.map (MsgLoggedIn << MsgPageWellChildProgressReport id) subCmd
                            , extraMsgs
                            )

                        MsgPageAcuteIllnessOutcome id subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    data.acuteIllnessOutcomePages
                                        |> Dict.get id
                                        |> Maybe.withDefault Pages.AcuteIllnessOutcome.Model.emptyModel
                                        |> Pages.AcuteIllnessOutcome.Update.update currentDate id subMsg
                            in
                            ( { data | acuteIllnessOutcomePages = Dict.insert id subModel data.acuteIllnessOutcomePages }
                            , Cmd.map (MsgLoggedIn << MsgPageAcuteIllnessOutcome id) subCmd
                            , appMsgs
                            )

                        MsgPageTraceContact id subMsg ->
                            let
                                ( subModel, subCmd, appMsgs ) =
                                    Dict.get id data.traceContactPages
                                        |> Maybe.withDefault Pages.TraceContact.Model.emptyModel
                                        |> Pages.TraceContact.Update.update currentDate id subMsg
                            in
                            ( { data | traceContactPages = Dict.insert id subModel data.traceContactPages }
                            , Cmd.map (MsgLoggedIn << MsgPageTraceContact id) subCmd
                            , appMsgs
                            )
                )
                model

        MsgPageDevice subMsg ->
            updateConfigured
                (\configured ->
                    let
                        ( subModel, subCmd, extraMsgs ) =
                            Pages.Device.Update.update subMsg configured.devicePage
                    in
                    ( { configured | devicePage = subModel }
                    , Cmd.map MsgPageDevice subCmd
                    , extraMsgs
                    )
                )
                model

        TryPairingCode code ->
            updateConfigured
                (\configured ->
                    let
                        postCode =
                            HttpBuilder.get (configured.config.backendUrl ++ "/api/pairing-code/" ++ code)
                                |> HttpBuilder.withExpectJson (Json.Decode.field "data" (Device.Decoder.decode configured.config.backendUrl))
                                |> HttpBuilder.toTask

                        cacheDevice device =
                            HttpBuilder.put "/sw/config/device"
                                |> HttpBuilder.withJsonBody (Device.Encoder.encode device)
                                |> HttpBuilder.toTask
                                |> Task.map (always device)

                        cmd =
                            -- Try to get the device's access token from the
                            -- backend, and if it succeeds, cache it on the
                            -- service worker
                            postCode
                                |> Task.andThen cacheDevice
                                |> RemoteData.fromTask
                                |> Task.perform HandlePairedDevice
                    in
                    ( { configured | device = Loading }
                    , cmd
                    , []
                    )
                )
                model

        HandlePairedDevice device ->
            updateConfigured
                (\configured ->
                    ( { configured | device = device }
                    , Cmd.none
                    , []
                    )
                )
                model
                |> sequence update [ MsgSyncManager SyncManager.Model.TrySyncing ]

        MsgPagePinCode subMsg ->
            updateConfigured
                (\configured ->
                    let
                        ( subModel, subCmd, outMsg ) =
                            Pages.PinCode.Update.update subMsg configured.pinCodePage

                        ( extraMsgs, extraCmds ) =
                            outMsg
                                |> Maybe.map
                                    (\out ->
                                        case out of
                                            Pages.PinCode.Model.TryPinCode code ->
                                                ( [ TryPinCode code ], [] )

                                            Pages.PinCode.Model.Logout ->
                                                ( [ SetLoggedIn NotAsked
                                                  , MsgIndexedDb Backend.Model.HandleLogout
                                                  , SetHealthCenter Nothing
                                                  , SetVillage Nothing
                                                  ]
                                                , [ cachePinCode "", cacheHealthCenter "", cacheVillage "" ]
                                                )

                                            Pages.PinCode.Model.SetActivePage page ->
                                                let
                                                    resetDashboardMsg =
                                                        case page of
                                                            -- When accessing Dashboard page, reset
                                                            -- the page to initial state - selected month,
                                                            -- for example will be set to current month.
                                                            UserPage (DashboardPage MainPage) ->
                                                                Pages.Dashboard.Model.Reset model.villageId
                                                                    |> MsgPageDashboard MainPage
                                                                    |> MsgLoggedIn
                                                                    |> List.singleton

                                                            _ ->
                                                                []
                                                in
                                                ( SetActivePage page :: resetDashboardMsg, [] )

                                            Pages.PinCode.Model.SetHealthCenter id ->
                                                ( [ SetHealthCenter (Just id) ], [] )

                                            Pages.PinCode.Model.SetVillage id ->
                                                ( [ SetVillage (Just id) ], [] )
                                    )
                                |> Maybe.withDefault ( [], [] )
                    in
                    ( { configured | pinCodePage = subModel }
                    , Cmd.batch (Cmd.map MsgPagePinCode subCmd :: extraCmds)
                    , extraMsgs
                    )
                )
                model

        MsgServiceWorker subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    ServiceWorker.Update.update model.currentTime subMsg model.serviceWorker
            in
            ( { model | serviceWorker = subModel }
            , Cmd.map MsgServiceWorker subCmd
            )
                |> sequence update extraMsgs

        MsgZScore subMsg ->
            let
                ( subModel, subCmd ) =
                    ZScore.Update.update subMsg model.zscores
            in
            ( { model | zscores = subModel }
            , Cmd.map MsgZScore subCmd
            )

        ScrollToElement elementId ->
            ( model, scrollToElement elementId )

        SetActivePage page ->
            let
                fragment =
                    pageToFragment page

                redirectUrl =
                    model.url
                        |> (\url -> { url | fragment = fragment, query = Nothing })

                cmd =
                    Nav.pushUrl model.navigationKey (Url.toString redirectUrl)

                extraMsgs =
                    case page of
                        -- When navigating to Device page (which is used for Sync management), trigger Sync.
                        DevicePage ->
                            [ MsgSyncManager SyncManager.Model.TrySyncing ]

                        -- When navigating to relationship page in group encounter context,
                        -- we automaticaly select the clinic, to which the session belongs.
                        UserPage (RelationshipPage id1 id2 (GroupEncounterOrigin sessionId)) ->
                            getSession sessionId model.indexedDb
                                |> Maybe.map
                                    (.clinicId
                                        >> fromEntityUuid
                                        >> Pages.Relationship.Model.AssignToClinicId
                                        >> MsgPageRelationship id1 id2
                                        >> MsgLoggedIn
                                        >> List.singleton
                                    )
                                |> Maybe.withDefault []

                        -- When navigating to Acute Illness participant page, set initial view mode.
                        UserPage (AcuteIllnessParticipantPage participantId) ->
                            Pages.AcuteIllnessParticipant.Model.SetViewMode Pages.AcuteIllnessParticipant.Model.ManageIllnesses
                                |> MsgPageAcuteIllnessParticipant participantId
                                |> MsgLoggedIn
                                |> List.singleton

                        _ ->
                            []
            in
            ( { model | activePage = page }
            , cmd
            )
                |> sequence update extraMsgs

        SetLanguage language ->
            ( { model | language = language }
            , setLanguage <| languageToCode language
            )

        SetPersistentStorage data ->
            ( { model | persistentStorage = Just data }
            , Cmd.none
            )

        SetMemoryQuota quota ->
            ( { model | memoryQuota = Just quota }
            , Cmd.none
            )

        SetStorageQuota quota ->
            ( { model | storageQuota = Just quota }
            , Cmd.none
            )

        SetHealthCenter maybeHealthCenterId ->
            ( { model | healthCenterId = maybeHealthCenterId }
            , maybeHealthCenterId
                |> Maybe.map fromEntityUuid
                |> Maybe.withDefault ""
                |> cacheHealthCenter
            )

        SetVillage maybeVillageId ->
            let
                maybeHealthCenterId =
                    maybeVillageId
                        |> Maybe.andThen
                            (\villageId ->
                                RemoteData.toMaybe model.indexedDb.villages
                                    |> Maybe.andThen (Dict.get villageId)
                                    |> Maybe.andThen (.healthCenterId >> Just)
                            )

                extraMsgs =
                    [ SetHealthCenter maybeHealthCenterId ]

                cacheVillageCmd =
                    maybeVillageId
                        |> Maybe.map fromEntityUuid
                        |> Maybe.withDefault ""
                        |> cacheVillage

                ( updatedModel, cmd ) =
                    updateLoggedIn
                        (\loggedId ->
                            ( { loggedId | dashboardPage = Pages.Dashboard.Model.emptyModel maybeVillageId }
                            , cacheVillageCmd
                            , []
                            )
                        )
                        { model | villageId = maybeVillageId }
            in
            ( updatedModel
            , cmd
            )
                |> sequence update extraMsgs

        Tick time ->
            let
                extraMsgs =
                    case model.serviceWorker.lastUpdateCheck of
                        Nothing ->
                            [ MsgServiceWorker <| ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Update ]

                        Just checked ->
                            let
                                diffInMillis =
                                    Time.posixToMillis time - Time.posixToMillis checked

                                diffInMinutes =
                                    diffInMillis // 60000
                            in
                            -- Automatically check for updates every hour
                            if diffInMinutes > 60 then
                                [ MsgServiceWorker <| ServiceWorker.Model.SendOutgoingMsg ServiceWorker.Model.Update ]

                            else
                                []
            in
            ( { model | currentTime = time }
            , Cmd.none
            )
                |> sequence update extraMsgs

        MsgSyncManager subMsg ->
            model.configuration
                |> RemoteData.toMaybe
                |> Maybe.andThen (\config -> RemoteData.toMaybe config.device)
                |> Maybe.map
                    (\device ->
                        updateSubModel
                            subMsg
                            model.syncManager
                            (\subMsg_ subModel -> SyncManager.Update.update currentDate model.currentTime model.activePage model.dbVersion device subMsg_ subModel)
                            (\subModel model_ -> { model_ | syncManager = subModel })
                            (\subCmds -> MsgSyncManager subCmds)
                            model
                    )
                |> Maybe.withDefault noChange

        TryPinCode code ->
            updateConfigured
                (\configured ->
                    let
                        formatResponse =
                            .items >> List.head >> Maybe.map RemoteData.succeed >> Maybe.withDefault (RemoteData.Failure NetworkError)

                        checkPinCode =
                            select "/sw" nurseEndpoint { pinCode = Just code }
                                |> toCmd (RemoteData.fromResult >> RemoteData.andThen formatResponse >> SetLoggedIn)
                    in
                    ( { configured | loggedIn = Loading }
                    , Cmd.batch
                        [ checkPinCode
                        , cachePinCode code
                        ]
                    , []
                    )
                )
                model

        -- Note that this also resets any data which depends on being logged in.
        SetLoggedIn nurse ->
            updateConfigured
                (\configured ->
                    ( { configured | loggedIn = RemoteData.map (emptyLoggedInModel model.villageId) nurse }
                    , Cmd.none
                    , []
                    )
                )
                model

        CheckDataWanted ->
            -- Note that we will be called repeatedly. So, it's vitally important that
            -- the `fetch` implementations use a `WebData`-like strategy to indicate
            -- that a request is in progress, and doesn't need to be triggered again.
            -- Otherwise, we'll keep issuing the same requests, over and over.
            let
                -- These are messages to fetch the data we want now, without
                -- considering whether we already have it.
                dataNowWanted =
                    App.Fetch.fetch model

                -- These are the messages we should actually issue to fetch data now.
                -- As an improvement, we could compare with `model.dataWanted` to see
                -- whether the msg is **newly** desired ... that is, whether its status
                -- just flipped. We could treat newly desired data differently. For
                -- instance, we might try to re-fetch it even in a `Failure` state.
                -- (Since that wouldn't infinitely repeat).
                dataToFetch =
                    List.filter (App.Fetch.shouldFetch model) dataNowWanted

                -- Update our existing dataWanted to indicate that the data now wanted
                -- was last wanted now.
                dataWanted =
                    List.foldl
                        (\msg_ ->
                            let
                                -- Since we may send extra messages as part of fetch editable session
                                -- command, normilise the message as if there're are none,
                                -- so that we can maintain the remember / forget logic.
                                normalizedMsg =
                                    case msg_ of
                                        MsgIndexedDb (Backend.Model.FetchEditableSession id _) ->
                                            MsgIndexedDb (Backend.Model.FetchEditableSession id [])

                                        _ ->
                                            msg_
                            in
                            Dict.insert normalizedMsg model.currentTime
                        )
                        model.dataWanted
                        dataNowWanted

                fiveMinutes =
                    5 * 1000 * 60

                -- Figure out what to remember and what to forget.
                ( dataToForget, dataToRemember ) =
                    Dict.partition (\_ lastWanted -> Time.posixToMillis model.currentTime - Time.posixToMillis lastWanted > fiveMinutes) dataWanted

                -- Our new base model, remembering the desired data, and forgetting
                -- the data to forget.
                newModel =
                    dataToForget
                        |> Dict.keys
                        |> List.foldl App.Fetch.forget
                            { model
                                | dataWanted = dataToRemember
                                , scheduleDataWantedCheck = False
                            }
            in
            sequence update dataToFetch ( newModel, Cmd.none )

        UrlRequested urlRequest ->
            let
                ( modelUpdated, cmd ) =
                    case urlRequest of
                        Browser.Internal url ->
                            ( model, Nav.pushUrl model.navigationKey (Url.toString url) )

                        -- As we use a tag in multiple places in HTML and CSS,
                        -- we'll get `External ""` msg when it's clicked.
                        -- Therefore, we will not react to external Url requests,
                        -- because app does not require it anyway.
                        Browser.External href ->
                            ( model, Cmd.none )
            in
            ( modelUpdated
            , cmd
            )

        UrlChanged url ->
            let
                activePage =
                    activePageByUrl url

                cmd =
                    case activePage of
                        UserPage (SessionPage _ (ChildPage _)) ->
                            App.Ports.bindDropZone ()

                        UserPage (SessionPage _ (ActivityPage (ChildActivity ChildPicture))) ->
                            App.Ports.bindDropZone ()

                        UserPage (CreatePersonPage _ _) ->
                            App.Ports.bindDropZone ()

                        UserPage (EditPersonPage _) ->
                            App.Ports.bindDropZone ()

                        UserPage (PrenatalActivityPage _ PrenatalPhoto) ->
                            App.Ports.bindDropZone ()

                        UserPage (NutritionActivityPage _ Photo) ->
                            App.Ports.bindDropZone ()

                        UserPage (WellChildActivityPage _ WellChildPhoto) ->
                            App.Ports.bindDropZone ()

                        _ ->
                            Cmd.none
            in
            ( { model | url = url, activePage = activePage }
            , cmd
            )


{-| Updates our `nurse` user if the uuid matches the logged-in user.
-}
handleRevision : Model -> Backend.Model.Revision -> Maybe Msg
handleRevision model revision =
    case revision of
        Backend.Model.NurseRevision uuid data ->
            Maybe.andThen
                (\( _, loggedIn ) ->
                    if Tuple.first loggedIn.nurse == uuid then
                        Just (SetLoggedIn (Success ( uuid, data )))

                    else
                        Nothing
                )
                (getLoggedInData model)

        _ ->
            Nothing


{-| Convenience function to process a msg which depends on having a configuration.

The function you supply returns a third parameter, which is a list of additional messages to process.

-}
updateConfigured : (ConfiguredModel -> ( ConfiguredModel, Cmd Msg, List Msg )) -> Model -> ( Model, Cmd Msg )
updateConfigured func model =
    model.configuration
        |> RemoteData.map
            (\configured ->
                let
                    ( subModel, cmd, extraMsgs ) =
                        func configured
                in
                sequence update
                    extraMsgs
                    ( { model | configuration = Success subModel }
                    , cmd
                    )
            )
        |> RemoteData.withDefault ( model, Cmd.none )


{-| Convenience function to process a msg which depends on being logged in.
-}
updateLoggedIn : (LoggedInModel -> ( LoggedInModel, Cmd Msg, List Msg )) -> Model -> ( Model, Cmd Msg )
updateLoggedIn func model =
    updateConfigured
        (\configured ->
            configured.loggedIn
                |> RemoteData.map
                    (\loggedIn ->
                        let
                            ( subModel, cmd, extraMsgs ) =
                                func loggedIn
                        in
                        ( { configured | loggedIn = Success subModel }
                        , cmd
                        , extraMsgs
                        )
                    )
                |> RemoteData.withDefault
                    ( configured, Cmd.none, [] )
        )
        model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        checkDataWanted =
            if model.scheduleDataWantedCheck then
                [ Time.every 50 (always CheckDataWanted)
                ]

            else
                []
    in
    Sub.batch
        ([ Time.every 10000 Tick
         , Sub.map MsgServiceWorker ServiceWorker.Update.subscriptions
         , persistentStorage SetPersistentStorage
         , storageQuota SetStorageQuota
         , memoryQuota SetMemoryQuota
         , Sub.map App.Model.MsgSyncManager (SyncManager.Update.subscriptions model.syncManager)
         ]
            ++ checkDataWanted
        )
