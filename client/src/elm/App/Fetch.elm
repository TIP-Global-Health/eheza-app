module App.Fetch exposing (fetch, forget, shouldFetch)

import App.Model exposing (..)
import App.Utils exposing (getLoggedIn, getLoggedInData)
import AssocList as Dict
import Backend.Fetch
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.AcuteIllness.Activity.Fetch
import Pages.AcuteIllness.Activity.Model
import Pages.AcuteIllness.Encounter.Fetch
import Pages.AcuteIllness.Outcome.Fetch
import Pages.AcuteIllness.Participant.Fetch
import Pages.AcuteIllness.ProgressReport.Fetch
import Pages.ChildScoreboard.Activity.Fetch
import Pages.ChildScoreboard.Encounter.Fetch
import Pages.ChildScoreboard.Participant.Fetch
import Pages.ChildScoreboard.ProgressReport.Fetch
import Pages.Clinical.Fetch
import Pages.Clinics.Fetch
import Pages.Dashboard.Fetch
import Pages.Device.Fetch
import Pages.EducationSession.Fetch
import Pages.EducationSession.Model
import Pages.GlobalCaseManagement.Fetch
import Pages.GroupEncounterTypes.Fetch
import Pages.HIV.Activity.Fetch
import Pages.HIV.Encounter.Fetch
import Pages.HIV.Participant.Fetch
import Pages.HomeVisit.Activity.Fetch
import Pages.HomeVisit.Encounter.Fetch
import Pages.IndividualEncounterParticipants.Fetch
import Pages.IndividualEncounterTypes.Fetch
import Pages.MessagingCenter.Fetch
import Pages.NCD.Activity.Fetch
import Pages.NCD.Encounter.Fetch
import Pages.NCD.Participant.Fetch
import Pages.NCD.ProgressReport.Fetch
import Pages.NCD.RecurrentActivity.Fetch
import Pages.NCD.RecurrentEncounter.Fetch
import Pages.Nutrition.Activity.Fetch
import Pages.Nutrition.Encounter.Fetch
import Pages.Nutrition.Participant.Fetch
import Pages.Nutrition.ProgressReport.Fetch
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Fetch
import Pages.People.Fetch
import Pages.Person.Fetch
import Pages.PinCode.Fetch
import Pages.Prenatal.Activity.Fetch
import Pages.Prenatal.DemographicsReport.Fetch
import Pages.Prenatal.Encounter.Fetch
import Pages.Prenatal.Outcome.Fetch
import Pages.Prenatal.Participant.Fetch
import Pages.Prenatal.ProgressReport.Fetch
import Pages.Prenatal.RecurrentActivity.Fetch
import Pages.Prenatal.RecurrentEncounter.Fetch
import Pages.Relationship.Fetch
import Pages.Session.Fetch
import Pages.StockManagement.Fetch
import Pages.TraceContact.Fetch
import Pages.Tuberculosis.Activity.Fetch
import Pages.Tuberculosis.Encounter.Fetch
import Pages.Tuberculosis.Participant.Fetch
import Pages.Tuberculosis.ProgressReport.Fetch
import Pages.WellChild.Activity.Fetch
import Pages.WellChild.Encounter.Fetch
import Pages.WellChild.Participant.Fetch
import Pages.WellChild.ProgressReport.Fetch
import Pages.Wellbeing.Fetch


{-| Basically, we're following down the `view` hierarchy to determine, given
what the `view` methods are going to want, what messages we ought to send
in order to get the data they will need.

For the sake of convenience, this isn't called by our own `update` method --
it would need to be called at the end, after the new model is determined.
Instead, it's integrated up one level, in `Main.elm`, where we hook together
the Elm architecture. (This is really a kind of little extension to the Elm
architecture).

As a future optimization, one could actually integrate this with
`animationFrame`, since you don't need to figure out what to fetch for
views. more often than that.

-}
fetch : Model -> List Msg
fetch model =
    if not model.serviceWorker.active then
        -- Do not fetch anything, until service worker is active.
        []

    else
        let
            currentDate =
                fromLocalDateTime model.currentTime
        in
        case model.activePage of
            DevicePage ->
                List.map MsgIndexedDb Pages.Device.Fetch.fetch

            PinCodePage ->
                getLoggedIn model
                    |> Maybe.map (.nurse >> Tuple.first)
                    |> Pages.PinCode.Fetch.fetch
                    |> List.map MsgIndexedDb

            MessagingCenterPage ->
                getLoggedIn model
                    |> Maybe.map
                        (\loggedIn ->
                            let
                                nurseId =
                                    Tuple.first loggedIn.nurse
                            in
                            Pages.MessagingCenter.Fetch.fetch currentDate nurseId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            WellbeingPage ->
                getLoggedIn model
                    |> Maybe.map
                        (\loggedIn ->
                            let
                                nurseId =
                                    Tuple.first loggedIn.nurse
                            in
                            Pages.Wellbeing.Fetch.fetch currentDate nurseId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            MessagingGuide ->
                []

            PageNotFound _ ->
                []

            ServiceWorkerPage ->
                []

            UserPage MyAccountPage ->
                []

            UserPage ClinicalPage ->
                Pages.Clinical.Fetch.fetch model.villageId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage ClinicsPage ->
                getLoggedInData model
                    |> Maybe.map
                        (\( healthCenterId, loggedIn ) ->
                            Pages.Clinics.Fetch.fetch healthCenterId model.indexedDb model.syncManager loggedIn.clinicsPage
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (DashboardPage _) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( healthCenterId, loggedIn ) ->
                            Pages.Dashboard.Fetch.fetch currentDate healthCenterId model.indexedDb loggedIn.dashboardPage
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage GlobalCaseManagementPage ->
                Maybe.map
                    (\( healthCenterId, _ ) ->
                        Pages.GlobalCaseManagement.Fetch.fetch currentDate healthCenterId model.villageId model.indexedDb
                            |> List.map MsgIndexedDb
                    )
                    (getLoggedInData model)
                    |> Maybe.withDefault []

            UserPage (ClinicalProgressReportPage _ prenatalEncounterId) ->
                Pages.Prenatal.ProgressReport.Fetch.fetch prenatalEncounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (CreatePersonPage relatedId _) ->
                Pages.Person.Fetch.fetchForCreateOrEdit relatedId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (EditPersonPage relatedId) ->
                Pages.Person.Fetch.fetchForCreateOrEdit (Just relatedId) model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (DemographicsReportPage _ personId) ->
                Pages.Prenatal.DemographicsReport.Fetch.fetch personId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PersonPage id initiator) ->
                Pages.Person.Fetch.fetch id initiator model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PersonsPage relation initiator) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            Pages.People.Fetch.fetch relation initiator loggedIn.personsPage
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (PrenatalParticipantPage _ personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.Prenatal.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (NutritionParticipantPage _ personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.Nutrition.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (AcuteIllnessParticipantPage _ personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.AcuteIllness.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (WellChildParticipantPage _ personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.WellChild.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (NCDParticipantPage _ personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.NCD.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (ChildScoreboardParticipantPage personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.ChildScoreboard.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (TuberculosisParticipantPage personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.Tuberculosis.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (HIVParticipantPage personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\_ ->
                            Pages.HIV.Participant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (IndividualEncounterParticipantsPage encounterType) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            Pages.IndividualEncounterParticipants.Fetch.fetch encounterType loggedIn.individualEncounterParticipantsPage
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (RelationshipPage id1 id2 _) ->
                Pages.Relationship.Fetch.fetch id1 id2 model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (SessionPage sessionId sessionPage) ->
                let
                    features =
                        model.syncManager.syncInfoGeneral.features
                in
                Pages.Session.Fetch.fetch currentDate
                    model.zscores
                    features
                    sessionId
                    sessionPage
                    model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalEncounterPage id) ->
                Pages.Prenatal.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalActivityPage encounterId _) ->
                Pages.Prenatal.Activity.Fetch.fetch encounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalRecurrentEncounterPage id) ->
                Pages.Prenatal.RecurrentEncounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalRecurrentActivityPage encounterId _) ->
                Pages.Prenatal.RecurrentActivity.Fetch.fetch encounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalLabsHistoryPage _ labEncounterId _) ->
                Pages.Prenatal.RecurrentActivity.Fetch.fetch labEncounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage IndividualEncounterTypesPage ->
                Pages.IndividualEncounterTypes.Fetch.fetch
                    |> List.map MsgIndexedDb

            UserPage GroupEncounterTypesPage ->
                Pages.GroupEncounterTypes.Fetch.fetch
                    |> List.map MsgIndexedDb

            UserPage (PregnancyOutcomePage _ id) ->
                Pages.Prenatal.Outcome.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NutritionEncounterPage id) ->
                Pages.Nutrition.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NutritionActivityPage id _) ->
                Pages.Nutrition.Activity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessEncounterPage id) ->
                Pages.AcuteIllness.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessActivityPage id activity) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            let
                                activityPage =
                                    Dict.get ( id, activity ) loggedIn.acuteIllnessActivityPages
                                        |> Maybe.withDefault Pages.AcuteIllness.Activity.Model.emptyModel
                            in
                            Pages.AcuteIllness.Activity.Fetch.fetch id activity model.indexedDb activityPage
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (HomeVisitEncounterPage id) ->
                Pages.HomeVisit.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (HomeVisitActivityPage id _) ->
                Pages.HomeVisit.Activity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (WellChildEncounterPage id) ->
                Pages.WellChild.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (WellChildActivityPage id _) ->
                Pages.WellChild.Activity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NCDEncounterPage id) ->
                Pages.NCD.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NCDActivityPage id _) ->
                Pages.NCD.Activity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NCDRecurrentEncounterPage id) ->
                Pages.NCD.RecurrentEncounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NCDRecurrentActivityPage encounterId _) ->
                Pages.NCD.RecurrentActivity.Fetch.fetch encounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (ChildScoreboardEncounterPage id) ->
                Pages.ChildScoreboard.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (ChildScoreboardActivityPage encounterId _) ->
                Pages.ChildScoreboard.Activity.Fetch.fetch encounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (TuberculosisEncounterPage id) ->
                Pages.Tuberculosis.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (TuberculosisActivityPage id _) ->
                Pages.Tuberculosis.Activity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (EducationSessionPage id) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            let
                                page_ =
                                    Dict.get id loggedIn.educationSessionPages
                                        |> Maybe.withDefault Pages.EducationSession.Model.emptyModel
                            in
                            Pages.EducationSession.Fetch.fetch id model.villageId model.indexedDb page_
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (HIVEncounterPage id) ->
                Pages.HIV.Encounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (HIVActivityPage id activity) ->
                Pages.HIV.Activity.Fetch.fetch id activity model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NutritionProgressReportPage id) ->
                Pages.Nutrition.ProgressReport.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessProgressReportPage _ id) ->
                Pages.AcuteIllness.ProgressReport.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (WellChildProgressReportPage id) ->
                Pages.WellChild.ProgressReport.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (TuberculosisProgressReportPage id) ->
                Pages.Tuberculosis.ProgressReport.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NCDProgressReportPage initiator) ->
                let
                    encounterId =
                        case initiator of
                            InitiatorEncounterPage id ->
                                id

                            InitiatorRecurrentEncounterPage id ->
                                id
                in
                Pages.NCD.ProgressReport.Fetch.fetch encounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (ChildScoreboardProgressReportPage id) ->
                Pages.ChildScoreboard.ProgressReport.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessOutcomePage id) ->
                Pages.AcuteIllness.Outcome.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (TraceContactPage id) ->
                Pages.TraceContact.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PatientRecordPage _ id) ->
                Pages.PatientRecord.Fetch.fetch currentDate id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage StockManagementPage ->
                getLoggedInData model
                    |> Maybe.map
                        (\( healthCenterId, _ ) ->
                            Pages.StockManagement.Fetch.fetch currentDate healthCenterId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []


{-| Given a `Msg`, do we need to fetch the data it would fetch? We only answer
`True` if the data is `NotAsked`. So, we don't automatically re-fetch errors.

Note that the data need not literally be a `RemoteData`, but that will be
common. The answer does need to flip to `False` when a request is in progress,
or we will enter an infinite loop.

-}
shouldFetch : Model -> Msg -> Bool
shouldFetch model msg =
    case msg of
        MsgIndexedDb subMsg ->
            Backend.Fetch.shouldFetch model.currentTime model.indexedDb subMsg

        _ ->
            False


{-| Given a Msg that would fetch some data, forget that data.
-}
forget : Msg -> Model -> Model
forget msg model =
    case msg of
        MsgIndexedDb subMsg ->
            let
                subModel =
                    Backend.Fetch.forget subMsg model.indexedDb
            in
            { model | indexedDb = subModel }

        _ ->
            model
