module App.Fetch exposing (fetch, forget, shouldFetch)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInData)
import Backend.Fetch
import Date
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.AcuteIllnessActivity.Fetch
import Pages.AcuteIllnessEncounter.Fetch
import Pages.AcuteIllnessOutcome.Fetch
import Pages.AcuteIllnessParticipant.Fetch
import Pages.AcuteIllnessProgressReport.Fetch
import Pages.Clinical.Fetch
import Pages.ClinicalProgressReport.Fetch
import Pages.Clinics.Fetch
import Pages.Dashboard.Fetch
import Pages.DemographicsReport.Fetch
import Pages.Device.Fetch
import Pages.GlobalCaseManagement.Fetch
import Pages.HomeVisitActivity.Fetch
import Pages.HomeVisitEncounter.Fetch
import Pages.IndividualEncounterParticipants.Fetch
import Pages.IndividualEncounterTypes.Fetch
import Pages.NutritionActivity.Fetch
import Pages.NutritionEncounter.Fetch
import Pages.NutritionParticipant.Fetch
import Pages.NutritionProgressReport.Fetch
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.People.Fetch
import Pages.Person.Fetch
import Pages.PinCode.Fetch
import Pages.PregnancyOutcome.Fetch
import Pages.PrenatalActivity.Fetch
import Pages.PrenatalEncounter.Fetch
import Pages.PrenatalParticipant.Fetch
import Pages.Relationship.Fetch
import Pages.Session.Fetch
import Time


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
    let
        currentDate =
            fromLocalDateTime model.currentTime
    in
    if not model.serviceWorker.active then
        -- Do not fetch anything, until service worker is active.
        []

    else
        case model.activePage of
            DevicePage ->
                List.map MsgIndexedDb Pages.Device.Fetch.fetch

            PinCodePage ->
                List.map MsgIndexedDb Pages.PinCode.Fetch.fetch

            PageNotFound _ ->
                []

            ServiceWorkerPage ->
                []

            UserPage MyAccountPage ->
                []

            UserPage ClinicalPage ->
                Pages.Clinical.Fetch.fetch model.villageId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (ClinicsPage clinicId) ->
                Pages.Clinics.Fetch.fetch clinicId
                    |> List.map MsgIndexedDb

            UserPage (DashboardPage subPage) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( healthCenterId, loggedIn ) ->
                            Pages.Dashboard.Fetch.fetch currentDate healthCenterId model.indexedDb loggedIn.dashboardPage
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage GlobalCaseManagementPage ->
                Maybe.map2
                    (\( healthCenterId, loggedIn ) villageId ->
                        Pages.GlobalCaseManagement.Fetch.fetch currentDate healthCenterId villageId model.indexedDb
                            |> List.map MsgIndexedDb
                    )
                    (getLoggedInData model)
                    model.villageId
                    |> Maybe.withDefault []

            UserPage (ClinicalProgressReportPage _ prenatalEncounterId) ->
                Pages.ClinicalProgressReport.Fetch.fetch prenatalEncounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (CreatePersonPage relatedId _) ->
                Pages.Person.Fetch.fetchForCreateOrEdit relatedId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (EditPersonPage relatedId) ->
                Pages.Person.Fetch.fetchForCreateOrEdit (Just relatedId) model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (DemographicsReportPage prenatalEncounterId) ->
                Pages.DemographicsReport.Fetch.fetch prenatalEncounterId model.indexedDb
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

            UserPage (PrenatalParticipantPage personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            Pages.PrenatalParticipant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (NutritionParticipantPage personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            Pages.NutritionParticipant.Fetch.fetch personId model.indexedDb
                                |> List.map MsgIndexedDb
                        )
                    |> Maybe.withDefault []

            UserPage (AcuteIllnessParticipantPage personId) ->
                getLoggedInData model
                    |> Maybe.map
                        (\( _, loggedIn ) ->
                            Pages.AcuteIllnessParticipant.Fetch.fetch personId model.indexedDb
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
                Pages.Session.Fetch.fetch currentDate model.zscores sessionId sessionPage model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalEncounterPage id) ->
                Pages.PrenatalEncounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (PrenatalActivityPage prenatalEncounterId _) ->
                Pages.PrenatalActivity.Fetch.fetch prenatalEncounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage IndividualEncounterTypesPage ->
                Pages.IndividualEncounterTypes.Fetch.fetch
                    |> List.map MsgIndexedDb

            UserPage (PregnancyOutcomePage _ id) ->
                Pages.PregnancyOutcome.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NutritionEncounterPage id) ->
                Pages.NutritionEncounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NutritionActivityPage id _) ->
                Pages.NutritionActivity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessEncounterPage id) ->
                Pages.AcuteIllnessEncounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessActivityPage id _) ->
                Pages.AcuteIllnessActivity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (HomeVisitEncounterPage id) ->
                Pages.HomeVisitEncounter.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (HomeVisitActivityPage id _) ->
                Pages.HomeVisitActivity.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (NutritionProgressReportPage nutritionEncounterId) ->
                Pages.NutritionProgressReport.Fetch.fetch nutritionEncounterId model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessProgressReportPage id) ->
                Pages.AcuteIllnessProgressReport.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb

            UserPage (AcuteIllnessOutcomePage id) ->
                Pages.AcuteIllnessOutcome.Fetch.fetch id model.indexedDb
                    |> List.map MsgIndexedDb


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
            Backend.Fetch.shouldFetch model.indexedDb subMsg

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
