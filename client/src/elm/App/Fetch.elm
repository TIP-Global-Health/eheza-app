module App.Fetch exposing (fetch, forget, shouldFetch)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInData)
import Backend.Fetch
import Date
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.Clinical.Fetch
import Pages.ClinicalProgressReport.Fetch
import Pages.Clinics.Fetch
import Pages.DemographicsReport.Fetch
import Pages.Device.Fetch
import Pages.IndividualEncounterParticipants.Fetch
import Pages.IndividualEncounterTypes.Fetch
import Pages.NutritionActivity.Fetch
import Pages.NutritionEncounter.Fetch
import Pages.NutritionParticipant.Fetch
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
            Pages.Clinical.Fetch.fetch
                |> List.map MsgIndexedDb

        UserPage (ClinicsPage clinicId) ->
            Pages.Clinics.Fetch.fetch clinicId
                |> List.map MsgIndexedDb

        UserPage (ClinicalProgressReportPage prenatalEncounterId) ->
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

        UserPage (PersonPage id) ->
            Pages.Person.Fetch.fetch id model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (PersonsPage relation) ->
            getLoggedInData model
                |> Maybe.map
                    (\( _, loggedIn ) ->
                        Pages.People.Fetch.fetch relation loggedIn.personsPage
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

        UserPage (IndividualEncounterParticipantsPage encounterType) ->
            getLoggedInData model
                |> Maybe.map
                    (\( _, loggedIn ) ->
                        Pages.IndividualEncounterParticipants.Fetch.fetch encounterType loggedIn.individualEncounterParticipantsPage
                            |> List.map MsgIndexedDb
                    )
                |> Maybe.withDefault []

        UserPage (RelationshipPage id1 id2) ->
            Pages.Relationship.Fetch.fetch id1 id2 model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (SessionPage sessionId sessionPage) ->
            Pages.Session.Fetch.fetch sessionId sessionPage model.indexedDb
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

        UserPage (PregnancyOutcomePage id) ->
            Pages.PregnancyOutcome.Fetch.fetch id model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (NutritionEncounterPage id) ->
            Pages.NutritionEncounter.Fetch.fetch id model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (NutritionActivityPage nutritionEncounterId _) ->
            Pages.NutritionActivity.Fetch.fetch nutritionEncounterId model.indexedDb
                |> List.map MsgIndexedDb



-- Pages.NutritionEncounter.Fetch.fetch id model.indexedDb
--     |> List.map MsgIndexedDb


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
