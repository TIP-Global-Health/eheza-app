module Pages.HomeVisitEncounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.homeVisitEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            participantId
                |> Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            participantId
                |> Maybe.map
                    (\participantId_ ->
                        Dict.get participantId_ db.homeVisitEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map Dict.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchHomeVisitMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Just <| FetchHomeVisitEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchHomeVisitEncountersForParticipant participantId
        ]
        ++ (Maybe.map
                (\personId ->
                    FetchPerson personId
                        :: Backend.NutritionEncounter.Fetch.fetch personId db
                )
                maybePersonId
                |> Maybe.withDefault []
           )
        ++ fetchMeasurements
