module Pages.HomeVisitEncounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import RemoteData exposing (RemoteData(..))


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.homeVisitEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
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
            encountersIds
                |> List.map FetchHomeVisitMeasurements

        -- We pull data for all Nutrition encounters, to be able to
        -- determine latest weight measurement that was taken for child.
        nutritionParticipantId =
            personId
                |> Maybe.andThen
                    (\personId_ ->
                        resolveIndividualParticipantForPerson personId_ NutritionEncounter db
                    )

        nutritionEncountersIds =
            nutritionParticipantId
                |> Maybe.map
                    (\participantId_ ->
                        Dict.get participantId_ db.nutritionEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map Dict.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements of all Nutrition encounters.
        fetchNutritionMeasurements =
            nutritionEncountersIds
                |> List.map FetchNutritionMeasurements
    in
    List.filterMap identity
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchHomeVisitEncountersForParticipant participantId
        , Maybe.map FetchNutritionEncountersForParticipant nutritionParticipantId

        -- We need this, so we can resolve the participant from the encounter.
        , Just <| FetchHomeVisitEncounter id
        ]
        ++ fetchMeasurements
        ++ fetchNutritionMeasurements
