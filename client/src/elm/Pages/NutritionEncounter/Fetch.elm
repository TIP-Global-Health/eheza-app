module Pages.NutritionEncounter.Fetch exposing (fetch, fetchForChild)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.nutritionEncounters
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
                        Dict.get participantId_ db.nutritionEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map Dict.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            encountersIds
                |> List.map FetchNutritionMeasurements
    in
    List.filterMap identity
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchNutritionEncountersForParticipant participantId

        -- We need this, so we can resolve the participant from the encounter.
        , Just <| FetchNutritionEncounter id
        ]
        ++ fetchMeasurements


fetchForChild : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForChild id db =
    let
        participantId =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.andThen
                    (Dict.toList
                        >> List.filter
                            (\( _, participant ) ->
                                participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                            )
                        >> List.head
                        >> Maybe.map Tuple.first
                    )

        encountersIds =
            participantId
                |> Maybe.map
                    (\participantId_ ->
                        Dict.get participantId_ db.nutritionEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map Dict.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            encountersIds
                |> List.map FetchNutritionMeasurements
    in
    List.filterMap identity
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchNutritionEncountersForParticipant participantId
        , Just <| FetchPerson id

        -- We need this, so we can resolve the nutrition participant for child.
        , Just <| FetchIndividualEncounterParticipantsForPerson id
        ]
        ++ fetchMeasurements
