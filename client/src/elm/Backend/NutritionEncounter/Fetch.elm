module Backend.NutritionEncounter.Fetch exposing (fetchForChild)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetchForChild : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForChild id db =
    [ FetchPerson id

    -- We need this, so we can resolve the nutrition participant for child.
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        ++ fetchForNutrition id db
        ++ fetchForWellChild id db


fetchForNutrition : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForNutrition id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id NutritionEncounter db

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
            List.map FetchNutritionMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchNutritionEncountersForParticipant participantId
        ]
        ++ fetchMeasurements


fetchForWellChild : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForWellChild id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id WellChildEncounter db

        encountersIds =
            participantId
                |> Maybe.map
                    (\participantId_ ->
                        Dict.get participantId_ db.wellChildEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map Dict.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchWellChildMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchWellChildEncountersForParticipant participantId
        ]
        ++ fetchMeasurements
