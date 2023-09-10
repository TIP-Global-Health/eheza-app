module Backend.NutritionEncounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Utils exposing (getWellChildEncountersForParticipant)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Utils exposing (resolveIndividualParticipantForPerson, resolveIndividualParticipantsForPerson)
import EverySet
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchParentsMsgs =
            Dict.get id db.relationshipsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.values
                        >> List.filter (.relatedBy >> (==) MyParent)
                        >> EverySet.fromList
                        >> EverySet.toList
                        >> List.map (.relatedTo >> FetchPerson)
                    )
                |> Maybe.withDefault []
    in
    [ FetchPerson id
    , FetchRelationshipsForPerson id

    -- We need this, so we can resolve the individual participants of child.
    , FetchIndividualEncounterParticipantsForPerson id

    -- Fetch Group measuments that belong to child.
    , Backend.Model.FetchChildMeasurements id
    ]
        ++ fetchForNutrition id db
        ++ fetchForWellChild id db
        ++ fetchForNCDAScoreboard id db
        ++ fetchParentsMsgs


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

        -- We fetch all encounters.
        fetchEncounters =
            List.map FetchNutritionEncounter encountersIds

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchNutritionMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchNutritionEncountersForParticipant participantId
        ]
        ++ fetchEncounters
        ++ fetchMeasurements


fetchForWellChild : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForWellChild id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id WellChildEncounter db

        encountersIds =
            Maybe.map (getWellChildEncountersForParticipant db) participantId
                |> Maybe.withDefault []
                |> List.map Tuple.first

        -- We fetch all encounters.
        fetchEncounters =
            List.map FetchWellChildEncounter encountersIds

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchWellChildMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchWellChildEncountersForParticipant participantId
        ]
        ++ fetchEncounters
        ++ fetchMeasurements


fetchForNCDAScoreboard : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForNCDAScoreboard id db =
    resolveIndividualParticipantsForPerson id AcuteIllnessEncounter db
        |> List.concatMap
            (\participantId ->
                let
                    fetchMeasurementsMsgs =
                        Dict.get participantId db.acuteIllnessEncountersByParticipant
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map
                                (Dict.toList
                                    >> List.filterMap
                                        (\( encounterId, encounter ) ->
                                            -- We need to fetch measurements of encounters where Uncomplicated
                                            -- Gastrointestinal Infection was diagnosed, to check if treatment was given.
                                            if encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated then
                                                Just <| FetchAcuteIllnessMeasurements encounterId

                                            else
                                                Nothing
                                        )
                                )
                            |> Maybe.withDefault []
                in
                FetchAcuteIllnessEncountersForParticipant participantId :: fetchMeasurementsMsgs
            )
