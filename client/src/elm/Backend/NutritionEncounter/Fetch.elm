module Backend.NutritionEncounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Utils
    exposing
        ( getAcuteIllnessEncountersForParticipant
        , getChildScoreboardEncountersForParticipant
        , getNutritionEncountersForParticipant
        , getWellChildEncountersForParticipant
        )
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Utils exposing (resolveIndividualParticipantForPerson, resolveIndividualParticipantsForPerson)
import EverySet
import Maybe.Extra
import RemoteData


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
    , Backend.Model.FetchExpectedSessions id
    ]
        ++ fetchForNutrition id db
        ++ fetchForWellChild id db
        ++ fetchForNCDAScoreboard id db
        ++ fetchForChildScoreboard id db
        ++ fetchParentsMsgs


fetchForNutrition : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForNutrition id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id NutritionEncounter db

        encountersIds =
            Maybe.map (getNutritionEncountersForParticipant db) participantId
                |> Maybe.withDefault []
                |> List.map Tuple.first

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


fetchForChildScoreboard : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForChildScoreboard id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id ChildScoreboardEncounter db

        encountersIds =
            Maybe.map (getChildScoreboardEncountersForParticipant db) participantId
                |> Maybe.withDefault []
                |> List.map Tuple.first

        -- We fetch all encounters.
        fetchEncounters =
            List.map FetchChildScoreboardEncounter encountersIds

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchChildScoreboardMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchChildScoreboardEncountersForParticipant participantId
        ]
        ++ fetchEncounters
        ++ fetchMeasurements


fetchForNCDAScoreboard : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForNCDAScoreboard id db =
    let
        fetchPrenatalDataMsgs =
            let
                -- Trying to resolve the pregnancy tracked on E-Heza, at which
                -- the child was registered on E-Heza (during CHW Postpartum
                -- encounter).
                maybePregnancyId =
                    Dict.get id db.pregnancyByNewborn
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.Extra.join
                        |> Maybe.map Tuple.first
            in
            FetchPregnancyByNewborn id
                :: Maybe.Extra.values
                    [ -- Loading data for pregnancy participant and encounters.
                      Maybe.map FetchIndividualEncounterParticipant maybePregnancyId
                    , Maybe.map FetchPrenatalEncountersForParticipant maybePregnancyId
                    ]

        fetchAcuteIllnessDataMsgs =
            resolveIndividualParticipantsForPerson id AcuteIllnessEncounter db
                |> List.concatMap
                    (\participantId ->
                        let
                            fetchMeasurementsMsgs =
                                getAcuteIllnessEncountersForParticipant db participantId
                                    |> List.filterMap
                                        (\( encounterId, encounter ) ->
                                            -- We need to fetch measurements of encounters where Uncomplicated
                                            -- Gastrointestinal Infection was diagnosed, to check if treatment was given.
                                            if encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated then
                                                Just <| FetchAcuteIllnessMeasurements encounterId

                                            else
                                                Nothing
                                        )
                        in
                        FetchAcuteIllnessEncountersForParticipant participantId :: fetchMeasurementsMsgs
                    )
    in
    fetchAcuteIllnessDataMsgs ++ fetchPrenatalDataMsgs
