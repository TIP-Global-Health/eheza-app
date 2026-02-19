module Backend.FamilyNutritionEncounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.FamilyNutritionEncounter.Utils
    exposing
        ( getAcuteIllnessEncountersForParticipant
        , getChildScoreboardEncountersForParticipant
        , getFamilyNutritionEncountersForParticipant
        , getWellChildEncountersForParticipant
        )
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Utils exposing (resolveIndividualParticipantForPerson, resolveIndividualParticipantsForPerson)
import EverySet
import Maybe.Extra
import RemoteData


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchChildrenMsgs =
            Dict.get id db.relationshipsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.values
                        >> List.filter (.relatedBy >> (==) MyChild)
                        >> EverySet.fromList
                        >> EverySet.toList
                        >> List.map (.relatedTo >> FetchPerson)
                    )
                |> Maybe.withDefault []
    in
    [ FetchPerson id
    , FetchRelationshipsForPerson id

    -- We need this, so we can resolve the family participants of child.
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        ++ fetchChildrenMsgs
        ++ fetchForFamilyNutrition id db


fetchForFamilyNutrition : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetchForFamilyNutrition id db =
    let
        participantId =
            resolveIndividualParticipantForPerson id FamilyNutritionEncounter db

        encountersIds =
            Maybe.map (getFamilyNutritionEncountersForParticipant db) participantId
                |> Maybe.withDefault []
                |> List.map Tuple.first

        -- We fetch all encounters.
        fetchEncounters =
            List.map FetchFamilyNutritionEncounter encountersIds

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchFamilyNutritionMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchFamilyEncounterParticipant participantId
        , Maybe.map FetchFamilyNutritionEncountersForParticipant participantId
        ]
        ++ fetchEncounters
        ++ fetchMeasurements
