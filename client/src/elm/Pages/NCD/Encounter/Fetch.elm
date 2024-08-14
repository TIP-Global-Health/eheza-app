module Pages.NCD.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Utils exposing (getNCDEncountersForParticipant)
import Maybe.Extra
import RemoteData


fetch : NCDEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.ncdEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            Maybe.map (getNCDEncountersForParticipant db >> List.map Tuple.first) participantId
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurementsMsgs =
            List.map FetchNCDMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchNCDEncountersForParticipant participantId
        , Just <| FetchNCDEncounter id
        ]
        ++ fetchMeasurementsMsgs
