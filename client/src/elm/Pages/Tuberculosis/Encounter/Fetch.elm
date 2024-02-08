module Pages.Tuberculosis.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.NutritionEncounter.Utils exposing (getTuberculosisEncountersForParticipant)
import Maybe.Extra
import RemoteData


fetch : TuberculosisEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.tuberculosisEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            Maybe.map (getTuberculosisEncountersForParticipant db >> List.map Tuple.first) participantId
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurementsMsgs =
            List.map FetchTuberculosisMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchTuberculosisEncountersForParticipant participantId
        , Just <| FetchTuberculosisEncounter id
        ]
        ++ fetchMeasurementsMsgs
