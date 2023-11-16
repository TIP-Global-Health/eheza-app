module Pages.AcuteIllness.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Utils exposing (getAcuteIllnessEncountersForParticipant)
import Maybe.Extra
import RemoteData


fetch : AcuteIllnessEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.acuteIllnessEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            Maybe.map (getAcuteIllnessEncountersForParticipant db) participantId
                |> Maybe.withDefault []
                |> List.map Tuple.first

        -- We fetch measurements for all encounters, to be
        -- able to apply `expectedAcuteIllnessActivity` logic.
        fetchMeasurements =
            List.map FetchAcuteIllnessMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchAcuteIllnessEncountersForParticipant participantId

        -- We need this, so we can resolve the participant from the encounter.
        , Just <| FetchAcuteIllnessEncounter id
        ]
        ++ fetchMeasurements
