module Pages.AcuteIllness.Outcome.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : IndividualEncounterParticipantId -> ModelIndexedDb -> List MsgIndexedDb
fetch participantId db =
    let
        personId =
            Dict.get participantId db.individualParticipants
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            Dict.get participantId db.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map Dict.keys
                |> RemoteData.withDefault []

        firstEncounterId =
            List.head encountersIds

        -- We fetch measurements for all encounters, to be
        -- able to apply `expectedAcuteIllnessActivity` logic.
        fetchMeasurements =
            encountersIds
                |> List.map FetchAcuteIllnessMeasurements
    in
    Maybe.Extra.values
        [ Just (FetchIndividualEncounterParticipant participantId)
        , Maybe.map FetchPerson personId
        , Just (FetchAcuteIllnessEncountersForParticipant participantId)
        , Maybe.map FetchAcuteIllnessEncounter firstEncounterId
        ]
        ++ fetchMeasurements
