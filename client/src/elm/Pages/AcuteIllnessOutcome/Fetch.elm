module Pages.AcuteIllnessOutcome.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
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
    List.filterMap identity
        [ Maybe.map FetchIndividualEncounterParticipant (Just participantId)
        , Maybe.map FetchPerson personId
        , Maybe.map FetchAcuteIllnessEncountersForParticipant (Just participantId)
        , Maybe.map FetchAcuteIllnessEncounter firstEncounterId
        ]
        ++ fetchMeasurements
