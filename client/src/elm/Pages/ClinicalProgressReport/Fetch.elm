module Pages.ClinicalProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import RemoteData exposing (RemoteData(..))


fetch : PrenatalEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            EveryDict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            participantId
                |> Maybe.andThen (\id -> EveryDict.get id db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person
    in
    List.filterMap identity
        [ Just <| FetchPrenatalEncounter id
        , Just <| FetchPrenatalMeasurements id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        ]
