module Pages.PrenatalEncounter.Fetch exposing (fetch)

import AllDict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


fetch : PrenatalEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            AllDict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            participantId
                |> Maybe.andThen (\id -> AllDict.get id db.prenatalParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person
    in
    List.filterMap identity
        [ Just <| FetchPrenatalEncounter id
        , Maybe.map FetchPrenatalParticipant participantId
        , Maybe.map FetchPerson personId
        ]
