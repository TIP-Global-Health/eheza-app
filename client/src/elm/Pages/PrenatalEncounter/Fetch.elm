module Pages.PrenatalEncounter.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
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

        encountersIds =
            participantId
                |> Maybe.map
                    (\participantId_ ->
                        EveryDict.get participantId_ db.prenatalEncountersByParticipant
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map EveryDictList.keys
                            |> RemoteData.withDefault []
                    )
                |> Maybe.withDefault []

        -- We fetch measurements for  all encounters, to be
        -- able to apply `expectedPrenatalActivity` logic.
        fetchMeasurements =
            encountersIds
                |> List.map FetchPrenatalMeasurements
    in
    List.filterMap identity
        [ Maybe.map FetchPrenatalParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchPrenatalEncountersForParticipant participantId

        -- We need this, so we can resolve the participant from the encounter.
        , Just <| FetchPrenatalEncounter id
        ]
        ++ fetchMeasurements
