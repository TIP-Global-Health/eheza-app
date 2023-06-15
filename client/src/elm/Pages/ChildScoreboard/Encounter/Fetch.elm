module Pages.ChildScoreboard.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : ChildScoreboardEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.childScoreboardEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            Maybe.map
                (\participantId_ ->
                    Dict.get participantId_ db.childScoreboardEncountersByParticipant
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map Dict.keys
                        |> RemoteData.withDefault []
                )
                participantId
                |> Maybe.withDefault []

        -- We fetch measurements of all encounters.
        fetchMeasurementsMsgs =
            -- @todo
            -- List.map FetchChildScoreboardMeasurements encountersIds
            []
    in
    Maybe.Extra.values
        [ Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPerson personId
        , Maybe.map FetchChildScoreboardEncountersForParticipant participantId
        , Just <| FetchChildScoreboardEncounter id
        ]
        ++ fetchMeasurementsMsgs
