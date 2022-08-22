module Pages.NCD.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : NCDEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.ncdEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            participantId
                |> Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person
    in
    Maybe.Extra.values
        [ Just <| FetchNCDEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        ]



-- @todo
-- ++ (Maybe.map (\personId -> Backend.NCDEncounter.Fetch.fetch personId db) maybePersonId
--         |> Maybe.withDefault []
--    )
