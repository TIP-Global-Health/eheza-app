module Pages.NCD.ProgressReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.NCD.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : NCDEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        encounter =
            Dict.get id db.ncdEncounters
                |> Maybe.withDefault NotAsked

        fetchCmds =
            RemoteData.andThen
                (\encounter_ ->
                    Dict.get encounter_.participant db.individualParticipants
                        |> Maybe.withDefault NotAsked
                )
                encounter
                |> RemoteData.map
                    (\participant ->
                        [ Backend.Model.FetchRelationshipsForPerson participant.person
                        ]
                    )
                |> RemoteData.withDefault []
    in
    Pages.NCD.Encounter.Fetch.fetch id db ++ fetchCmds
