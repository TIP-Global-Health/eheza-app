module Pages.NCD.ProgressReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.AcuteIllness.Participant.Fetch
import Pages.NCD.Encounter.Fetch
import RemoteData


fetch : NCDEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchCmds =
            Dict.get id db.ncdEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.andThen
                    (\encounter ->
                        Dict.get encounter.participant db.individualParticipants
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map
                                (\participant ->
                                    Backend.Model.FetchRelationshipsForPerson participant.person
                                        :: Pages.AcuteIllness.Participant.Fetch.fetch participant.person db
                                )
                    )
                |> Maybe.withDefault []
    in
    Pages.NCD.Encounter.Fetch.fetch id db ++ fetchCmds
