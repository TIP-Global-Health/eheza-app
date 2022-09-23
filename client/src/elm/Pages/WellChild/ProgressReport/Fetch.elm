module Pages.WellChild.ProgressReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.AcuteIllness.Participant.Fetch
import Pages.WellChild.Encounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : WellChildEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.wellChildEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            participantId
                |> Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        fetchAcuteIllnessDataMsgs =
            Maybe.map
                (\personId ->
                    Pages.AcuteIllness.Participant.Fetch.fetch personId db
                )
                maybePersonId
                |> Maybe.withDefault []
    in
    FetchHealthCenters
        :: Pages.WellChild.Encounter.Fetch.fetch id db
        ++ fetchAcuteIllnessDataMsgs
