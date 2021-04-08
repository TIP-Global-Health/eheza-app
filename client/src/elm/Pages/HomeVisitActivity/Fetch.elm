module Pages.HomeVisitActivity.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.HomeVisitEncounter.Fetch
import RemoteData exposing (RemoteData(..))


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.homeVisitEncounters
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .participant

        personId =
            participantId
                |> Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person
    in
    Pages.HomeVisitEncounter.Fetch.fetch id db
        ++ -- We pull data of all Group measuments, to be able to
           -- determine latest weight measurement that was taken for child.
           List.filterMap identity
            [ Maybe.map Backend.Model.FetchChildMeasurements personId ]
