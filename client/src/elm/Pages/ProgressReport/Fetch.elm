module Pages.ProgressReport.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import EveryDict
import RemoteData exposing (RemoteData(..), WebData)
import Utils.WebData exposing (whenNotAsked)


fetch : ChildId -> ModelIndexedDb -> List MsgIndexedDb
fetch childId db =
    let
        fetchChildMeasurements =
            EveryDict.get childId db.childMeasurements
                |> Maybe.withDefault NotAsked
                |> whenNotAsked (Backend.Model.FetchChildMeasurements childId)

        fetchExpectedSessions =
            EveryDict.get childId db.expectedSessions
                |> Maybe.withDefault NotAsked
                |> whenNotAsked (Backend.Model.FetchExpectedSessions childId)
    in
    List.filterMap identity
        [ fetchChildMeasurements
        , fetchExpectedSessions
        ]
