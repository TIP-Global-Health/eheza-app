module Pages.Session.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import Pages.Page exposing (SessionPage(..))
import Pages.ProgressReport.Fetch
import RemoteData exposing (RemoteData(..))


fetch : SessionId -> SessionPage -> ModelIndexedDb -> List MsgIndexedDb
fetch sessionId sessionPage db =
    let
        forSessionPage =
            case sessionPage of
                ProgressReportPage childId ->
                    Pages.ProgressReport.Fetch.fetch childId

                _ ->
                    []

        fetchMeasurements =
            EveryDict.get sessionId db.expectedParticipants
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (\participants ->
                        List.concat
                            [ EveryDictList.keys participants.children
                                |> List.map FetchChildMeasurements
                            , EveryDictList.keys participants.mothers
                                |> List.map FetchMotherMeasurements
                            ]
                    )
                |> RemoteData.withDefault []
    in
    [ FetchSession sessionId
    , FetchClinics
    , FetchEveryCounselingSchedule
    , FetchParticipantForms
    , FetchExpectedParticipants sessionId
    ]
        |> List.append forSessionPage
        |> List.append fetchMeasurements
